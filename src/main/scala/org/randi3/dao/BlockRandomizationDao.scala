package org.randi3.dao


import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.extended.ExtendedProfile

import org.randi3.randomization.{VariableBlockRandomization, AbstractBlockRandomization, BlockRandomization}
import scala.collection.mutable.ListBuffer
import scalaz._
import org.randi3.schema.{DatabaseSchema, BlockRandomizationSchema}

class BlockRandomizationDao(database: Database, driver: ExtendedProfile) extends AbstractRandomizationMethodDao(database, driver) {

  import driver.Implicit._

  val schemaCore = new DatabaseSchema(driver)
  import schemaCore._
  val schemaBlock = new BlockRandomizationSchema(driver)
  import schemaBlock._

  private val queryBlockRandomizationFromId = for {
    id <- Parameters[Int]
    blockRandomization <- BlockRandomizations if blockRandomization.randomizationMethodId is id
  } yield blockRandomization.id ~ blockRandomization.version ~ blockRandomization.randomizationMethodId ~ blockRandomization.blocksize ~ blockRandomization.minBlockSize ~ blockRandomization.maxBlockSize

  private val queryBlocksFromId = for {
    id <- Parameters[Int]
    blocks <- Blocks if blocks.randomizationMethodId is id
  } yield blocks.id ~ blocks.treatmentArmId ~ blocks.stratum

  def create(randomizationMethod: AbstractBlockRandomization, trialId: Int): Validation[String, Int] = {
    database withSession {
      val identifier =
        threadLocalSession withTransaction {
          val seed = randomizationMethod.random.nextLong()
          randomizationMethod.random.setSeed(seed)
          RandomizationMethods.noId insert(trialId, generateBlob(randomizationMethod.random).get, randomizationMethod.getClass().getName(), seed)
          val id = getId(trialId).either match {
            case Left(x) => return Failure(x)
            case Right(id1) => id1
          }
          if (randomizationMethod.isInstanceOf[BlockRandomization]) {
            BlockRandomizations.noId insert(0, Some(id), Some(randomizationMethod.asInstanceOf[BlockRandomization].blocksize), None, None)
          } else if (randomizationMethod.isInstanceOf[VariableBlockRandomization]) {
            val method = randomizationMethod.asInstanceOf[VariableBlockRandomization]
            BlockRandomizations.noId insert(0, Some(id), None, Some(method.minBlockSize), Some(method.maxBlockSize))
          }

          id
        }
      Success(identifier)
    }
  }

  def get(id: Int): Validation[String, Option[AbstractBlockRandomization]] = {
    database withSession {
      val resultList = queryRandomizationMethodFromId(id).list
      if (resultList.isEmpty) Success(None)
      else if (resultList.size == 1) {
        val rm = resultList(0)
        if (rm._3 == classOf[BlockRandomization].getName) {
          val blockSize = getBlockSize(id).either match {
            case Left(x) => return Failure(x)
            case Right(blocksize) => blocksize
          }
          val blockRandomization = new BlockRandomization(rm._1.get, 0, blockSize)(deserializeRandomGenerator(rm._2))
          getBlocks(blockRandomization)
          return Success(Some(blockRandomization))
        } else if (rm._3 == classOf[VariableBlockRandomization].getName) {
          val resultListBlock = queryBlockRandomizationFromId(rm._1.get).list
          if (resultListBlock.isEmpty) return Failure("Plugin entry not found")
          else if (resultListBlock.size > 1) return Failure("Database failure more than one entry")
          else {
            val minBlockSize = resultListBlock.head._5.getOrElse(return Failure("Minimal block size not found"))
            val maxBlockSize = resultListBlock.head._6.getOrElse(return Failure("Maximal block size not found"))

            val blockRandomization = new VariableBlockRandomization(rm._1.get, resultListBlock.head._2, minBlockSize, maxBlockSize)(deserializeRandomGenerator(rm._2))
            getBlocks(blockRandomization)
            return Success(Some(blockRandomization))
          }

        } else {
          Failure("Wrong plugin")
        }
      } else Failure("Duplicated database entry")
    }
  }

  def getFromTrialId(trialId: Int): Validation[String, Option[AbstractBlockRandomization]] = {
    database withSession {
      val resultList = queryRandomizationMethodFromTrialId(trialId).list
      if (resultList.isEmpty) Success(None)
      else if (resultList.size == 1) {
        val rm = resultList(0)
        if (rm._4 == classOf[BlockRandomization].getName) {
          val blockSize = getBlockSize(rm._1.get).either match {
            case Left(x) => return Failure(x)
            case Right(blocksize) => blocksize
          }
          val blockRandomization = new BlockRandomization(rm._1.get, 0, blockSize)(deserializeRandomGenerator(rm._3))
          getBlocks(blockRandomization)
          return Success(Some(blockRandomization))
        } else if (rm._4 == classOf[VariableBlockRandomization].getName) {
          val resultListBlock = queryBlockRandomizationFromId(rm._1.get).list
          if (resultListBlock.isEmpty) return Failure("Plugin entry not found")
          else if (resultListBlock.size > 1) return Failure("Database failure more than one entry")
          else {
            val minBlockSize = resultListBlock.head._5.getOrElse(return Failure("Minimal block size not found"))
            val maxBlockSize = resultListBlock.head._6.getOrElse(return Failure("Maximal block size not found"))

            val blockRandomization = new VariableBlockRandomization(rm._1.get, resultListBlock.head._2, minBlockSize, maxBlockSize)(deserializeRandomGenerator(rm._3))
            getBlocks(blockRandomization)
            return Success(Some(blockRandomization))
          }

        } else {
          Failure("Wrong plugin")
        }
      } else Failure("Duplicated database entry")
    }
  }


  def update(randomizationMethod: AbstractBlockRandomization): Validation[String, AbstractBlockRandomization] = {
    database withSession {
      threadLocalSession withTransaction {
        queryRandomizationMethodFromId(randomizationMethod.id).mutate {
          r =>
            r.row = r.row.copy(_2 = generateBlob(randomizationMethod.random).get, _3 = randomizationMethod.getClass().getName())
        }
        if (randomizationMethod.isInstanceOf[BlockRandomization]) {
          //update blocksize
          queryBlockRandomizationFromId(randomizationMethod.id).mutate {
            r =>
              r.row = r.row.copy(_4 = Some(randomizationMethod.asInstanceOf[BlockRandomization].blocksize))
          }
        } else if (randomizationMethod.isInstanceOf[VariableBlockRandomization]) {
          //update min, max and type
          val variableBlock = randomizationMethod.asInstanceOf[VariableBlockRandomization]
          queryBlockRandomizationFromId(randomizationMethod.id).mutate {
            r =>
              r.row = r.row.copy(_5 = Some(variableBlock.minBlockSize), _6 = Some(variableBlock.maxBlockSize))
          }
        }
        updateBlocks(randomizationMethod)
      }
    }
    get(randomizationMethod.id).either match {
      case Left(x) => Failure(x)
      case Right(None) => Failure("Method not found")
      case Right(Some(blockRandomizationMethod)) => Success(blockRandomizationMethod)
    }
  }

  def delete(randomizationMethod: AbstractBlockRandomization) {
    database withSession {
      queryBlockRandomizationFromId(randomizationMethod.id).mutate {
        r =>
          r.delete()
      }

      queryRandomizationMethodFromId(randomizationMethod.id).mutate {
        r =>
          r.delete()
      }

    }

  }

  private def getBlockSize(id: Int): Validation[String, Int] = {
    database withSession {
      val resultList = queryBlockRandomizationFromId(id).list
      if (resultList.isEmpty) Failure("Block size not found")
      else if (resultList.size == 1) Success(resultList(0)._4.getOrElse(return Failure("Blocksize not set")))
      else Failure("More than one block size found")
    }
  }

  private def getBlocks(blockRandomization: AbstractBlockRandomization) {
    database withSession {
      for (block <- queryBlocksFromId(blockRandomization.id)) {
        if (blockRandomization.blocks.get(block._3).isEmpty) blockRandomization.blocks.put(block._3, new ListBuffer())
        blockRandomization.blocks.get(block._3).get.append(block._2)
      }
    }
  }

  private def updateBlocks(randomizationMethod: AbstractBlockRandomization) {
    deleteBlocks(randomizationMethod)
    saveBlocks(randomizationMethod)
  }

  private def deleteBlocks(randomizationMethod: AbstractBlockRandomization) {
    queryBlocksFromId(randomizationMethod.id).mutate {
      block =>
        block.delete()
    }
  }

  private def saveBlocks(randomizationMethod: AbstractBlockRandomization) {
    randomizationMethod.blocks.foreach(entry => entry._2.foreach(armId => Blocks.noId insert(Some(randomizationMethod.id), armId, entry._1)))
  }

}
