package org.randi3.dao

import org.randi3.schema.DatabaseSchema._

import org.randi3.schema.BlockRandomizationSchema._

import org.scalaquery.session.Database
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.extended.ExtendedProfile

import org.randi3.randomization.BlockRandomization
import scala.collection.mutable.ListBuffer
import scalaz._

class BlockRandomizationDao(database: Database, driver: ExtendedProfile) extends AbstractRandomizationMethodDao(database, driver) {
  import driver.Implicit._

  private val queryBlockRandomizationFromId = for {
    id <- Parameters[Int]
    blockRandomization <- BlockRandomizations if blockRandomization.randomizationMethodId is id
  } yield blockRandomization.id ~ blockRandomization.version ~ blockRandomization.randomizationMethodId ~ blockRandomization.blocksize

  private val queryBlocksFromId = for {
    id <- Parameters[Int]
    blocks <- Blocks if blocks.randomizationMethodId is id
  } yield blocks.id ~ blocks.treatmentArmId ~ blocks.stratum

  def create(randomizationMethod: BlockRandomization, trialId: Int): Validation[String, Int] = {
    database withSession {
      val identifier =
        threadLocalSession withTransaction {
          RandomizationMethods.noId insert (trialId, generateBlob(randomizationMethod.random), randomizationMethod.getClass().getName())
          val id = getId(trialId).either match {
            case Left(x) => return Failure(x)
            case Right(id1) => id1
          }
          BlockRandomizations.noId insert (0, id, randomizationMethod.blocksize)
          id
        }
      Success(identifier)
    }
  }

  def get(id: Int): Validation[String, Option[BlockRandomization]] = {
    database withSession {
      val resultList = queryRandomizationMethodFromId(id).list
      if (resultList.isEmpty) Success(None)
      else if (resultList.size == 1) {
        val rm = resultList(0)
        if (rm._3 == classOf[BlockRandomization].getName()) {
          val blockSize = getBlockSize(id).either match {
            case Left(x) => return Failure(x)
            case Right(blocksize) => blocksize
          }
          val blockRandomization = new BlockRandomization(rm._1.get, 0, deserializeRandomGenerator(rm._2.get), blockSize)
          getBlocks(blockRandomization)
          return Success(Some(blockRandomization))
        } else {
          Failure("Wrong plugin")
        }
      } else Failure("Duplicated database entry")
    }
  }

  def getFromTrialId(trialId: Int): Validation[String, Option[BlockRandomization]] = {
    database withSession {
      val resultList = queryRandomizationMethodFromTrialId(trialId).list
      if (resultList.isEmpty) Success(None)
      else if (resultList.size == 1) {
        val rm = resultList(0)
        if (rm._4 == classOf[BlockRandomization].getName()) {
          val blockSize = getBlockSize(rm._1.get).either match {
            case Left(x) => return Failure(x)
            case Right(blocksize) => blocksize
          }
          val blockRandomization = new BlockRandomization(rm._1.get, 0, deserializeRandomGenerator(rm._3.get), blockSize)
          getBlocks(blockRandomization)
          return Success(Some(blockRandomization))
        } else {
          Failure("Wrong plugin")
        }
      } else Failure("Duplicated database entry")
    }
  }

  def update(randomizationMethod: BlockRandomization): Validation[String, BlockRandomization] = {
    database withSession {
      threadLocalSession withTransaction {
        queryRandomizationMethodFromId(randomizationMethod.id).mutate { r =>
          r.row = r.row.copy(_2 = generateBlob(randomizationMethod.random), _3 = randomizationMethod.getClass().getName())
        }
        //update blocksize
        queryBlockRandomizationFromId(randomizationMethod.id).mutate { r =>
          r.row = r.row.copy(_4 = randomizationMethod.blocksize)
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

  def delete(randomizationMethod: BlockRandomization) {

  }

  private def getBlockSize(id: Int): Validation[String, Int] = {
    database withSession {
      val resultList = queryBlockRandomizationFromId(id).list
      if (resultList.isEmpty) Failure("Block size not found")
      else if (resultList.size == 1) Success(resultList(0)._4)
      else Failure("More than one block size found")
    }
  }

  private def getBlocks(blockRandomization: BlockRandomization) {
    database withSession {
      for (block <- queryBlocksFromId(blockRandomization.id)) {
        if (blockRandomization.blocks.get(block._3).isEmpty) blockRandomization.blocks.put(block._3, new ListBuffer())
        blockRandomization.blocks.get(block._3).get.append(block._2)
      }
    }
  }

  private def updateBlocks(randomizationMethod: BlockRandomization) {
    deleteBlocks(randomizationMethod)
    saveBlocks(randomizationMethod)
  }

  private def deleteBlocks(randomizationMethod: BlockRandomization) {
    queryBlocksFromId(randomizationMethod.id).mutate { block =>
      block.delete()
    }
  }

  private def saveBlocks(randomizationMethod: BlockRandomization) {
    randomizationMethod.blocks.foreach(entry => entry._2.foreach(armId => Blocks.noId insert (randomizationMethod.id, armId, entry._1)))
  }

}
