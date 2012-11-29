package org.randi3.method.block

import org.apache.commons.math3.random.MersenneTwister

import org.junit.runner.RunWith
import org.randi3.randomization.BlockRandomization

import org.scalaquery.ql.extended.H2Driver.Implicit._
import org.scalaquery.ql._
import org.scalaquery.session.Database.threadLocalSession


import org.scalatest.matchers.{ShouldMatchers, MustMatchers}
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.randi3.utility.TestingEnvironment._
import scala.Left
import org.randi3.randomization.BlockRandomization
import scala.Right
import scala.Some


@RunWith(classOf[JUnitRunner])
class BlockRandomizationDaoSpec extends FunSpec with MustMatchers with ShouldMatchers {

  import org.randi3.utility.TestingEnvironment._

  import schemaBlock._

  describe("BlockRandomizationDao create method") {

    it("should be able to create a new block randomizationmethod with block size and without blocks") {
      val blockRandomization: BlockRandomization = new BlockRandomization(blocksize = 8)(random = new MersenneTwister)
      val trialDB = createTrialDB
      val id = blockRandomizationDao.create(blockRandomization, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(id) => id
      }

      database withSession {
        val allBlockRandomizations = Query(BlockRandomizations).list
        allBlockRandomizations.size must be(1)
        allBlockRandomizations.head._3 must be(Some(id))
        allBlockRandomizations.head._4 must be(Some(blockRandomization.blocksize))
        allBlockRandomizations.head._5 must be(None)
        allBlockRandomizations.head._6 must be(None)

      }
    }


  }
}
