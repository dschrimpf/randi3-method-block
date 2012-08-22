package org.randi3.method.block
import java.util.Date
import org.apache.commons.math3.random.MersenneTwister

import org.junit.runner.RunWith
import org.randi3.randomization.BlockRandomization
import org.randi3.randomization.BlockRandomization
import org.randi3.schema.BlockRandomizationSchema._

import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.extended.H2Driver.Implicit._
import org.scalaquery.ql._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.session.Database

import org.scalatest.matchers.MustMatchers
import org.scalatest.Spec
import org.specs.runner.JUnitSuiteRunner
import org.randi3.model._

@RunWith(classOf[JUnitSuiteRunner])
class BlockRandomizationDaoSpec extends Spec with MustMatchers {
  import org.randi3.utility.TestingEnvironment._

  describe("BlockRandomizationDao create method") {

    it("should be able to create a new block randomizationmethod with block size and without blocks") {
      val blockRandomization: BlockRandomization = new BlockRandomization(random = new MersenneTwister, blocksize = 8)
      val trialDB = createTrialDB
      val id = blockRandomizationDao.create(blockRandomization, trialDB.id).either match {
        case Left(x) => fail(x)
        case Right(id) => id
      }

      database withSession {
        val allBlockRandomizations = Query(BlockRandomizations).list
        allBlockRandomizations.size must be(1)
        allBlockRandomizations.head._3 must be(id)
        allBlockRandomizations.head._4 must be(blockRandomization.blocksize)
      }
    }


  }
}
