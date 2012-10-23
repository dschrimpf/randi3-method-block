package org.randi3.randomization

import org.randi3.model._
import org.randi3.randomization._
import org.apache.commons.math3.random._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import org.randi3.randomization.AbstractBlockRandomization._

class BlockRandomization(val id: Int = Int.MinValue, val version: Int = 0, val random: RandomGenerator, val blocksize: Int) extends AbstractBlockRandomization {





  override protected def generateBlock(trial: Trial, subject: TrialSubject) {
    val stratum = subject.getStratum(trial.stratifyTrialSite)
    val block = blocks.get(stratum).getOrElse {
      blocks.put(stratum, new ListBuffer())
      blocks.get(stratum).get
    }
    if (!block.isEmpty) return

    var tmpBlock = generateRawBlock(trial)

    val tmpRandom = new MersenneTwister
    for (i <- 0 until blocksize) {
      if (tmpBlock.isEmpty) tmpBlock = generateRawBlock(trial)
      val armPosition = tmpRandom.nextInt(tmpBlock.size)
      block.append(tmpBlock(armPosition).id)
      tmpBlock.remove(armPosition)
    }
  }




}
