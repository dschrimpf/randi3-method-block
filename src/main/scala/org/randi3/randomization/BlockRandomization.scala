package org.randi3.randomization

import org.randi3.model._
import org.randi3.randomization._
import org.apache.commons.math3.random._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import org.randi3.randomization.BlockRandomization._

class BlockRandomization(val id: Int = Int.MinValue, val version: Int = 0, val random: RandomGenerator, val blocksize: Int) extends RandomizationMethod {

  //Int because of easier database mapping
  val blocks = new HashMap[String, ListBuffer[Int]]()

  def randomize(trial: Trial, subject: TrialSubject): TreatmentArm = {
    pullFromBlock(trial, subject)
  }

  private def generateBlock(trial: Trial, subject: TrialSubject) {
    val stratum = subject.getStratum(trial.stratifyTrialSite)
    val block  = blocks.get(stratum).getOrElse{
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

  private def pullFromBlock(trial: Trial, subject: TrialSubject): TreatmentArm = {
    val stratum = subject.getStratum(trial.stratifyTrialSite)
    val block = blocks.get(stratum).getOrElse{ 
     generateBlock(trial, subject)
     blocks.get(stratum).get
    }
    if (block.isEmpty) generateBlock(trial, subject)
    val armPosition = random.nextInt(block.size)
    val armId = block(armPosition)
    block.remove(armPosition)
    trial.treatmentArms.find(arm => arm.id == armId).get
  }


}
object BlockRandomization {

  def generateRawBlock(trial: Trial): ListBuffer[TreatmentArm] = {
    val tmpBlock = new ListBuffer[TreatmentArm]

    val arms = trial.treatmentArms
    val sizes = arms.map(arm => arm.plannedSize)

    var divide = sizes(0)

    for (plannedSize <- sizes) {
      divide = gcd(divide, plannedSize)
    }

    for (arm <- arms) {
      val size = arm.plannedSize / divide
      for (i <- 0 until size) tmpBlock.append(arm)
    }

    tmpBlock
  }

  def gcd(x: Int, y: Int): Int =
    {
      if (x == 0) y
      else if (x < 0) gcd(-x, y)
      else if (y < 0) -gcd(x, -y)
      else gcd(y % x, x)
    }

  
}
