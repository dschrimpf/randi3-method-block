package org.randi3.randomization

import org.randi3.model.{TrialSubject, TreatmentArm, Trial}
import collection.mutable.{HashMap, ListBuffer}
import org.apache.commons.math3.random.RandomGenerator


abstract class AbstractBlockRandomization extends RandomizationMethod {


  val id: Int

  val version: Int

  val random: RandomGenerator


  //Int because of easier database mapping
  val blocks = new HashMap[String, ListBuffer[Int]]()


  protected def generateBlock(trial: Trial, subject: TrialSubject)


  def randomize(trial: Trial, subject: TrialSubject): TreatmentArm = {
    pullFromBlock(trial, subject)
  }

  private def pullFromBlock(trial: Trial, subject: TrialSubject): TreatmentArm = {
    val stratum = subject.getStratum(trial.stratifyTrialSite)
    val block = blocks.get(stratum).getOrElse {
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

object AbstractBlockRandomization {

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

  def gcd(x: Int, y: Int): Int = {
    if (x == 0) y
    else if (x < 0) gcd(-x, y)
    else if (y < 0) -gcd(x, -y)
    else gcd(y % x, x)
  }
}
