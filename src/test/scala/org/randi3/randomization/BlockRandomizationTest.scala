package org.randi3.randomization

import org.junit.runner.RunWith
import org.scalatest.matchers.MustMatchers
import org.scalatest.FunSpec

import org.apache.commons.math3.random.MersenneTwister
import org.randi3.model._
import collection.mutable.ListBuffer
import org.randi3.model.criterion.{IntegerCriterion, OrdinalCriterion}
import org.randi3.model.criterion.constraint.{IntegerConstraint, OrdinalConstraint}

import scala.Some
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BlockRandomizationTest extends FunSpec with MustMatchers {

  import org.randi3.utility.TestingEnvironmentBlock._

  describe("A block randomization method") {

    it("should be balanced after the end of each block") {

      for (armCount <- 2 to 6) {

        for (blocksize <- 4 to 20) {
          if (blocksize % armCount == 0) {

            val arms = new ListBuffer[TreatmentArm]()

            //create the arms
            for (i <- 1 to armCount) arms.append(createTreatmentArm.copy(id = i, plannedSize = 100))

            val blockRandomizationMethod = new BlockRandomization(blocksize = blocksize)(random = new MersenneTwister())
            val trial = createTrial.copy(treatmentArms = arms.toList, randomizationMethod = Some(blockRandomizationMethod))


            val necessaryChecks = trial.plannedSubjects / blocksize
            10
            var checks = 0

            for (i <- 1 to trial.plannedSubjects) {
              val subject = TrialSubject(identifier = "subject" + i, investigatorUserName = "investigator", trialSite = trial.participatingSites.head, properties = Nil).toOption.get
              trial.randomize(subject).isSuccess must be(true)
              trial.getSubjects.size must be(i)
              if (i % blocksize == 0) {
                checks = checks + 1
                val comparisonSize = trial.treatmentArms.head.subjects.size
                comparisonSize must be > 0
                trial.treatmentArms.foreach(arm => arm.subjects.size must be(comparisonSize))
              }
            }
            necessaryChecks must be(checks)
          }
        }
      }

    }

    it("should be balanced in a subgroup after the end of each block (property stratification)") {

      for (armCount <- 2 to 6) {

        for (blocksize <- 4 to 20) {
          if (blocksize % armCount == 0) {

            val arms = new ListBuffer[TreatmentArm]()

            //create the arms
            for (i <- 1 to armCount) arms.append(createTreatmentArm.copy(id = i, plannedSize = 300))

            val blockRandomizationMethod = new BlockRandomization(blocksize = blocksize)(random = new MersenneTwister())

            val ordinalConstraints = List(
              OrdinalConstraint(id = 1, configurations = List(Some("a"))).toOption.get,
              OrdinalConstraint(id = 2, configurations = List(Some("b"))).toOption.get,
              OrdinalConstraint(id = 3, configurations = List(Some("c"))).toOption.get)

            val ordinalCriterion = OrdinalCriterion(id = 1, name = "ordinalCrit", description = "description", values = Set("a", "b", "c"), inclusionConstraint = None, strata = ordinalConstraints).toOption.get

            val integerConstraint = List(
              IntegerConstraint(id = 4, configurations = List(None, Some(40))).toOption.get,
              IntegerConstraint(id = 5, configurations = List(Some(41), Some(60))).toOption.get,
              IntegerConstraint(id = 6, configurations = List(Some(61), None)).toOption.get)


            val integerCriterion = IntegerCriterion(id = 2, name = "age", description = "description", inclusionConstraint = None, strata = integerConstraint).toOption.get

            val trial = createTrial.copy(
              treatmentArms = arms.toList,
              randomizationMethod = Some(blockRandomizationMethod),
              stratifyTrialSite = StratifiedTrialSite.NO,
              criterions = List(ordinalCriterion, integerCriterion))


            val groupCounter = new ListBuffer[(String, Int)]()
            groupCounter.append(("1_1;2_4", 0))
            groupCounter.append(("1_1;2_5", 0))
            groupCounter.append(("1_1;2_6", 0))
            groupCounter.append(("1_2;2_4", 0))
            groupCounter.append(("1_2;2_5", 0))
            groupCounter.append(("1_2;2_6", 0))
            groupCounter.append(("1_3;2_4", 0))
            groupCounter.append(("1_3;2_5", 0))
            groupCounter.append(("1_3;2_6", 0))


            for (i <- 1 to trial.plannedSubjects) {

              val properties: List[SubjectProperty[_ <: Any]] = {
                val remainder = (i - 1) % 9
                groupCounter(remainder) = (groupCounter(remainder)._1, groupCounter(remainder)._2 + 1)
                (remainder) match {
                  case 0 => List(SubjectProperty(criterion = integerCriterion, value = 20).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "a").toOption.get)
                  case 1 => List(SubjectProperty(criterion = integerCriterion, value = 50).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "a").toOption.get)
                  case 2 => List(SubjectProperty(criterion = integerCriterion, value = 61).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "a").toOption.get)
                  case 3 => List(SubjectProperty(criterion = integerCriterion, value = 40).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "b").toOption.get)
                  case 4 => List(SubjectProperty(criterion = integerCriterion, value = 54).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "b").toOption.get)
                  case 5 => List(SubjectProperty(criterion = integerCriterion, value = 89).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "b").toOption.get)
                  case 6 => List(SubjectProperty(criterion = integerCriterion, value = 40).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "c").toOption.get)
                  case 7 => List(SubjectProperty(criterion = integerCriterion, value = 58).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "c").toOption.get)
                  case 8 => List(SubjectProperty(criterion = integerCriterion, value = 70).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "c").toOption.get)

                }
              }

              val subject = TrialSubject(identifier = "subject" + i, investigatorUserName = "investigator", trialSite = trial.participatingSites.head, properties = properties).toOption.get

              trial.randomize(subject).isSuccess must be(true)

              trial.getSubjects.size must be(i)
              groupCounter.indices.foreach(index =>
                if (groupCounter(index)._2 > 0 && groupCounter(index)._2 % blocksize == 0) {
                  val comparisonSize = trial.treatmentArms.head.subjects.filter(subject => {
                    subject.getStratum(StratifiedTrialSite.NO) == groupCounter(index)._1
                  }).size
                  comparisonSize must be > 0
                  trial.treatmentArms.foreach(arm => arm.subjects.filter(subject => subject.getStratum(StratifiedTrialSite.NO) == groupCounter(index)._1).size must be(comparisonSize))
                }
              )

            }

          }
        }
      }
    }

    it("should be balanced in a subgroup after the end of each block (trial site stratification)") {

      for (armCount <- 2 to 6) {

        for (blocksize <- 4 to 20) {

          if (blocksize % armCount == 0) {

            val arms = new ListBuffer[TreatmentArm]()

            //create the arms
            for (i <- 1 to armCount) arms.append(createTreatmentArm.copy(id = i, plannedSize = 300))

            val blockRandomizationMethod = new BlockRandomization(blocksize = blocksize)(random = new MersenneTwister())

            val trialSites = List(createTrialSite.copy(id = 1), createTrialSite.copy(id = 2), createTrialSite.copy(id = 3))
            10
            val trial = createTrial.copy(
              treatmentArms = arms.toList,
              randomizationMethod = Some(blockRandomizationMethod),
              stratifyTrialSite = StratifiedTrialSite.YES_CLOSED,
              participatingSites = trialSites,
              criterions = List())


            val groupCounter = new ListBuffer[(String, Int)]()
            groupCounter.append(("1;", 0))
            groupCounter.append(("2;", 0))
            groupCounter.append(("3;", 0))


            for (i <- 1 to trial.plannedSubjects) {

              val site: TrialSite = {
                val remainder = (i - 1) % 3
                groupCounter(remainder) = (groupCounter(remainder)._1, groupCounter(remainder)._2 + 1)
                (remainder) match {
                  case 0 => trial.participatingSites(0)
                  case 1 => trial.participatingSites(1)
                  case 2 => trial.participatingSites(2)
                }
              }

              val subject = TrialSubject(identifier = "subject" + i, investigatorUserName = "investigator", trialSite = site, properties = Nil).toOption.get

              trial.randomize(subject).isSuccess must be(true)
              trial.getSubjects.size must be(i)
              groupCounter.indices.foreach(index =>
                if (groupCounter(index)._2 > 0 && groupCounter(index)._2 % blocksize == 0) {
                  val comparisonSize = trial.treatmentArms.head.subjects.filter(subject => {
                    subject.getStratum(StratifiedTrialSite.YES_CLOSED) == groupCounter(index)._1
                  }).size
                  comparisonSize must be > 0
                  trial.treatmentArms.foreach(arm => arm.subjects.filter(subject => subject.getStratum(StratifiedTrialSite.YES_CLOSED) == groupCounter(index)._1).size must be(comparisonSize))
                }
              )

            }

          }
        }
      }
    }



    it("should be balanced in a subgroup after the end of each block (property and trial site stratification)") {

      for (armCount <- 2 to 6) {
        for (blocksize <- 4 to 20) {
          if (blocksize % armCount == 0) {
            val arms = new ListBuffer[TreatmentArm]()

            //create the arms
            for (i <- 1 to armCount) arms.append(createTreatmentArm.copy(id = i, plannedSize = 300))

            val blockRandomizationMethod = new BlockRandomization(blocksize = blocksize)(random = new MersenneTwister())

            val ordinalConstraints = List(
              OrdinalConstraint(id = 1, configurations = List(Some("a"))).toOption.get,
              OrdinalConstraint(id = 2, configurations = List(Some("b"))).toOption.get,
              OrdinalConstraint(id = 3, configurations = List(Some("c"))).toOption.get)

            val ordinalCriterion = OrdinalCriterion(id = 1, name = "ordinalCrit", description = "description", values = Set("a", "b", "c"), inclusionConstraint = None, strata = ordinalConstraints).toOption.get

            val integerConstraint = List(
              IntegerConstraint(id = 4, configurations = List(None, Some(40))).toOption.get,
              IntegerConstraint(id = 5, configurations = List(Some(41), Some(60))).toOption.get,
              IntegerConstraint(id = 6, configurations = List(Some(61), None)).toOption.get)


            val integerCriterion = IntegerCriterion(id = 2, name = "age", description = "description", inclusionConstraint = None, strata = integerConstraint).toOption.get

            val trialSites = List(createTrialSite.copy(id = 1), createTrialSite.copy(id = 2), createTrialSite.copy(id = 3))

            val trial = createTrial.copy(
              treatmentArms = arms.toList,
              randomizationMethod = Some(blockRandomizationMethod),
              stratifyTrialSite = StratifiedTrialSite.YES_CLOSED,
              participatingSites = trialSites,
              criterions = List(ordinalCriterion, integerCriterion))


            val groupCounter = new ListBuffer[(String, Int)]()
            groupCounter.append(("1;1_1;2_4", 0))
            groupCounter.append(("1;1_1;2_5", 0))
            groupCounter.append(("1;1_1;2_6", 0))
            groupCounter.append(("1;1_2;2_4", 0))
            groupCounter.append(("1;1_2;2_5", 0))
            groupCounter.append(("1;1_2;2_6", 0))
            groupCounter.append(("1;1_3;2_4", 0))
            groupCounter.append(("1;1_3;2_5", 0))
            groupCounter.append(("1;1_3;2_6", 0))

            groupCounter.append(("2;1_1;2_4", 0))
            groupCounter.append(("2;1_1;2_5", 0))
            groupCounter.append(("2;1_1;2_6", 0))
            groupCounter.append(("2;1_2;2_4", 0))
            groupCounter.append(("2;1_2;2_5", 0))
            groupCounter.append(("2;1_2;2_6", 0))
            groupCounter.append(("2;1_3;2_4", 0))
            groupCounter.append(("2;1_3;2_5", 0))
            groupCounter.append(("2;1_3;2_6", 0))

            groupCounter.append(("3;1_1;2_4", 0))
            groupCounter.append(("3;1_1;2_5", 0))
            groupCounter.append(("3;1_1;2_6", 0))
            groupCounter.append(("3;1_2;2_4", 0))
            groupCounter.append(("3;1_2;2_5", 0))
            groupCounter.append(("3;1_2;2_6", 0))
            groupCounter.append(("3;1_3;2_4", 0))
            groupCounter.append(("3;1_3;2_5", 0))
            groupCounter.append(("3;1_3;2_6", 0))


            for (i <- 1 to trial.plannedSubjects) {

              val setting: (TrialSite, List[SubjectProperty[_ <: Any]]) = {

                val test = 8

                val remainder = (i - 1) % 27

                groupCounter(remainder) = (groupCounter(remainder)._1, groupCounter(remainder)._2 + 1)
                (remainder) match {
                  case 0 => (trialSites(0), List(SubjectProperty(criterion = integerCriterion, value = 20).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "a").toOption.get))
                  case 1 => (trialSites(0), List(SubjectProperty(criterion = integerCriterion, value = 50).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "a").toOption.get))
                  case 2 => (trialSites(0), List(SubjectProperty(criterion = integerCriterion, value = 62).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "a").toOption.get))
                  case 3 => (trialSites(0), List(SubjectProperty(criterion = integerCriterion, value = 20).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "b").toOption.get))
                  case 4 => (trialSites(0), List(SubjectProperty(criterion = integerCriterion, value = 50).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "b").toOption.get))
                  case 5 => (trialSites(0), List(SubjectProperty(criterion = integerCriterion, value = 62).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "b").toOption.get))
                  case 6 => (trialSites(0), List(SubjectProperty(criterion = integerCriterion, value = 20).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "c").toOption.get))
                  case 7 => (trialSites(0), List(SubjectProperty(criterion = integerCriterion, value = 50).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "c").toOption.get))
                  case 8 => (trialSites(0), List(SubjectProperty(criterion = integerCriterion, value = 62).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "c").toOption.get))
                  case 9 => (trialSites(1), List(SubjectProperty(criterion = integerCriterion, value = 20).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "a").toOption.get))
                  case 10 => (trialSites(1), List(SubjectProperty(criterion = integerCriterion, value = 50).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "a").toOption.get))
                  case 11 => (trialSites(1), List(SubjectProperty(criterion = integerCriterion, value = 62).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "a").toOption.get))
                  case 12 => (trialSites(1), List(SubjectProperty(criterion = integerCriterion, value = 20).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "b").toOption.get))
                  case 13 => (trialSites(1), List(SubjectProperty(criterion = integerCriterion, value = 50).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "b").toOption.get))
                  case 14 => (trialSites(1), List(SubjectProperty(criterion = integerCriterion, value = 62).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "b").toOption.get))
                  case 15 => (trialSites(1), List(SubjectProperty(criterion = integerCriterion, value = 20).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "c").toOption.get))
                  case 16 => (trialSites(1), List(SubjectProperty(criterion = integerCriterion, value = 50).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "c").toOption.get))
                  case 17 => (trialSites(1), List(SubjectProperty(criterion = integerCriterion, value = 62).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "c").toOption.get))
                  case 18 => (trialSites(2), List(SubjectProperty(criterion = integerCriterion, value = 20).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "a").toOption.get))
                  case 19 => (trialSites(2), List(SubjectProperty(criterion = integerCriterion, value = 50).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "a").toOption.get))
                  case 20 => (trialSites(2), List(SubjectProperty(criterion = integerCriterion, value = 62).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "a").toOption.get))
                  case 21 => (trialSites(2), List(SubjectProperty(criterion = integerCriterion, value = 20).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "b").toOption.get))
                  case 22 => (trialSites(2), List(SubjectProperty(criterion = integerCriterion, value = 50).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "b").toOption.get))
                  case 23 => (trialSites(2), List(SubjectProperty(criterion = integerCriterion, value = 62).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "b").toOption.get))
                  case 24 => (trialSites(2), List(SubjectProperty(criterion = integerCriterion, value = 20).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "c").toOption.get))
                  case 25 => (trialSites(2), List(SubjectProperty(criterion = integerCriterion, value = 50).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "c").toOption.get))
                  case 26 => (trialSites(2), List(SubjectProperty(criterion = integerCriterion, value = 62).toOption.get, SubjectProperty(criterion = ordinalCriterion, value = "c").toOption.get))
                }
              }

              val subject = TrialSubject(identifier = "subject" + i, investigatorUserName = "investigator", trialSite = setting._1, properties = setting._2).toOption.get

              trial.randomize(subject).isSuccess must be(true)

              trial.getSubjects.size must be(i)
              groupCounter.indices.foreach(index =>
                if (groupCounter(index)._2 > 0 && groupCounter(index)._2 % blocksize == 0) {
                  val comparisonSize = trial.treatmentArms.head.subjects.filter(subject => {
                    subject.getStratum(StratifiedTrialSite.YES_CLOSED) == groupCounter(index)._1
                  }).size
                  comparisonSize must be > 0
                  trial.treatmentArms.foreach(arm => arm.subjects.filter(subject => subject.getStratum(StratifiedTrialSite.YES_CLOSED) == groupCounter(index)._1).size must be(comparisonSize))
                }
              )

            }

          }
        }
      }
    }


  }
}
