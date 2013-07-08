package org.randi3.schema


import scala.slick.driver.{BasicProfile, ExtendedProfile}
import scala.slick.lifted.DDL

/**
 * A simple example that uses statically typed queries against an in-memory
 * H2 database. The example data comes from Oracle's JDBC tutorial at
 * http://download.oracle.com/javase/tutorial/jdbc/basics/tables.html.
 */
class BlockRandomizationSchema(val driver: ExtendedProfile) {
  import driver.Implicit._

  import driver.simple._

  val schema = new DatabaseSchema(driver)
  import schema._

  object BlockRandomizations extends Table[(Int, Int, Option[Int], Option[Int], Option[Int], Option[Int])]("BlockRandomization") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def randomizationMethodId = column[Option[Int]]("RandomizationMethodId")

    def blocksize = column[Option[Int]]("Blocksize", O Nullable)

    def minBlockSize = column[Option[Int]]("MinBlocksize")

    def maxBlockSize = column[Option[Int]]("MaxBlocksize")

    def * = id ~ version ~ randomizationMethodId ~ blocksize ~ minBlockSize ~ maxBlockSize

    def noId = version ~ randomizationMethodId ~ blocksize ~ minBlockSize ~ maxBlockSize

    def randomizationMethod = foreignKey("BlockRandomizationFK_RandomizationMethod", randomizationMethodId, schema.RandomizationMethods)(_.id)
  }

  object Blocks extends Table[(Int, Option[Int], Int, String)]("Blocks") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

    def randomizationMethodId = column[Option[Int]]("RandomizationMethodId")

    def treatmentArmId = column[Int]("TreatmentArmId")

    def stratum = column[String]("Stratum")

    def * = id ~ randomizationMethodId ~ treatmentArmId ~ stratum

    def noId = randomizationMethodId ~ treatmentArmId ~ stratum

    def randomizationMethod = foreignKey("BlockFK_RandomizationMethod", randomizationMethodId, schema.RandomizationMethods)(_.id)

    def treatmentArm = foreignKey("BlockFK_TreatmentArm", treatmentArmId, schema.TreatmentArms)(_.id)
  }

  def getDatabaseTables: DDL = {
    (BlockRandomizations.ddl ++ Blocks.ddl)
  }

}
