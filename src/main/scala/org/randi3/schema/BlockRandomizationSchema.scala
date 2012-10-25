package org.randi3.schema

import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.extended.{ExtendedTable => Table}
import org.randi3.model._
import org.scalaquery.ql.extended._
import org.scalaquery.ql.basic._
import java.sql.Blob
import java.sql.Date

import org.randi3.schema.DatabaseSchema._

/**
 * A simple example that uses statically typed queries against an in-memory
 * H2 database. The example data comes from Oracle's JDBC tutorial at
 * http://download.oracle.com/javase/tutorial/jdbc/basics/tables.html.
 */
object BlockRandomizationSchema {

  val BlockRandomizations = new Table[(Int, Int, Int, Option[Int], Option[Int], Option[Int])]("BlockRandomization") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def randomizationMethodId = column[Int]("RandomizationMethodId")

    def blocksize = column[Option[Int]]("Blocksize", O Nullable)

    def minBlockSize = column[Option[Int]]("MinBlocksize")

    def maxBlockSize = column[Option[Int]]("MaxBlocksize")

    def * = id ~ version ~ randomizationMethodId ~ blocksize ~ minBlockSize ~ maxBlockSize

    def noId = version ~ randomizationMethodId ~ blocksize ~ minBlockSize ~ maxBlockSize

    def randomizationMethod = foreignKey("BlockRandomizationFK_RandomizationMethod", randomizationMethodId, RandomizationMethods)(_.id.get)
  }

  val Blocks = new Table[(Int, Int, Int, String)]("Blocks") {
    def id = column[Int]("ID", O PrimaryKey, O AutoInc)

    def randomizationMethodId = column[Int]("RandomizationMethodId")

    def treatmentArmId = column[Int]("TreatmentArmId")

    def stratum = column[String]("Stratum")

    def * = id ~ randomizationMethodId ~ treatmentArmId ~ stratum

    def noId = randomizationMethodId ~ treatmentArmId ~ stratum

    def randomizationMethod = foreignKey("BlockFK_RandomizationMethod", randomizationMethodId, RandomizationMethods)(_.id.get)

    def treatmentArm = foreignKey("BlockFK_TreatmentArm", treatmentArmId, TreatmentArms)(_.id)
  }

  def getDatabaseTables(driver: ExtendedProfile): DDL = {
    import driver.Implicit._
    (BlockRandomizations.ddl ++ Blocks.ddl)
  }

  def createBlockRandomizationDatabaseTables(database: Database, driver: ExtendedProfile) = {
    import driver.Implicit._

    database withSession {
      (BlockRandomizations.ddl ++ Blocks.ddl).create
    }
  }

}
