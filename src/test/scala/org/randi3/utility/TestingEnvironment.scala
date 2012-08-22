package org.randi3.utility
import org.randi3.dao.RandomizationMethodDaoComponent
import org.scalaquery.ql.extended.ExtendedProfile
import org.scalaquery.session.Database
import org.randi3.schema.DatabaseSchema._
import org.randi3.schema.BlockRandomizationSchema._
import org.randi3.dao.TrialDaoComponent
import org.randi3.dao.TreatmentArmDaoComponent
import org.randi3.dao.TrialSubjectDaoComponent
import org.randi3.dao.TrialRightDaoComponent
import org.randi3.dao.TrialSiteDaoComponent
import org.randi3.dao.UserDaoComponent
import org.randi3.dao.BlockRandomizationDao
import org.randi3.randomization.RandomizationPluginManagerComponent
import org.apache.commons.math.random.MersenneTwister

import org.randi3.model._
import scala.collection.mutable.ListBuffer
import java.util.Date


object TestingEnvironment extends RandomizationMethodDaoComponent with TrialDaoComponent with TreatmentArmDaoComponent with TrialSubjectDaoComponent with TrialSiteDaoComponent  with TrialRightDaoComponent with UserDaoComponent{

  val databaseTupel: (Database, ExtendedProfile) = createDatabaseH2("Randi3TestDatabase")
  val database = databaseTupel._1
  val driver = databaseTupel._2

  createBlockRandomizationDatabaseTables(database, driver)

  lazy val blockRandomizationDao = new BlockRandomizationDao(database, driver)
  lazy val randomizationMethodDao = new RandomizationMethodDao
  lazy val trialDao = new TrialDao
  lazy val treatmentArmDao = new TreatmentArmDao
  lazy val trialSubjectDao = new TrialSubjectDao
  lazy val trialSiteDao = new TrialSiteDao
  lazy val trialRightDao = new TrialRightDao
  lazy val userDao = new UserDao
  lazy val criterionDao = new CriterionDao
  lazy val randomizationPluginManager = new RandomizationPluginManager
  
  val random = new MersenneTwister
  def trialName = "trial" + random.nextLong()
  def trialAbbreviation = "trial" + random.nextLong()
  def trialSiteName = "trialSite" + random.nextLong()
  def userName = "user" + random.nextLong()
  def treatmentArmName = "arm" + random.nextLong()
  
  def createTreatmentArm = new TreatmentArm(Int.MinValue, 0, treatmentArmName, "description", null, 100)
  
  def createTreatmentArms(count: Int) = {
    val result = new ListBuffer[TreatmentArm]()
    (1 to count).foreach{_ => result.append(createTreatmentArm)}
    result.toList
  }
  
  def createTrial = new Trial(Int.MinValue, 0, trialName, trialAbbreviation, "description", new Date, new Date, false, createTreatmentArms(2), null, null, None, Nil)
  
  def createTrialDB = trialDao.get(trialDao.create(createTrial).toOption.get).toOption.get.get
}
