package org.randi3.randomization


import org.randi3.randomization._
import org.randi3.randomization.configuration.IntegerConfigurationType
import org.randi3.randomization.configuration._
import org.randi3.dao.BlockRandomizationDao
import org.randi3.model._

import org.scalaquery.ql._
import org.scalaquery.ql.extended.ExtendedProfile
import org.scalaquery.session.Database
import scalaz._

import org.apache.commons.math3.random._

import org.randi3.schema.BlockRandomizationSchema._

class BlockRandomizationPlugin(database: Database, driver: ExtendedProfile) extends RandomizationMethodPlugin(database, driver) {

  val name = classOf[BlockRandomization].getName()

  val i18nName = name

  val BlockRandomizationDao = new BlockRandomizationDao(database, driver)

  def randomizationConfigurations(): List[ConfigurationType[Any]] = {
    List(new IntegerConfigurationType(name="blocksize", description="blocksize"))
  }

  def randomizationMethod(random: RandomGenerator, trial: Trial, configuration: List[ConfigurationProperty[Any]]): Validation[String, RandomizationMethod] = {
    if(configuration.isEmpty) Failure("no configuration available")
    else Success(new BlockRandomization(random = random, blocksize = configuration.head.value.asInstanceOf[Int]))
  }

  def databaseTables(): Option[DDL] = {
    Some(getDatabaseTables(driver))
  }

  def create(randomizationMethod: RandomizationMethod, trialId: Int): Validation[String, Int] = {
    BlockRandomizationDao.create(randomizationMethod.asInstanceOf[BlockRandomization], trialId)
  }

  def get(id: Int): Validation[String, Option[RandomizationMethod]] = {
    BlockRandomizationDao.get(id)
  }

  def getFromTrialId(trialId: Int): Validation[String, Option[RandomizationMethod]] = { 
    BlockRandomizationDao.getFromTrialId(trialId)
  }

  def update(randomizationMethod: RandomizationMethod): Validation[String, RandomizationMethod] = {
    BlockRandomizationDao.update(randomizationMethod.asInstanceOf[BlockRandomization])
  }

  def delete(randomizationMethod: RandomizationMethod) {
    BlockRandomizationDao.delete(randomizationMethod.asInstanceOf[BlockRandomization])
  }

}
