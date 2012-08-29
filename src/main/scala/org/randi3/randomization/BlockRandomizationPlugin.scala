package org.randi3.randomization


import org.randi3.randomization._
import org.randi3.randomization.configuration.IntegerConfigurationType
import org.randi3.randomization.configuration._
import org.randi3.dao.BlockRandomizationDao
import org.randi3.model._
import org.randi3.model.criterion.Criterion
import org.randi3.model.criterion.constraint.Constraint
import org.scalaquery.ql._
import org.scalaquery.ql.extended.ExtendedProfile
import org.scalaquery.session.Database
import scalaz._

import org.apache.commons.math3.random._

import org.randi3.schema.BlockRandomizationSchema._

class BlockRandomizationPlugin(database: Database, driver: ExtendedProfile) extends RandomizationMethodPlugin(database, driver) {

  val name = classOf[BlockRandomization].getName

  val i18nName = name

  val description = name

  val canBeUsedWithStratification = true

  private val blockRandomizationDao = new BlockRandomizationDao(database, driver)

  def randomizationConfigurationOptions(): (List[ConfigurationType[Any]], List[Criterion[_ <: Any, Constraint[_ <: Any]]]) = {
    (List(new IntegerConfigurationType(name="blocksize", description="blocksize")), Nil)
  }

  def getRandomizationConfigurations: List[ConfigurationProperty[Any]] = {
    Nil
  }

  def randomizationMethod(random: RandomGenerator, trial: Trial, configuration: List[ConfigurationProperty[Any]]): Validation[String, RandomizationMethod] = {
    if(configuration.isEmpty) Failure("no configuration available")
    else Success(new BlockRandomization(random = random, blocksize = configuration.head.value.asInstanceOf[Int]))
  }

  def databaseTables(): Option[DDL] = {
    Some(getDatabaseTables(driver))
  }

  def create(randomizationMethod: RandomizationMethod, trialId: Int): Validation[String, Int] = {
    blockRandomizationDao.create(randomizationMethod.asInstanceOf[BlockRandomization], trialId)
  }

  def get(id: Int): Validation[String, Option[RandomizationMethod]] = {
    blockRandomizationDao.get(id)
  }

  def getFromTrialId(trialId: Int): Validation[String, Option[RandomizationMethod]] = { 
    blockRandomizationDao.getFromTrialId(trialId)
  }

  def update(randomizationMethod: RandomizationMethod): Validation[String, RandomizationMethod] = {
    blockRandomizationDao.update(randomizationMethod.asInstanceOf[BlockRandomization])
  }

  def delete(randomizationMethod: RandomizationMethod) {
    blockRandomizationDao.delete(randomizationMethod.asInstanceOf[BlockRandomization])
  }

}
