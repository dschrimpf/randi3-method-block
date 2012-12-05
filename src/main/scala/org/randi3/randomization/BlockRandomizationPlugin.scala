package org.randi3.randomization


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

import org.randi3.schema.{LiquibaseUtil, BlockRandomizationSchema}

class BlockRandomizationPlugin(database: Database, driver: ExtendedProfile) extends RandomizationMethodPlugin(database, driver) {


  val schema = new BlockRandomizationSchema(driver)
  import schema._


  val name = classOf[BlockRandomization].getName

  val i18nName = name

  val description = "Block randomization algorithm with a fixed block size"

  val canBeUsedWithStratification = true

  private val blockRandomizationDao = new BlockRandomizationDao(database, driver)

  private val blockSizeConfigurationType = new IntegerConfigurationType(name = "blocksize", description = "blocksize")

  def randomizationConfigurationOptions(): (List[ConfigurationType[Any]], List[Criterion[_ <: Any, Constraint[_ <: Any]]]) = {
    (List(blockSizeConfigurationType), Nil)
  }

  def getRandomizationConfigurations(id: Int): List[ConfigurationProperty[Any]] = {
    val method = blockRandomizationDao.get(id).toOption.getOrElse(return Nil).getOrElse(return Nil)
    List(new ConfigurationProperty[Any](blockSizeConfigurationType, method.asInstanceOf[BlockRandomization].blocksize))
  }

  def randomizationMethod(random: RandomGenerator, trial: Trial, configuration: List[ConfigurationProperty[Any]]): Validation[String, RandomizationMethod] = {
    if (configuration.isEmpty) Failure("No configuration available")
    else Success(new BlockRandomization(blocksize = configuration.head.value.asInstanceOf[Int])(random = random))
  }

  def databaseTables(): Option[DDL] = {
    Some(getDatabaseTables)
  }

  def updateDatabase() {
    LiquibaseUtil.updateDatabase(database, "db/db.changelog-master-block.xml", this.getClass.getClassLoader)
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
