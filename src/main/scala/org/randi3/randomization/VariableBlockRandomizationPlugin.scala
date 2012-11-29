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
import org.randi3.schema.BlockRandomizationSchema

class VariableBlockRandomizationPlugin(database: Database, driver: ExtendedProfile) extends RandomizationMethodPlugin(database, driver) {

  val schema = new BlockRandomizationSchema(driver)
  import schema._

  val name = classOf[VariableBlockRandomization].getName

  val i18nName = name

  val description = "Block randomization algorithm with variable block size (either with multiple block sizes of the proportion of the treatment arms or absolute flexible block sizes)"

  val canBeUsedWithStratification = true

  private val blockRandomizationDao = new BlockRandomizationDao(database, driver)

  private val minBlockSizeConfigurationType = new IntegerConfigurationType(name = "minimal block size", description = "minimal block size")
  private val maxBlockSizeConfigurationType = new IntegerConfigurationType(name = "maximal block size", description = "maximal block size")

  def randomizationConfigurationOptions(): (List[ConfigurationType[Any]], List[Criterion[_ <: Any, Constraint[_ <: Any]]]) = {
    (List(
      minBlockSizeConfigurationType,
      maxBlockSizeConfigurationType
    ), Nil)
  }

  def getRandomizationConfigurations(id: Int): List[ConfigurationProperty[Any]] = {
    val method = blockRandomizationDao.get(id).toOption.getOrElse(return Nil).getOrElse(return Nil)
    List(new ConfigurationProperty[Any](minBlockSizeConfigurationType, method.asInstanceOf[VariableBlockRandomization].minBlockSize),
      new ConfigurationProperty[Any](maxBlockSizeConfigurationType, method.asInstanceOf[VariableBlockRandomization].maxBlockSize))
  }

  def randomizationMethod(random: RandomGenerator, trial: Trial, configuration: List[ConfigurationProperty[Any]]): Validation[String, RandomizationMethod] = {
    if (configuration.isEmpty) Failure("no configuration available")
    else {
      val minBlockSize = configuration.find(conf => conf.configurationType.name == minBlockSizeConfigurationType.name).getOrElse(return Failure("Min block size not found"))
      val maxBlockSize = configuration.find(conf => conf.configurationType.name == maxBlockSizeConfigurationType.name).getOrElse(return Failure("Max block size not found"))
      Success(new VariableBlockRandomization(minBlockSize = minBlockSize.value.asInstanceOf[Int], maxBlockSize = maxBlockSize.value.asInstanceOf[Int])(random = random))
    }
  }

  def databaseTables(): Option[DDL] = {
    Some(getDatabaseTables)
  }

  def create(randomizationMethod: RandomizationMethod, trialId: Int): Validation[String, Int] = {
    blockRandomizationDao.create(randomizationMethod.asInstanceOf[VariableBlockRandomization], trialId)
  }

  def get(id: Int): Validation[String, Option[RandomizationMethod]] = {
    blockRandomizationDao.get(id)
  }

  def getFromTrialId(trialId: Int): Validation[String, Option[RandomizationMethod]] = {
    blockRandomizationDao.getFromTrialId(trialId)
  }

  def update(randomizationMethod: RandomizationMethod): Validation[String, RandomizationMethod] = {
    blockRandomizationDao.update(randomizationMethod.asInstanceOf[VariableBlockRandomization])
  }

  def delete(randomizationMethod: RandomizationMethod) {
    blockRandomizationDao.delete(randomizationMethod.asInstanceOf[VariableBlockRandomization])
  }

}
