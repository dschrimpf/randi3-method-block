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
import org.randi3.utility.{I18NHelper, I18NRandomization, AbstractSecurityUtil}

class VariableBlockRandomizationPlugin(database: Database, driver: ExtendedProfile, securityUtil: AbstractSecurityUtil) extends RandomizationMethodPlugin(database, driver, securityUtil) {

  private val i18n = new I18NRandomization(I18NHelper.getLocalizationMap("blockRandomizationM", getClass.getClassLoader), securityUtil)

  val schema = new BlockRandomizationSchema(driver)
  import schema._

  val name = classOf[VariableBlockRandomization].getName

  def i18nName = i18n.text("variableBlock.name")

  def description = i18n.text("variableBlock.description")

  val canBeUsedWithStratification = true

  private val blockRandomizationDao = new BlockRandomizationDao(database, driver)

  private def minBlockSizeConfigurationType = new IntegerConfigurationType(name = i18n.text("variableBlock.minBlockSize"), description = i18n.text("variableBlock.minBlockSizeDesc"))
  private def maxBlockSizeConfigurationType = new IntegerConfigurationType(name = i18n.text("variableBlock.maxBlockSize"), description = i18n.text("variableBlock.maxBlockSize"))

  def randomizationConfigurationOptions(): (List[ConfigurationType[Any]], Map[String, List[Criterion[_ <: Any, Constraint[_ <: Any]]]]) = {
    (List(
      minBlockSizeConfigurationType,
      maxBlockSizeConfigurationType
    ), Map())
  }

  def getRandomizationConfigurations(id: Int): List[ConfigurationProperty[Any]] = {
    val method = blockRandomizationDao.get(id).toOption.getOrElse(return Nil).getOrElse(return Nil)
    List(new ConfigurationProperty[Any](minBlockSizeConfigurationType, method.asInstanceOf[VariableBlockRandomization].minBlockSize),
      new ConfigurationProperty[Any](maxBlockSizeConfigurationType, method.asInstanceOf[VariableBlockRandomization].maxBlockSize))
  }

  def randomizationMethod(random: RandomGenerator, trial: Trial, configuration: List[ConfigurationProperty[Any]]): Validation[String, RandomizationMethod] = {
    if (configuration.isEmpty) Failure( i18n.text("variableBlock.configurationNotSet"))
    else {
      val minBlockSize = configuration.find(conf => conf.configurationType.name == minBlockSizeConfigurationType.name).getOrElse(return Failure( i18n.text("variableBlock.minBlockNotSet")))
      val maxBlockSize = configuration.find(conf => conf.configurationType.name == maxBlockSizeConfigurationType.name).getOrElse(return Failure( i18n.text("variableBlock.maxBlockNotSet")))
      Success(new VariableBlockRandomization(minBlockSize = minBlockSize.value.asInstanceOf[Int], maxBlockSize = maxBlockSize.value.asInstanceOf[Int])(random = random))
    }
  }

  def databaseTables(): Option[DDL] = {
    Some(getDatabaseTables)
  }

  def updateDatabase() {
    LiquibaseUtil.updateDatabase(database, "db/db.changelog-master-block.xml", this.getClass.getClassLoader)
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
