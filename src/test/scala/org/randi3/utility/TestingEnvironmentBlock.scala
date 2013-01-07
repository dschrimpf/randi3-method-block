package org.randi3.utility

import org.randi3.dao._
import org.randi3.schema.{LiquibaseUtil, BlockRandomizationSchema}


object TestingEnvironmentBlock extends TestingEnvironment{


  val schemaBlock = new BlockRandomizationSchema(driver)

  LiquibaseUtil.updateDatabase(database, "db/db.changelog-master-block.xml", this.getClass.getClassLoader)

  lazy val blockRandomizationDao = new BlockRandomizationDao(database, driver)

}