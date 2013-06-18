package scala.slick.typeproviders

import language.experimental.macros

object TypeProvider {
<<<<<<< HEAD
  def generateCode(configurationFileName: String) = new CodeGenerator(configurationFileName).generateCode()
}
=======
  type Db(configurationFileName: String = "configuration") = macro Macros.DbImpl
  
  def generateCode(configurationFileName: String) = new CodeGenerator(configurationFileName).generateCode()
}
>>>>>>> origin/topic/type-providers
