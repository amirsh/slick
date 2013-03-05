package scala.slick.typeProviders

import language.experimental.macros

object TypeProvider {
	type Db(url: String) = macro Macros.DbImpl
}