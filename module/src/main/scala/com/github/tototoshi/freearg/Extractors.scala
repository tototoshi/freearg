package com.github.tototoshi.freearg

object Extractors {

  object OptionName {

    def unapply(s: String): Option[String] = {
      val longOptionNameRegex = """--(\w[\w_-]+)$""".r
      val shortOptionNameRegex = """-(\w)$""".r
      s match {
        case longOptionNameRegex(n) => Some(n)
        case shortOptionNameRegex(n) => Some(n)
        case _ => None
      }
    }

  }

  object OptionNameAndValue {

    def unapply(s: String): Option[(String, String)] = {
      val regex = """--(\w[\w_-]+)=(.*)""".r
      s match {
        case regex(n, v) => Some((n, v))
        case _ => None
      }
    }

  }

  object OptionValue {

    def unapply(s: String): Option[String] = {
      s match {
        case OptionName(_) => None
        case OptionNameAndValue(_, _) => None
        case _ => Some(s)
      }
    }

  }

}
