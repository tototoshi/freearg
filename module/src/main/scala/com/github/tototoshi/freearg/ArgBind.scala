package com.github.tototoshi.freearg

import cats.*
import cats.implicits.*
import cats.instances.string

trait ArgBind[M[_], T]:
  def bind(name: String, stringValue: Option[String], defaultValue: Option[T]): M[T]

object ArgBind:

  given [M[_]](using ME: MonadError[M, Throwable]): ArgBind[M, String] with
    def bind(name: String, stringValue: Option[String], defaultValue: Option[String]): M[String] =
      stringValue
      .orElse(defaultValue)
      .map(ME.pure)
      .getOrElse(ME.raiseError(new ArgParserException(s"${name} is not specified")))

  given [M[_]](using ME: MonadError[M, Throwable]): ArgBind[M, Int] with
    def bind(name: String, stringValue: Option[String], defaultValue: Option[Int]): M[Int] =
      stringValue match
        case Some(s) =>
          try { ME.pure(s.toInt) }
          catch { case e: NumberFormatException => ME.raiseError(new ArgParserException(s"${name} could not be parsed as int")) }
        case None => defaultValue.map(ME.pure).getOrElse(ME.raiseError(new ArgParserException(s"option is not specified: $name")))

  given [M[_], T](using ME: MonadError[M, Throwable], B: ArgBind[M, T]): ArgBind[M, Option[T]] with
    def bind(name: String, stringValue: Option[String], defaultValue: Option[Option[T]]): M[Option[T]] =
      stringValue match
        case Some(s) => B.bind(name, Some(s), defaultValue.getOrElse(None)).map(Some(_))
        case None => ME.pure(None)
