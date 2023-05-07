package com.github.tototoshi.freearg

import cats.MonadError

trait ArgBind[M[_], T]:
  def bind(s: String): M[T]

object ArgBind:

  given [M[_]](using me: MonadError[M, Throwable]): ArgBind[M, String] with
    def bind(s: String): M[String] = me.pure(s)

  given [M[_]](using me: MonadError[M, Throwable]): ArgBind[M, Int] with
    def bind(s: String): M[Int] = try { me.pure(s.toInt) }
    catch { case e: NumberFormatException => me.raiseError(e) }
