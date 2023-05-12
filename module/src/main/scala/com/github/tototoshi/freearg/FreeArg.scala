package com.github.tototoshi.freearg

import cats.*
import cats.implicits.*
import cats.free.{Free, FreeT}
import cats.data.{State, StateT}

class PrintUsageTextException(message: String) extends RuntimeException(message)

class ArgParserException(message: String) extends RuntimeException(message)

class FreeArg[M[_]](using ME: MonadError[M, Throwable]):

  trait Matcher {
    val name: String
    def matches(n: String): Boolean = name.split("\\|").toList.exists(_ == n)
  }

  case class OptionalArg(name: String, description: String) extends Matcher

  case class FlagArg(name: String, description: String) extends Matcher

  case class PositionalArg(name: String, description: String) extends Matcher

  case class VariadicArg(name: String, description: String) extends Matcher

  sealed trait ParserA[T]

  case class DefineOptionalArg(arg: OptionalArg) extends ParserA[OptionalArg]

  case class DefineFlagArg(arg: FlagArg) extends ParserA[FlagArg]

  case class DefinePositionalArg(arg: PositionalArg) extends ParserA[PositionalArg]

  case class DefineVariadicArg(arg: VariadicArg) extends ParserA[VariadicArg]

  case class Parse() extends ParserA[Unit]

  case class GetOptionalArg[T](definition: OptionalArg, defaultValue: Option[T], binder: ArgBind[M, T])
      extends ParserA[T]

  case class GetFlagArg(definition: FlagArg) extends ParserA[Boolean]

  case class GetPositionalArg[T](definition: PositionalArg, defaultValue: Option[T], binder: ArgBind[M, T])
      extends ParserA[T]

  case class GetVariadicArg[T](definition: VariadicArg, binder: ArgBind[M, T]) extends ParserA[List[T]]

  case class PrintUsageText() extends ParserA[Unit]

  type ParserStateT[T] = StateT[M, ParserContext, T]

  type Parser[T] = FreeT[ParserA, ParserStateT, T]

  def optional(name: String, description: String = ""): Parser[OptionalArg] =
    FreeT.liftF(DefineOptionalArg(OptionalArg(name, description)))

  def flag(name: String, description: String = ""): Parser[FlagArg] =
    FreeT.liftF(DefineFlagArg(FlagArg(name, description)))

  def positional(name: String, description: String = ""): Parser[PositionalArg] =
    FreeT.liftF(DefinePositionalArg(PositionalArg(name, description)))

  def variadic(name: String, description: String = ""): Parser[VariadicArg] =
    FreeT.liftF(DefineVariadicArg(VariadicArg(name, description)))

  def parse(): Parser[Unit] =
    FreeT.liftF(Parse())

  def get[T](definition: OptionalArg)(using binder: ArgBind[M, T]): Parser[T] =
    FreeT.liftF(GetOptionalArg(definition, None, binder))

  def get[T](definition: FlagArg): Parser[Boolean] =
    FreeT.liftF(GetFlagArg(definition))

  def get[T](definition: PositionalArg)(using binder: ArgBind[M, T]): Parser[T] =
    FreeT.liftF(GetPositionalArg(definition, None, binder))

  def getOrElse[T](definition: OptionalArg, defaultValue: T)(using binder: ArgBind[M, T]): Parser[T] =
    FreeT.liftF(GetOptionalArg(definition, Some(defaultValue), binder))

  def getOrElse[T](definition: PositionalArg, defaultValue: T)(using binder: ArgBind[M, T]): Parser[T] =
    FreeT.liftF(GetPositionalArg(definition, Some(defaultValue), binder))

  def get[T](definition: VariadicArg)(using binder: ArgBind[M, T]): Parser[List[T]] =
    FreeT.liftF(GetVariadicArg(definition, binder))

  def run[T](program: Parser[T], args: List[String]): M[T] =
    program.foldMap(ParserInterpreter).runA(ParserContext.init(args))

  def printUsageText[T](): Parser[Unit] = FreeT.liftF(PrintUsageText())

  case class ParserContext(
      restArgs: List[String],
      optionalArgs: List[OptionalArg],
      flagArgs: List[FlagArg],
      positionalArgs: List[PositionalArg],
      variadicArgs: Option[VariadicArg],
      result: ParsingResult
  ):
    def register(optionalArg: OptionalArg): ParserContext = copy(optionalArgs = optionalArgs :+ optionalArg)
    def register(flagArg: FlagArg): ParserContext = copy(flagArgs = flagArgs :+ flagArg)
    def register(positionalArg: PositionalArg): ParserContext = copy(positionalArgs = positionalArgs :+ positionalArg)
    def register(variadicArg: VariadicArg): ParserContext = copy(variadicArgs = Some(variadicArg))

    def updateRestArgs(args: List[String]): ParserContext = copy(restArgs = args)
    def updateResult(f: ParsingResult => ParsingResult): ParserContext = copy(result = f(result))

  object ParserContext:
    def init(args: List[String]): ParserContext = ParserContext(args, Nil, Nil, Nil, None, ParsingResult.empty)

  case class ParsingResult(
      options: Map[String, String],
      flags: List[String],
      positionalArgs: Map[String, String],
      variadicArgs: List[String]
  ):
    def addOption(name: String, value: String): ParsingResult = copy(options = options + (name -> value))
    def getOption(name: String): Option[String] = options.get(name)
    def addFlag(name: String): ParsingResult = copy(flags = flags :+ name)
    def hasFlag(name: String): Boolean = flags.contains(name)
    def addPositionalArg(name: String, value: String): ParsingResult =
      copy(positionalArgs = positionalArgs + (name -> value))
    def getPositionalArg(name: String): Option[String] = positionalArgs.get(name)
    def addVariadicArg(value: String): ParsingResult = copy(variadicArgs = variadicArgs :+ value)

  object ParsingResult:
    def empty: ParsingResult = ParsingResult(Map.empty, Nil, Map.empty, Nil)

  def parseArgs(optionalArgs: List[OptionalArg]): ParserStateT[Unit] =
    Monad[ParserStateT].tailRecM(optionalArgs) { optionalArgs =>
      for
        ctx <- StateT.get
        result <- parseSingle(ctx)
      yield if (result) Left(optionalArgs) else Right(())
    }

  def parseSingle(ctx: ParserContext): ParserStateT[Boolean] =
    import Extractors._

    def getOptionalArg(name: String): Option[OptionalArg] = ctx.optionalArgs.find(_.matches(name))

    def hasOptionalArg(name: String): Boolean = getOptionalArg(name).isDefined

    def getFlagArg(name: String): Option[FlagArg] = ctx.flagArgs.find(_.matches(name))

    def hasFlagArg(name: String): Boolean = getFlagArg(name).isDefined

    def getOptionalArgName(name: String): String = getOptionalArg(name).map(_.name).get

    def getFlagArgName(name: String): String = getFlagArg(name).map(_.name).get

    def error[T](msg: String): ParserStateT[T] = StateT.liftF(ME.raiseError(new ArgParserException(msg)))

    ctx.restArgs match

      case OptionNameAndValue(name, value) :: rest if hasOptionalArg(name) =>
        for
          ctx <- StateT.get[M, ParserContext]
          _ <- StateT.set(ctx.updateRestArgs(rest).updateResult(_.addOption(getOptionalArgName(name), value)))
        yield true

      case OptionNameAndValue(name, value) :: rest if !hasOptionalArg(name) =>
        if (hasFlagArg(name)) error(s"name should not have a value: $name")
        else error(s"no such option: $name")

      case OptionName(name) :: OptionValue(value) :: rest if hasOptionalArg(name) =>
        for
          ctx <- StateT.get[M, ParserContext]
          _ <- StateT.set(ctx.updateRestArgs(rest).updateResult(_.addOption(getOptionalArgName(name), value)))
        yield true

      case OptionName(name) :: OptionValue(value) :: rest if !hasOptionalArg(name) && !hasFlagArg(name) =>
        error(s"no such option: $name")

      case OptionName(name) :: rest if hasOptionalArg(name) =>
        error(s"no value for option: $name")

      case OptionName(name) :: rest if hasFlagArg(name) =>
        for
          ctx <- StateT.get[M, ParserContext]
          _ <- StateT.set(ctx.updateRestArgs(rest).updateResult(_.addFlag(getFlagArgName(name))))
        yield true

      case OptionName(name) :: rest if !hasFlagArg(name) =>
        error(s"no such flag: $name")

      case value :: rest =>
        for
          ctx <- StateT.get[M, ParserContext]
          numOfPositionalArg = ctx.positionalArgs.size
          numOfParsedPositionalArg = ctx.result.positionalArgs.size
          ctx <-
            if numOfParsedPositionalArg < numOfPositionalArg
            then
              StateT.liftF(
                ME.pure(ctx.updateResult(_.addPositionalArg(ctx.positionalArgs(numOfParsedPositionalArg).name, value)))
              )
            else StateT.liftF(ME.pure(ctx.updateResult(_.addVariadicArg(value))))
          _ <- StateT.set(ctx.updateRestArgs(rest))
        yield true

      case Nil => StateT.pure(false)

  object ParserInterpreter extends (ParserA ~> ParserStateT):
    def apply[A](fa: ParserA[A]): ParserStateT[A] =
      fa match
        case DefineOptionalArg(o @ OptionalArg(name, _)) =>
          for
            ctx <- StateT.get[M, ParserContext]
            updated <- StateT.liftF(ME.pure(ctx.register(o)))
            _ <- StateT.set(updated)
          yield o

        case DefineFlagArg(f @ FlagArg(name, _)) =>
          for
            ctx <- StateT.get[M, ParserContext]
            updated <- StateT.liftF(ME.pure(ctx.register(f)))
            _ <- StateT.set(updated)
          yield f

        case DefinePositionalArg(p @ PositionalArg(name, _)) =>
          for
            ctx <- StateT.get[M, ParserContext]
            updated <- StateT.liftF(ME.pure(ctx.register(p)))
            _ <- StateT.set(updated)
          yield p

        case DefineVariadicArg(v @ VariadicArg(name, _)) =>
          for
            ctx <- StateT.get[M, ParserContext]
            updated <- StateT.liftF(ME.pure(ctx.register(v)))
            _ <- StateT.set(updated)
          yield v

        case Parse() =>
          for
            ctx <- StateT.get[M, ParserContext]
            _ <- parseArgs(ctx.optionalArgs)
          yield ()

        case GetOptionalArg(optionalArg, defaultValue, binder) =>
          for
            ctx <- StateT.get[M, ParserContext]
            value <- StateT.liftF(bindOptionalArg(ctx, optionalArg.name, defaultValue, binder))
          yield value

        case GetFlagArg(flagArg) =>
          for
            ctx <- StateT.get[M, ParserContext]
            value <- StateT.liftF(ME.pure(ctx.result.hasFlag(flagArg.name)))
          yield value

        case GetPositionalArg(positionalArg, defaultValue, binder) =>
          for
            ctx <- StateT.get[M, ParserContext]
            value <- StateT.liftF(bindPositionalArg(ctx, positionalArg.name, defaultValue, binder))
          yield value

        case GetVariadicArg(variadicArg, binder) =>
          for
            ctx <- StateT.get[M, ParserContext]
            value <- StateT.liftF(bindVariadicArg(ctx, variadicArg.name, binder))
          yield value

        case PrintUsageText() =>
          for
            ctx <- StateT.get[M, ParserContext]
            text = makeUsageText(ctx)
            _ <- StateT.liftF(ME.raiseError(new PrintUsageTextException(text)))
          yield ()

    private def makeUsageText(ctx: ParserContext): String =
      ctx.flagArgs
        .map { f =>
          f.name
            .split("\\|")
            .toList
            .map(key => if key.length == 1 then s"-${key}" else s"--${key}")
            .mkString("[", "|", "]")
        }
        .mkString(" ") +
        " " +
        ctx.optionalArgs
          .map { f =>
            f.name
              .split("\\|")
              .toList
              .map(key => if key.length == 1 then s"-${key}" else s"--${key}")
              .mkString("[", "|", " VALUE]")
          }
          .mkString(" ") +
        " " +
        ctx.positionalArgs.map(p => s"${p.name}").mkString(" ") +
        " " +
        ctx.variadicArgs.map(v => s"${v.name}...").mkString(" ")

    private def bindOptionalArg[A](
        ctx: ParserContext,
        name: String,
        defaultValue: Option[A],
        binder: ArgBind[M, A]
    ): M[A] =
      binder.bind(name, ctx.result.getOption(name), defaultValue)

    private def bindPositionalArg[A](
        ctx: ParserContext,
        name: String,
        defaultValue: Option[A],
        binder: ArgBind[M, A]
    ): M[A] =
      binder.bind(name, ctx.result.getPositionalArg(name), defaultValue)

    private def bindVariadicArg[A](ctx: ParserContext, name: String, binder: ArgBind[M, A]): M[List[A]] =
      Traverse[List].traverse(ctx.result.variadicArgs)(v => binder.bind(name, Some(v), None))
