# freearg

Monadic Command Line Argument Parser for Cats

```scala
import cats.*
import cats.effect.*
import com.github.tototoshi.freearg.FreeArg
import com.github.tototoshi.freearg.ArgParserException
import com.github.tototoshi.freearg.PrintUsageTextException

object Main extends IOApp:

  case class Config(
      help: Boolean,
      verbose: Boolean,
      encoding: String,
      input: String,
      output: String
  ):
    def dump: String = {
      s"""help: $help
         |verbose: $verbose
         |encoding: $encoding
         |input: $input
         |output: $output""".stripMargin
    }

  def run(args: List[String]): IO[ExitCode] =
    val p = new FreeArg[IO]

    val parser = for
      // define arguments
      h <- p.flag("h|help", "display usage text")
      v <- p.flag("v|verbose", "verbose flag")
      encoding <- p.optional("e|encoding", "file encoding")
      input <- p.positional("input", "input file")
      output <- p.positional("output", "output file")

      // parse arguments
      _ <- p.parse()

      // if help flag is set, print usage text and exit
      _ <- Monad[p.Parser].ifM(p.get(h))(p.printUsageText(), Monad[p.Parser].unit)

      // extract values
      h <- p.get(h)
      v <- p.get(v)
      encoding <- p.get[String](encoding)
      input <- p.get[String](input)
      output <- p.get[String](output)
    yield Config(h, v, encoding, input, output)

    val program = for
      config <- p.run(parser, args)
      _ <- IO.println(config.dump)
    yield ()

    program
      .handleErrorWith {
        case e: ArgParserException => IO.println(e.getMessage())
        case e: PrintUsageTextException => IO.println(e.getMessage())
       }
      .as(ExitCode.Success)



/*
sbt:freearg-example> r --verbose  --encoding utf-16 input.csv output.csv
help: false
verbose: true
encoding: utf-16
input: input.csv
output: output.csv

sbt:freearg-example> r -h
[--h|help] [--v|verbose] [--e|encoding VALUE] input output
```

