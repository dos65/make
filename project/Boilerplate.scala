import sbt._

/**
  * Copied, with some modifications, from https://github.com/milessabin/shapeless/blob/master/project/Boilerplate.scala
  *
  * Generate a range of boilerplate classes, those offering alternatives with 0-22 params
  * and would be tedious to craft by hand
  */
object Boilerplate {

  import scala.StringContext._

  implicit class BlockHelper(val sc: StringContext) extends AnyVal {
    def block(args: Any*): String = {
      val interpolated = sc.standardInterpolator(treatEscapes, args)
      val rawLines = interpolated split '\n'
      val trimmedLines = rawLines map { _ dropWhile (_.isWhitespace) }
      trimmedLines mkString "\n"
    }
  }


  val templates: Seq[Template] = List(
    MakeProductNOps, MakeTupleInstances, MakeTupleSyntaxClasses, MakeTupleSyntax
  )

  /** Returns a seq of the generated files.  As a side-effect, it actually generates them... */
  def gen(dir : File) = for(t <- templates) yield {
    val tgtFile = dir / t.packageName / t.filename
    IO.write(tgtFile, t.body)
    tgtFile
  }

  /*
    Blocks in the templates below use a custom interpolator, combined with post-processing to produce the body

      - The contents of the `header` val is output first

      - Then the first block of lines beginning with '|'

      - Then the block of lines beginning with '-' is replicated once for each arity,
        with the `templateVals` already pre-populated with relevant relevant vals for that arity

      - Then the last block of lines prefixed with '|'

    The block otherwise behaves as a standard interpolated string with regards to variable substitution.
  */
  object MakeProductNOps extends Template {
    override def packageName: String = "make/internal/MakeProductNOps"
    override def filename: String = "MakeProductNOps.scala"
    override def range: Range = 2 to 22
    override def content(tv: TemplateVals): String = {

      import tv._

      val apArgs = synVals.reverse.dropRight(1)
      val impl = synVals.foldLeft(""){
        case ("" , c) => s"map($c)(${synVals.mkString("", " => ", " => ")} ${`(a..n)`})"
        case (acc, c) => s"ap($c)($acc)"   
      }
      val toArity = arity - 1
      val funcTags = 
        (1 to toArity).map(x => {
          val in = (x to toArity).map(n => (n+'A').toChar).map(_.toChar).mkString(" => ")
          s"$in => ${`(A..N)`}"
        }).map(s => s"Tag[$s]")

      val implictTags =
        (s"Tag[${synTypes.mkString("(",",", ")")}]" :: funcTags.toList)
          .zipWithIndex.map{case (t, i) => s"tag$i: $t"}
          .mkString(",")


      block"""
           |package make.internal
           |
           |import make.Make
           |import make.Tag
           |import cats.Applicative
           |
           |trait MakeProductNOps extends MakeBasicOps {
           | 
           -  def productN[Eff[_]: Applicative, ${`A..N`}](${`MakeA..MakeN`})(implicit $implictTags): Make[Eff, ${`(A..N)`}] =
           -    $impl
           |}
           """
    }
  } 

  object MakeTupleInstances extends Template {
    override def packageName: String = "make/MakeTupleInstances"
    override def filename: String = "MakeTupleInstances.scala"
    override def range: Range = 2 to 22
    override def content(tv: TemplateVals): String = {

      import tv._

      val toArity = arity - 1
      val funcTags = 
        (1 to toArity).map(x => {
          val in = (x to toArity).map(n => (n+'A').toChar).map(_.toChar).mkString(" => ")
          s"$in => ${`(A..N)`}"
        }).map(s => s"Tag[$s]")

      val implictTags =
        (s"Tag[${synTypes.mkString("(",",", ")")}]" :: funcTags.toList)
          .zipWithIndex.map{case (t, i) => s"tag$i: $t"}
      

      val deps = (0 until arity).map(n => {
        val tpe = (n+'A').toChar
        val arg = (n+'a').toChar
        s"$arg: Make[Eff, $tpe]"
      })
      val args = synVals.mkString(",")

      val implicitValues = (deps ++ implictTags).mkString(",")


      block"""
           |package make
           |
           |import make.internal.MakeOps
           |import cats.Applicative
           |
           |trait MakeTupleInstances {
           | 
           -  implicit def tuple$arity[Eff[_]: Applicative, ${`A..N`}](implicit $implicitValues): Make[Eff, ${`(A..N)`}] =
           -    MakeOps.productN($args)
           |}
           """
    }
  } 

  object MakeTupleSyntaxClasses extends Template {
    override def packageName: String = "make/tupleNSyntaxClasses"
    override def filename: String = "tupleNSyntaxClasses.scala"
    override def range: Range = 2 to 22
    override def content(tv: TemplateVals): String = {

      import tv._

      block"""
           |package make
           |
           |import make.internal.MakeOps
           |import cats.Applicative
           |import cats.effect.Resource
           |
           |object tupleNSyntaxClasses {
           -  class MakeTupleNSyntax$arity[Eff[_], ${`A..N`}](private val v: Make[Eff, ${`(A..N)`}]) extends AnyVal {
           -    def mapN[Res: Tag](ff: ${`(A..N)`} => Res)(implicit Eff: Applicative[Eff]): Make[Eff, Res] =
           -      MakeOps.map(v)({case ${`(a..n)`} => ff${`(a..n)`}})
           -
           -    def mapFN[Res: Tag](ff: ${`(A..N)`} => Eff[Res])(implicit Eff: Applicative[Eff]): Make[Eff, Res] =
           -      MakeOps.mapF(v)({case ${`(a..n)`} => ff${`(a..n)`}})
           -
           -    def mapResourceN[Res: Tag](ff: ${`(A..N)`} => Resource[Eff, Res]): Make[Eff, Res] =
           -      MakeOps.mapResource(v)({case ${`(a..n)`} => ff${`(a..n)`}})
           -  }
           |}
           """
    }
  } 

  object MakeTupleSyntax extends Template {
    override def packageName: String = "make/MakeTupleSyntax"
    override def filename: String = "MakeTupleSyntax.scala"
    override def range: Range = 2 to 22
    override def content(tv: TemplateVals): String = {

      import tv._

      block"""
           |package make
           |
           |import make.internal.MakeOps
           |import cats.Applicative
           |
           |trait MakeTupleSyntax {
           |
           -  implicit def makeToTupleNSyntax$arity[Eff[_], ${`A..N`}](make: Make[Eff, ${`(A..N)`}]) =
           -    new tupleNSyntaxClasses.MakeTupleNSyntax$arity(make)
           -
           |}
           """
    }
  } 

  trait Template { self =>

    def packageName: String

    def createVals(arity: Int): TemplateVals = new TemplateVals(arity)

    def filename: String
    def content(tv: TemplateVals): String
    def range: Range = 1 to 22
    def body: String = {
      val rawContents = range map { n => content(createVals(n)) split '\n' filterNot (_.isEmpty) }
      val preBody = rawContents.head takeWhile (_ startsWith "|") map (_.tail)
      val instances = rawContents flatMap {_ filter (_ startsWith "-") map (_.tail) }
      val postBody = rawContents.head dropWhile (_ startsWith "|") dropWhile (_ startsWith "-") map (_.tail)
      (preBody ++ instances ++ postBody) mkString "\n"
    }
  }

  class TemplateVals(val arity: Int) {
    val synTypes     = (0 until arity) map (n => (n+'A').toChar)
    val synVals      = (0 until arity) map (n => (n+'a').toChar)
    val synTypedVals = (synVals zip synTypes) map { case (v,t) => v + ":" + t}

    val `A..N`       = synTypes.mkString(", ")
    val `MakeA..MakeN` = (synVals zip synTypes.map(t => s"Make[Eff, $t]")) map { case (v, t) => s"$v: $t"} mkString(",")

    val `(A..N)`     = if (arity == 1) "Tuple1[A]" else synTypes.mkString("(", ", ", ")")
    val `(a..n)`     = if (arity == 1) "Tuple1(a)" else synVals.mkString("(", ", ", ")")
  }

}