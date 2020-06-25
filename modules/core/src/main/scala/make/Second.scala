package sdi

import cats.effect.Resource
import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

// trait InstanceTree[F[_], A]

// trait EffForInstances[F[_]]

// sealed trait Instance2[A] {
//   def tag: Tag[A]
// }
// object Instance2 {

//   final case class Pure[A](v: A, tag: Tag[A]) extends Instance2[A]
//   final case class Func[In, A](prev: Instance2[In], f: In => A, tag: Tag[A]) extends Instance2[A]

//   sealed trait EffInstance[F[_], A] extends Instance2[A]
//   final case class Eff[F[_], A](v: F[A], tag: Tag[A]) extends EffInstance[F, A]
//   final case class EffFunc[F[_], In, A](prev: Instance2[In], f: In => Resource[F, A], tag: Tag[A]) extends EffInstance[F, A]

//   final case class Ap[In, A](prev: Instance2[In], f: Instance[In => A], tag: Tag[In => A]) extends Instance2[In => A]

//   // def deriveTree[F[_], A]: InstanceTree[F, A] = ???

// }


// sealed trait Instance[A] {
//   def tag: Tag[A]
// }

// object Instance {

//   final case class Pure[A](v: A, tag: Tag[A]) extends Instance[A]
//   final case class Effect[F[_], A](v: F[A], tag: Tag[A]) extends Instance[A]
//   final case class Map[In, A](prev: Instance[In], f: In => A, tag: Tag[A]) extends Instance[A]
//   final case class Ap[In, A](prev: Instance[In], f: Instance[In => A], tag: Tag[In => A]) extends Instance[In => A]

//   final case class PureK[F[_], A](v: A, tag: Tag[A]) extends Instance[A]

//   def pure[A: Tag](v: A): Instance.Pure[A] = Instance.Pure(v, Tag.of[A])
//   def effect[F[_], A: Tag](v: F[A]): Instance.Effect[F, A] = Instance.Effect(v, Tag.of[A])

// }

// trait Requires[A]
// object Requires {
//   implicit def derive[A]: Requires[A] = macro RequiresMacro.materialize[A]
// }

// class RequiresMacro(val c: Context) {
//   import c.universe._

//   private val intanceTC = typeOf[Instance[_]].typeConstructor

//   def materialize[A: c.WeakTypeTag]: c.Expr[Requires[A]] = {
//     val tpe = weakTypeOf[A]
//     println(tpe.getClass())
//     val out = resolve(tpe).orElse(tryTuple(tpe))

//     out match {
//       case Some(tree) => c.Expr[Requires[A]](q"new Requires[$tpe]{}")
//       case None =>
//         c.error(c.enclosingPosition, s"Can't find `Requires` for $tpe")
//         c.Expr[Requires[A]](EmptyTree)
//     }
//   }

//   private val tupleRegex = "scala.Tuple(\\d+)".r


//   private def tryTuple(tpe: Type): Option[c.Tree] = {
//     val name = tpe.typeSymbol.fullName
    
//     name match {
//       case tupleRegex(x) =>
//         println(s"sdassa: ${x}")
//         println(tpe.typeArgs)

//         val init = List.empty[String]
//         val errors = tpe.typeArgs.foldLeft(init){
//           case (errAcc, t) =>
//             resolve(t) match {
//               case Some(_) => errAcc
//               case None => s"Can't find Instance for ${t}" :: errAcc
//             }
//         }
//         if (errors.nonEmpty) {
//           val x = errors.mkString("\n")
//           c.error(c.enclosingPosition, x)
//           None
//         } else {
//           Some(EmptyTree)
//         }
//       case _ => None
//     }
//   }

//   private def resolve(tpe: Type): Option[c.Tree] = {
//     c.inferImplicitValue(appliedType(intanceTC, tpe)) match {
//       case EmptyTree => None
//       case tree => Some(tree) 
//     }
//   }
// }


// object XXx2 {

//   case class Args(args: List[String])
//   object Args {
//     implicit def instance(implicit req: Requires[List[String]]): Instance[]
//   }

//   case class Abc(args: Args, out: String)
//   object Abc {
//     implicit val instance: Bind2.Func[Args, Abc] = Bind2.func(Abc(_, "default"))
//   }

//   case class Foo(args: Args)
//   object Foo {
//     implicit val instanceXXX: Bind2.Func[Args, Foo] = Bind2.func(Foo(_))
//     implicit val instance2: Bind2.Func[Int, Foo] = Bind2.func(x => Foo(Args(List(x.toString()))))
//   }

//   case class End(
//     abc: Abc,
//     foo: Foo
//   ) {
//     def what: String = abc.out
//   }

//   object End {
//     implicit val instance: Bind2.Func[(Abc, Foo), End] = Bind2.func({ case (abc, foo) => End(abc, foo)})
//   }


//   def main(args: Array[String]): Unit = {
//     import cats.implicits._
//     import cats.effect.implicits._

//     implicit val initArgs = Bind2.pure(args.toList)

//     implicit val tmp = Bind2.pure(1)
//     // implicit val abcOverride = Bind2.func(Abc(_, "override"))
//     val out = DependencyTree.of[IO, Foo]
//     // val rsc = Instantiate(out)
    
//     // val eff = rsc.use(value => IO.delay(println(s"HAHAHAH: ${value.what}")))
//     // eff.unsafeRunSync()
//   }

// }
