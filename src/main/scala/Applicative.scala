import Functor._

trait Applicative[F[_]] extends Functor[F]{

  def apply[A, B] : F[A => B] => F[A] => F[B]

  def unit[A] : (=> A) => F[A]

  def map[A, B] : F[A] => (A => B) => F[B] =
    fa => f => apply(unit(f))(fa)

  def map2[A, B, C] : F[A] => F[B] => ((A, B) => C) => F[C] =
    fa => fb => f => apply(apply(unit(f curried))(fa))(fb)

  def map3[A, B, C, D] : F[A] => F[B] => F[C] => ((A, B, C) => D ) => F[D] =
    fa => fb => fc => f => apply(apply(map(fa)(f curried))(fb))(fc)

  def map4[A, B, C, D, E] : F[A] => F[B] => F[C] => F[D] => ((A, B, C, D) => E ) => F[E] =
    fa => fb => fc => fd => f => apply(apply(apply(map(fa)(f curried))(fb))(fc))(fd)

  def sequence[A] : List[F[A]] => F[List[A]] = l => traverse(l)(x => x)

  def traverse[A, B] : List[A] => (A => F[B]) => F[List[B]] =  {
    l => f => l.foldRight[F[List[B]]](unit(List()))((a, b) => map2(f(a))(b)(_ :: _))
  }

  def replicateM[A] : Int => F[A] => F[List[A]] = n => fa => sequence(List.fill(n)(fa))

  def product[A, B] : F[A] => F[B] => F[(A, B)] = fa => fb => map2(fa)(fb)((_ , _))

  def product[G[_]](implicit G: Applicative[G]) : Applicative[({type f[X] = (F[X], G[X])})#f] = {

    val self = this

    new Applicative[({type f[X] = (F[X], G[X])})#f] {

      override def apply[A, B]: ((F[(A) => B], G[(A) => B])) => ((F[A], G[A])) => ((F[B], G[B])) =
        (x) => (y) => ((self.apply(x._1)(y._1), G.apply(x._2)(y._2)))

      override def unit[A]: (=> A) => (F[A], G[A]) = a => (self.unit(a), G.unit(a))
    }
  }

  def compose[G[_]](G: Applicative[G]) : Applicative[({type f[X]= F[G[X]]})#f] = {

    val self = this

    new Applicative[({type f[X] = F[G[X]]})#f] {

      override def apply[A, B]: (F[G[(A) => B]]) => (F[G[A]]) => F[G[B]] =
        fgab => fga => self.map2(fgab)(fga)(G.apply(_)(_))

      override def unit[A]: ( =>A) => F[G[A]] = a => self.unit(G.unit(a))
    }
  }

  def sequenceMap[K, V](m: Map[K, F[V]]): F[Map[K, V]] =
    m.foldLeft(unit(Map[K,V]()))((fm, tup) => {
    this.map2(fm)(tup._2)((m, v) => m + (tup._1 -> v))
  })

  def liftMonoid[O](implicit monoidO: Monoid[O]): Monoid[F[O]] = new Monoid[F[O]] {

    override def op: (F[O], F[O]) => F[O] = (fo1, fo2) => map2(fo1)(fo2)(monoidO.op(_, _))

    override def zero: F[O] = unit(monoidO.zero)
  }
}

object Applicative {

  implicit def function1Applicative[IN] = new Applicative[({type f[OUT]= Function1[IN, OUT]})#f] {

    override def apply[A, B]: ((IN) => (A) => B) => ((IN) => A) => (IN) => B =
      inab => ina => {
        in => (inab)(in)(ina(in))
      }

    override def unit[A]: (=> A) => (IN) => A = a => _ => a
  }

  implicit object StreamApplicative extends Applicative[Stream]{

      override def apply[A, B]: (Stream[(A) => B]) => (Stream[A]) => Stream[B] =
        sab => sa => Stream.apply(sab)(sa)

      override def unit[A]: (=> A) => Stream[A] = a => Stream.constant(a)
    }

  implicit object OptionApplication extends Applicative[Option] {

    override def apply[A, B]: (Option[(A) => B]) => (Option[A]) => Option[B] = oab => oa => (oab, oa) match {
      case (Some(f), Some(a)) => Some(f(a))
      case (_ , _) => None
    }

    override def unit[A]: (=> A) => Option[A] = a => Some(a)
  }
}

sealed trait Validation[+E, + A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](value: A) extends Validation[Nothing, A]


object Validation{

  def validationApplicative[E] = new Applicative[({type f[X] = Validation[E, X]})#f] {

    override def apply[A, B]: (Validation[E, (A) => B]) => (Validation[E, A]) => Validation[E, B] =
      f => va => (f, va) match {
        case (Failure(e1, t1), Failure(e2, t2)) => Failure(e1, t1 ++ Vector(e2) ++ t2)
        case (_, x: Failure[E]) => x
        case (x: Failure[E], _) => x
        case (a: Success[A => B], b: Success[A]) => Success(a.value(b.value))

      }

    override def unit[A]: (=>A) => Validation[E, A] = a => Success(a)
  }

}

trait Pointed[+A]
object Bottom extends Pointed[Nothing]
case class Embedded[A](x: A) extends Pointed[A]

object Pointed{

  implicit def PointedMonoid[A](implicit ordA: Ord[A]) = new Monoid[Pointed[A]] {

    override def op: (Pointed[A], Pointed[A]) => Pointed[A] = {
      case (Bottom, Bottom) => Bottom
      case (Bottom, x) => x
      case (x, Bottom) => x
      case (Embedded(a), Embedded(b)) => Embedded(ordA.max(a)(b))
    }

    override def zero: Pointed[A] = Bottom
  }
}

case class Mighty(x: Boolean)

object Mighty{

  implicit object MightyMonoid extends Monoid[Mighty] {

    override def op: (Mighty, Mighty) => Mighty = (a1, a2) => Mighty(a1.x || a2.x)

    override def zero: Mighty = Mighty(false)
  }
}

case class Musty(x: Boolean)

object Musty{

  implicit object MustyMonoid extends Monoid[Musty] {

    override def op: (Musty, Musty) => Musty = (a, b) => Musty(a.x && b.x)

    override def zero: Musty = Musty(true)
  }
}

case class Acc[O, A](value: O)

object Acc {

  implicit def accApplicative[O: Monoid] = new Applicative[({type f[X] = Acc[O, X]})#f] {

    override def apply[A, B]: (Acc[O, (A) => B]) => (Acc[O, A]) => Acc[O, B] = acc1 => acc2 => {
      Acc(implicitly[Monoid[O]].op(acc1.value, acc2.value))
    }

    override def unit[A]: (=> A) => Acc[O, A] = _ => Acc(implicitly[Monoid[O]].zero)
  }
}

trait Traversable[F[_]] extends Functor[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B])(implicit F:Functor[F]) =
    sequence(F.map(fa)(f))
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]]
}

case class Tree[A](head: A, tail: List[Tree[A]])

object Traversable {

  import Acc._

  implicit object OptionTraversable extends Traversable[Option] {
    override def map[A, B]: (Option[A]) => ((A) => B) => Option[B] = ???

    override def sequence[G[_] : Applicative, A](fga: Option[G[A]]): G[Option[A]] = fga match {
      case Some(x) => implicitly[Applicative[G]].map(x)(Some(_))
      case None => implicitly[Applicative[G]].unit(None)
    }
  }

  implicit object ListTraversable extends Traversable[List] {

    override def sequence[G[_] : Applicative, A](fga: List[G[A]]): G[List[A]] =
      fga.foldLeft[G[List[A]]](implicitly[Applicative[G]].unit[List[A]](List()))((x, y) => {
        implicitly[Applicative[G]].map2(x)(y)(_ ++ List(_))
      })


    override def map[A, B]: (List[A]) => ((A) => B) => List[B] = implicitly[Functor[List]].map

  }

  case class Tree[A](head: A, tail: List[Tree[A]])
  object Tree{

    implicit object TreeFunctor extends Functor[Tree] {


      override def map[A, B]: (Tree[A]) => ((A) => B) => Tree[B] = t => f => {
        Tree(f(t.head), t.tail.map(map(_)(f)))
      }
    }
  }

  implicit object TreeTraversable extends Traversable[Tree] {
    override def sequence[G[_] : Applicative, A](fga: Tree[G[A]]): G[Tree[A]] =
      fga.tail.foldLeft(implicitly[Applicative[G]].map(fga.head)(Tree(_, List())))((x, y) => {
        implicitly[Applicative[G]].map2(x)(sequence(y))((x, y) => x match {
          case Tree(x, l) => Tree(x, l ++ List(y))
        })
      })

    override def map[A, B]: (Tree[A]) => ((A) => B) => Tree[B] = implicitly[Functor[Tree]].map
  }

  //icrush operator in "Functional Programming with Effects" paper
  def accumulate[T[_]: Traversable, O: Monoid, A]: (A => O) => T[A] => O =
    fao => ta =>
      (implicitly[Traversable[T]].traverse[({type f[X] = Acc[O, X]})#f, A, O](ta)(a => Acc(fao(a)))).value

  //isum operator in "Functional Programming with Effects" paper
  def reduce[T[_]: Traversable, O: Monoid]: T[O] => O = { to =>
    // This forces the compiler to pass in the correct implicits
    val accumulate_ = accumulate[T, O, O]
    accumulate_(a => a)(to)
  }

  //find if predicate is true for any value in the traversable structure
  def any[X, T[_]](f: X => Boolean)(in: T[X])(implicit trav: Traversable[T]): Boolean = {
    val accumulate_ = accumulate[T, Mighty, X]
    accumulate_(x => Mighty(f(x)))(in).x
  }

  //find if predicate is true for all values in the traversable structure
  def all[X, T[_]](f: X => Boolean)(in: T[X])(implicit trav: Traversable[T]): Boolean = {
    val accumulate_ = accumulate[T, Musty, X]
    accumulate_(x => Musty(f(x)))(in).x
  }

  def max[X, T[_]](in: T[X])(implicit trav: Traversable[T], ord:Ord[X]): Option[X] = {
    val accumulate_ = accumulate[T, Pointed[X], X]
    accumulate_(x => Embedded[X](x))(in) match {
      case Embedded(x)  => Some(x)
      case Bottom => None
    }

  }
}
