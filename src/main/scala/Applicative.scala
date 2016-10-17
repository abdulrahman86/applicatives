import Functor._

trait Applicative[F[_]] extends Functor[F]{

  def apply[A, B] : F[A => B] => F[A] => F[B]

  def unit[A] : (=> A) => F[A]

  def map[A, B] : F[A] => (A => B) => F[B] =
    fa => f => apply(unit(f))(fa)

  def map2[A, B, C] : F[A] => F[B] => ((A, B) => C) => F[C] =
    fa => fb => f => apply(apply(unit(f curried))(fa))(fb)

  def sequence[A] : List[F[A]] => F[List[A]] = l => traverse(l)(x => x)

  def traverse[A, B] : List[A] => (A => F[B]) => F[List[B]] =  {
    l => f => l.foldRight[F[List[B]]](unit(List()))((a, b) => map2(f(a))(b)(_ :: _))
  }

  def replicateM[A] : Int => F[A] => F[List[A]] = n => fa => sequence(List.fill(n)(fa))

  def product[A, B] : F[A] => F[B] => F[(A, B)] = fa => fb => map2(fa)(fb)((_ , _))
}
