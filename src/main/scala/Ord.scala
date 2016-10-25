



trait Ord[F] {

  trait Result
  case class GT() extends Result
  case class EQ() extends Result
  case class LT() extends Result

  def compare(a: F)(b:F): Result

  def max(a: F)(b: F): F = compare(a)(b) match {
    case x:LT => b
    case x:GT => a
    case x:EQ => a
  }

  def min(a: F)(b: F) = (max(a)(b) == a) match {
    case true => b
    case _ => a  
  }
}
