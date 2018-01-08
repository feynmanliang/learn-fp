package learnfp.traversable

import learnfp.applicative.Applicative
import learnfp.functor.Functor

class TraversableOps[A, F[_]](initialXs:List[F[A]])(implicit functor: Functor[F]) {
  import learnfp.functor.FunctorOps._
  import learnfp.applicative.ApplicativeOps._

  def traverse[B](fx:A => B)(implicit applicative: Applicative[F]):F[List[B]] =
    initialXs.foldRight(
      applicative.pure(List.empty[B])
    ) { (appx, acc) =>
      (((b:B) => (bs:List[B]) => b::bs) `<$>` (fx `<$>` appx)) <*> acc
    }
  def sequence(implicit applicative: Applicative[F]):F[List[A]] = traverse(x => x)
}

object TraversableOps {
  implicit def toTraversableOps[A, F[_]](xs:List[F[A]])(implicit functor:Functor[F]) = new TraversableOps[A, F](xs)
}
