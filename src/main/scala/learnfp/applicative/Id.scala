package learnfp.applicative

import learnfp.functor.{Id, IdInstance => IdFunctorInstance}
import learnfp.functor.FunctorOps._

object IdInstance {
  import IdFunctorInstance._
  implicit val idApplicativeInstance = new Applicative[Id] {
    override def pure[A](a: A): Id[A] = Id(a)
    override def <*>[A, R](fx: Id[A => R])(a: Id[A]): Id[R] = a.fmap(fx.value)
  }
}
