package learnfp.applicative

import learnfp.functor.Disjunction._
import learnfp.functor.DisjunctionInstance._
import learnfp.monad.DisjunctionInstance._

object DisjunctionInstance {
  implicit def disjunctionInstance[L] = new Applicative[({type E[X] = Disjunction[L, X]})#E]() {
    override def pure[A](a: A): Disjunction[L, A] = pure(a)
    override def <*>[A, R](dfx: Disjunction[L, A => R])(da: Disjunction[L, A]): Disjunction[L, R] = (dfx, da) match {
      case (LeftDisjunction(lv), _) => LeftDisjunction(lv)
      case (_, LeftDisjunction(lv)) => LeftDisjunction(lv)
      case (RightDisjunction(f), RightDisjunction(av)) => RightDisjunction(f(av))
    }
  }

  implicit def disjunctionToApplicativeOps[L, A, R](fx:Disjunction[L, A => R])(
    implicit applicative:Applicative[({type E[X] = Disjunction[L, X]})#E]) =
    new FxApplicativeOps[A, R, ({type E[X] = Disjunction[L, X]})#E](fx)
}
