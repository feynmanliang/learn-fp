package learnfp.transformer

import learnfp.functor.Functor
import learnfp.functor.Maybe.{Just, Maybe, Nothing}
import learnfp.monad.{Monad, MonadOps}
import learnfp.functor.FunctorOps._
import learnfp.monad.MonadOps._

case class MaybeT[A, M[_]](runMaybeT:M[Maybe[A]])(implicit f:Functor[M], m:Monad[M])

object MaybeT {
  implicit def maybeTFunctorInstance[M[_]](implicit f:Functor[M], m:Monad[M]) = new Functor[({type E[X] = MaybeT[X, M]})#E] {
    override def fmap[A, B](a: MaybeT[A, M])(fx: A => B): MaybeT[B, M] = MaybeT(
      a.runMaybeT.fmap(learnfp.functor.MaybeInstance.maybeInstance.fmap(_)(fx))
    )
  }

  implicit def maybeTMonadInstance[M[_]](implicit f:Functor[M], m:Monad[M]) = new Monad[({type E[X] = MaybeT[X, M]})#E]() {
    override def pure[A](a: A): MaybeT[A, M] = MaybeT(
      m.pure(a).fmap(Just(_))
    )
    override def flatMap[A, B](a: MaybeT[A, M])(fx: A => MaybeT[B, M]): MaybeT[B, M] = MaybeT(
      a.runMaybeT.flatMap { maybeA =>
        learnfp.functor.MaybeInstance.maybeInstance.fmap(maybeA)(fx) match {
          case Nothing() => m.pure(Nothing())
          case Just(b) => b.runMaybeT
        }
      }
    )
  }

  implicit def maybeTToMonadOps[A, M[_]](a:MaybeT[A, M])(implicit m:Monad[M], f:Functor[M]) =
    new MonadOps[A, ({type E[X] = MaybeT[X, M]})#E](a)

  implicit def maybeTMonadTransInstance[M[_]](implicit f:Functor[M], m:Monad[M]) = new MonadTransformer[M, MaybeT] {
    override def lift[A](a: M[A]): MaybeT[A, M] = MaybeT(
      a.map(Just(_))
    )
  }

  def nothingT[A, M[_]](implicit f:Functor[M], m:Monad[M]):MaybeT[A, M] = MaybeT(
    m.pure(Nothing())
  )

  def lift[A, M[_]](a:M[A])(implicit f:Functor[M], m:Monad[M]):MaybeT[A, M] = maybeTMonadTransInstance(f,m).lift(a)
}
