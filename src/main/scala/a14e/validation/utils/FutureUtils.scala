package a14e.validation.utils

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}


private[validation] object FutureUtils {

  def serially[T, B](xs: immutable.Seq[T])
                    (f: T => Future[B]): Future[immutable.Seq[B]] = {
    implicit val ctx = sameThreadExecutionContext

    xs.foldLeft(Future.successful(List.newBuilder[B])) { (prevFuture, x) =>
      for {
        builder <- prevFuture
        elem <- f(x)
      } yield builder += elem
    }.map(_.result())
  }


  def batched[T, B](xs: immutable.Seq[T], batch: Int)
                   (f: T => Future[B])
                   (implicit ctx: ExecutionContext): Future[immutable.Seq[B]] = {

    if (batch == 1) serially(xs)(f)
    else xs.grouped(batch).foldLeft(Future.successful(List.newBuilder[B])) { (prevFuture, group) =>
      for {
        builder <- prevFuture
        elems <- Future.traverse(group)(f)
      } yield builder ++= elems
    }.map(_.result())
  }

  val sameThreadExecutionContext: ExecutionContext = new ExecutionContext {
    override def execute(runnable: Runnable): Unit = runnable.run()

    override def reportFailure(cause: Throwable): Unit = ()
  }

}