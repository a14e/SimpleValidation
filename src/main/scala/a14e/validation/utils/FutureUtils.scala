package a14e.validation.utils

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}


private[validation] object FutureUtils {

  /**
    * executes futures one by one
    **/
  def serially[T, B](xs: immutable.Seq[T])
                    (f: T => Future[B]): Future[immutable.Seq[B]] = {
    implicit val ctx: ExecutionContext = sameThreadExecutionContext

    xs.foldLeft(Future.successful(List.newBuilder[B])) { (prevFuture, x) =>
      for {
        builder <- prevFuture
        elem <- f(x)
      } yield builder += elem
    }.map(_.result())
  }


  def batched[T, B](xs: immutable.Seq[T], batchSize: Int)
                   (f: T => Future[B])
                   (implicit ctx: ExecutionContext): Future[immutable.Seq[B]] = {

    if (batchSize == 1) serially(xs)(f)
    else xs.iterator.grouped(batchSize).foldLeft(Future.successful(List.newBuilder[B])) { (prevFuture, group) =>
      for {
        builder <- prevFuture
        elems <- Future.traverse(group)(f)
      } yield builder ++= elems
    }.map(_.result())(sameThreadExecutionContext)
  }

  val sameThreadExecutionContext: ExecutionContext = new ExecutionContext {
    override def execute(runnable: Runnable): Unit = runnable.run()

    override def reportFailure(cause: Throwable): Unit = ()
  }

}