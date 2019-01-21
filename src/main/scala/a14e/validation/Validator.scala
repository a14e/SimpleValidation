package a14e.validation

import a14e.validation.containers.{BuildingValidatorContainer, CheckContainer, OptValidatorContainer, SeqValidatorContainer, ValidationCheck, ValidationContainer, ValidatorContainer}
import a14e.validation.results.ValidationError
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import akka.stream.scaladsl.{Sink, Source}

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.Try
import scala.util.control.NonFatal


object Validator {
  def empty[T, MARKER]: Validator[T, MARKER] = new Validator[T, MARKER] {}
}

trait Validator[T, MARKER] {
  self =>


  def firstFail(x: T)
               (implicit
                executionContext: ExecutionContext): Future[Option[MARKER]] = {

    def recursiveSearch(checks: immutable.Seq[ValidationContainer[T, MARKER]]): Future[Option[MARKER]] = {
      checks match {
        case v +: tail =>
          v.firstFail(x).flatMap {
            case res@Some(_) => Future.successful(res)
            case _ => recursiveSearch(tail)
          }
        case _ => Future.successful(None)
      }
    }

    try {
      recursiveSearch(validations())
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }

  def firstFailWith(x: T)
                   (markersToError: Option[MARKER] => Option[Throwable])
                   (implicit
                    materializer: Materializer,
                    executionContext: ExecutionContext): Future[Unit] = {
    firstFail(x).flatMap { markers =>
      markersToError(markers) match {
        case Some(error) => Future.failed(error)
        case None => Future.unit
      }
    }
  }

  def firstFailWithErr(x: T)
                      (markersToError: Option[MARKER] => Throwable)
                      (implicit
                       materializer: Materializer,
                       executionContext: ExecutionContext): Future[Unit] = {
    firstFailWith(x)(res => Some(markersToError(res)))
  }

  def failWith(x: T,
               parallelLevel: Int = 1)
              (markersToError: immutable.Seq[MARKER] => Option[Throwable])
              (implicit
               materializer: Materializer,
               executionContext: ExecutionContext): Future[Unit] = {
    collectFails(x, parallelLevel)
      .flatMap { markers =>
        markersToError(markers) match {
          case Some(error) => Future.failed(error)
          case None => Future.unit
        }
      }
  }

  def failWithErr(x: T,
                  parallelLevel: Int = 1)
                 (markersToError: immutable.Seq[MARKER] => Throwable)
                 (implicit
                  materializer: Materializer,
                  executionContext: ExecutionContext): Future[Unit] = {
    failWith(x, parallelLevel)(res => Some(markersToError(res)))
  }

  def collectFails(x: T,
                   parallelLevel: Int = 1)
                  (implicit
                   materializer: Materializer,
                   executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    val checksList = validations()

    Source(checksList)
      .mapAsync(parallelLevel)(_.collectFails(x))
      .mapConcat(x => x)
      .runWith(Sink.seq)
  }


  def collectSuccesses(x: T,
                       parallelLevel: Int = 1)
                      (implicit executionContext: ExecutionContext,
                       materializer: Materializer): Future[immutable.Seq[MARKER]] = {

    val checksList = validations()

    Source(checksList)
      .mapAsync(parallelLevel)(_.collectSuccesses(x))
      .mapConcat(x => x)
      .runWith(Sink.seq)
  }

  def firstSuccess(x: T)
                  (implicit executionContext: ExecutionContext): Future[Option[MARKER]] = {

    def recursiveSearch(checks: immutable.Seq[ValidationContainer[T, MARKER]]): Future[Option[MARKER]] = {
      checks match {
        case v +: tail =>
          v.firstSuccess(x).flatMap {
            case res@Some(_) => Future.successful(res)
            case _ => recursiveSearch(tail)
          }
        case _ => Future.successful(None)
      }
    }

    try {
      recursiveSearch(validations())
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }

  protected def ruleAsync(marker: MARKER)
                         (block: T => Future[Boolean]): Unit = {
    checks += CheckContainer(ValidationCheck(marker, block))
  }

  protected def rule(marker: MARKER)
                    (block: T => Boolean): Unit = {
    ruleAsync(marker) { x =>
      try {
        Future.successful(block(x))
      } catch {
        case NonFatal(e) => Future.failed(e)
      }
    }
  }

  protected def register(v: Validator[T, MARKER]): Unit = {
    checks += ValidatorContainer(v)
  }

  protected def registerOnSeq[B](extract: T => immutable.Seq[B])(v: Validator[B, MARKER]): Unit = {
    checks += SeqValidatorContainer[T, B, MARKER](v, extract)
  }

  protected def registerOnOpt[B](extract: T => Option[B])(v: Validator[B, MARKER]): Unit = {
    checks += OptValidatorContainer[T, B, MARKER](v, extract)
  }


  protected def registerOnFunc[B](build: T => Validator[T, MARKER]): Unit = {
    checks += BuildingValidatorContainer[T, MARKER](build)
  }

  protected def registerOn[B](extract: T => B)(builder: PartialFunction[B, Validator[T, MARKER]]): Unit = {
    registerOnFunc { obj =>
      val input = extract(obj)
      builder.applyOrElse(input, (_: B) => Validator.empty[T, MARKER])
    }
  }


  def validations(): immutable.Seq[ValidationContainer[T, MARKER]] = checks.toList

  def contramap[B](f: B => T): Validator[B, MARKER] = {
    new Validator[B, MARKER] {
      checks ++= self.checks.map(_.contramap(f))
    }
  }

  def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): Validator[T, NEW_MARKER] = {
    new Validator[T, NEW_MARKER] {
      checks ++= self.checks.map(_.mapMarkers(f))

    }
  }

  def ++(other: Validator[T, MARKER]): Validator[T, MARKER] = {
    new Validator[T, MARKER] {
      checks ++= self.checks
      checks ++= other.checks

    }

  }

  protected val checks = new ArrayBuffer[ValidationContainer[T, MARKER]]()
}

/** example  */

case class MyUser(age: Int,
                  name: String,
                  phone: String)

class PhoneValidation extends Validator[String, ValidationError] {
  rule("Телефон должен подходить по формату") { phone =>
    phone.matches(PhonePattern)
  }

  private val PhonePattern = """^\+7\d{10}$"""
}

class UserValidation extends Validator[MyUser, ValidationError] {
  rule("Пользователь должен быть моложе 20 лет") { user =>
    user.age < 20
  }

  rule(s"Имя пользователя должно быть короче 20 символов") { user =>
    user.name.length < 20
  }

  register {
    new PhoneValidation()
      .mapMarkers[ValidationError](x => "Ошибка пользователя " + x.text)
      .contramap(_.phone)
  }

  registerOn(_.phone) {
    case "phone123" => new PhoneValidation().contramap(_.phone)
    case "phone1234" => new PhoneValidation().contramap(_.phone)
  }

}

object Test extends App {
  implicit val actorSystem = ActorSystem()
  implicit val materializer = ActorMaterializer()

  import actorSystem.dispatcher

  val user = MyUser(33, "a" * 30, "+7012345789")

  val validator = new UserValidation()

  //  val mockUserValidation = mock[UserValidation]

  val resultErrorsFuture = validator.collectFails(user)
  val resultErrors = Await.result(resultErrorsFuture, Duration.Inf)
  println(resultErrors)

  val resultErrorFuture = validator.failWithErr(user)(errs => new RuntimeException(errs.mkString(", ")))
  val r = Await.ready(resultErrorFuture, Duration.Inf)
  println(r)
  actorSystem.terminate()
}