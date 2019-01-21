package a14e.validation

import a14e.validation.containers.{AsyncCheckContainer, AsyncValidationCheck, BuildingValidatorContainer, MappingValidatorContainer, OptValidatorContainer, SeqValidatorContainer, SyncCheckContainer, SyncValidationCheck, ValidationContainer, ValidatorContainer}
import a14e.validation.results.ValidationError
import a14e.validation.utils.FutureUtils
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.{Random, Try}
import scala.util.control.NonFatal


object Validator {
  def empty[T, MARKER]: Validator[T, MARKER] = new Validator[T, MARKER] {}
}

trait Validator[T, MARKER] {
  self =>

  type ThisValidator = Validator[T, MARKER]

  def firstFail(x: T)
               (implicit
                executionContext: ExecutionContext): Future[Option[MARKER]] = {

    def recursiveSearch(checks: immutable.Seq[ValidationContainer[T, MARKER]]): Future[Option[MARKER]] = {
      checks match {
        case v +: tail =>
          v.firstFail(x).flatMap {
            case res@Some(_) => Future.successful(res)
            case _ => recursiveSearch(tail)
          }(FutureUtils.sameThreadExecutionContext)
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
                    executionContext: ExecutionContext): Future[Unit] = {
    firstFail(x).flatMap { markers =>
      markersToError(markers) match {
        case Some(error) => Future.failed(error)
        case None => Future.unit
      }
    }(FutureUtils.sameThreadExecutionContext)
  }

  def firstFailWithErr(x: T)
                      (markersToError: Option[MARKER] => Throwable)
                      (implicit
                       executionContext: ExecutionContext): Future[Unit] = {
    firstFailWith(x)(res => Some(markersToError(res)))
  }

  def failWith(x: T,
               parallelLevel: Int = 1)
              (markersToError: immutable.Seq[MARKER] => Option[Throwable])
              (implicit
               executionContext: ExecutionContext): Future[Unit] = {
    collectFails(x, parallelLevel)
      .flatMap { markers =>
        markersToError(markers) match {
          case Some(error) => Future.failed(error)
          case None => Future.unit
        }
      }(FutureUtils.sameThreadExecutionContext)
  }

  def failWithErr(x: T,
                  parallelLevel: Int = 1)
                 (markersToError: immutable.Seq[MARKER] => Throwable)
                 (implicit
                  executionContext: ExecutionContext): Future[Unit] = {
    failWith(x, parallelLevel)(res => Some(markersToError(res)))
  }

  def collectFails(x: T,
                   parallelLevel: Int = 1)
                  (implicit
                   executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    FutureUtils.batched(validations(), parallelLevel)(_.collectFails(x))
      .map(_.flatten)(FutureUtils.sameThreadExecutionContext)
  }


  def collectSuccesses(x: T,
                       parallelLevel: Int = 1)
                      (implicit executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    FutureUtils.batched(validations(), parallelLevel)(_.collectSuccesses(x))
      .map(_.flatten)(FutureUtils.sameThreadExecutionContext)
  }

  def firstSuccess(x: T)
                  (implicit executionContext: ExecutionContext): Future[Option[MARKER]] = {

    def recursiveSearch(checks: immutable.Seq[ValidationContainer[T, MARKER]]): Future[Option[MARKER]] = {
      checks match {
        case v +: tail =>
          v.firstSuccess(x).flatMap {
            case res@Some(_) => Future.successful(res)
            case _ => recursiveSearch(tail)
          }(FutureUtils.sameThreadExecutionContext)
        case _ => Future.successful(None)
      }
    }

    try {
      recursiveSearch(validations())
    } catch {
      case NonFatal(e) => Future.failed(e)
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

  protected def ruleAsync(marker: MARKER)
                         (block: T => Future[Boolean]): Unit = {
    checks += AsyncCheckContainer(AsyncValidationCheck(marker, block))
  }

  protected def rule(marker: MARKER)
                    (block: T => Boolean): Unit = {
    checks += SyncCheckContainer(SyncValidationCheck(marker, block))
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

  protected def registerOnMapping[KEY](extract: T => KEY)(builder: (KEY, Validator[T, MARKER])*): Unit = {
    val mapping = builder.groupBy { case (k, _) => k }
      .mapValues(validators => validators.map { case (_, v) => v }.reduce(_ ++ _))
      .map(identity) // remove lazyness
    checks += MappingValidatorContainer(extract, mapping)
  }

  protected def registerPartial[B](extract: T => B)(builder: PartialFunction[B, Validator[T, MARKER]]): Unit = {
    registerOnFunc { obj =>
      val input = extract(obj)
      builder.applyOrElse(input, (_: B) => Validator.empty[T, MARKER])
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


  registerPartial(_.phone) {
    case "phone123" => new PhoneValidation().contramap(_.phone)
    case "phone1234" => new PhoneValidation().contramap(_.phone)
  }
}

object Test extends App {
  import ExecutionContext.Implicits.global

  def user = MyUser(33 + Random.nextInt(2), "a" * 30, "phone123")

  val validator = new UserValidation()

  //  val mockUserValidation = mock[UserValidation]

  for (_ <- 0 to 40000) {
    val t1 = System.nanoTime()
    val resultErrorsFuture = validator.collectFails(user)
    val resultErrors = Await.result(resultErrorsFuture, Duration.Inf)
    val t2 = System.nanoTime()
    println(s"${(t2 - t1).toDouble / 1000000.0} ms")
    println(resultErrors)
  }

  val resultErrorFuture = validator.failWithErr(user)(errs => new RuntimeException(errs.mkString(", ")))
  val r = Await.ready(resultErrorFuture, Duration.Inf)
  println(r)
//  actorSystem.terminate()
}