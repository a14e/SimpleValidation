WIP



```scala
import a14e.validation.containers._
import a14e.validation.results._
import a14e.validation.Validator
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/** example  */

case class MyUser(age: Int,
                  name: String,
                  phone: String)

class PhoneValidation extends Validator[String, ValidationError] {
  rule("Phone should matches format") { phone =>
    phone.matches(PhonePattern)
  }

  private val PhonePattern = """^\+7\d{10}$"""
}

class UserValidation extends Validator[MyUser, ValidationError] {
  rule("User should be younger then 20 years") { user: MyUser =>
    user.age < 20
  }

  rule(s"User name should have length less then 20") { user: MyUser =>
    user.name.length < 20
  }

  register {
    new PhoneValidation()
      .mapMarkers[ValidationError](x => "User validation error: " + x.text)
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


  val resultErrorsFuture = validator.collectFails(user)
  val resultErrors = Await.result(resultErrorsFuture, Duration.Inf)
  println(resultErrors)

  val resultErrorFuture = validator.failWithErr(user)(errs => new RuntimeException(errs.mkString(", ")))
  val r = Await.ready(resultErrorFuture, Duration.Inf)
  println(r)
  actorSystem.terminate()
}


```

