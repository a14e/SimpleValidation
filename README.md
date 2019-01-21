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


```

