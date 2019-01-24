WIP



```scala
import a14e.validation.containers._
import a14e.validation.results._
import a14e.validation.Validator
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/** example  */

case class Animal(sound: String,
                  `type`: String)

class CatValidator extends Validator[String, ValidationError] {
  rule("bad cat") { str => str == "meow" }
}

class DogValidator extends Validator[String, ValidationError] {
  ruleAsync("bad dog") { str => Future(str == "woof") }
}

class TestValidator extends Validator[Animal, ValidationError] {

  rule("bad type") { animal => animal.`type` == "cat" || animal.`type` == "dog" }

  registerOnFunc {
    case Animal(_, "cat") => new CatValidator().contramap(_.sound)
    case Animal(_, "dog") => new DogValidator().contramap(_.sound)
  }
}

new TestValidator().firstFail(Animal("meow", "cat")).futureValue shouldBe None
new TestValidator().firstFail(Animal("woof", "dog")).futureValue shouldBe None


new TestValidator().firstFail(Animal("woof", "snake")).futureValue shouldBe Some(ValidationError("bad type"))

new TestValidator().firstFail(Animal("meow", "dog")).futureValue shouldBe Some(ValidationError("bad dog"))
new TestValidator().firstFail(Animal("woof", "cat")).futureValue shouldBe Some(ValidationError("bad cat"))


```

