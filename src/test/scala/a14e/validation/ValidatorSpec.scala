package a14e.validation

import a14e.utils.DefaultSpec
import a14e.validation.results.ValidationError

import scala.collection.immutable
import scala.concurrent.Future

class ValidatorSpec extends DefaultSpec {


  "firstFail" should "work none rule" in new Wiring {

    class TestValidator extends Validator[String, String] {

    }

    new TestValidator().firstFail("234").futureValue shouldBe None

  }

  it should "work single rule" in new Wiring {

    class TestValidator extends Validator[String, ValidationError] {
      rule("error") { str => str != "123" }
    }

    new TestValidator().firstFail("234").futureValue shouldBe None

    new TestValidator().firstFail("123").futureValue shouldBe Some(ValidationError("error"))

  }

  it should "work multiple rules" in new Wiring {

    class TestValidator extends Validator[String, ValidationError] {
      rule("error") { str => str != "123" }
      rule("error1") { str => str != "345" }
    }

    new TestValidator().firstFail("234").futureValue shouldBe None

    new TestValidator().firstFail("123").futureValue shouldBe Some(ValidationError("error"))

    new TestValidator().firstFail("345").futureValue shouldBe Some(ValidationError("error1"))
  }

  it should "work with async rule" in new Wiring {

    class TestValidator extends Validator[String, ValidationError] {
      ruleAsync("error") { str =>
        Future(str != "123")
      }
    }

    new TestValidator().firstFail("234").futureValue shouldBe None

    new TestValidator().firstFail("123").futureValue shouldBe Some(ValidationError("error"))

  }

  it should "work with multiple async rules" in new Wiring {

    class TestValidator extends Validator[String, ValidationError] {
      ruleAsync("error") { str =>
        Future(str != "123")
      }

      ruleAsync("error1") { str =>
        Future(str != "345")
      }
    }

    new TestValidator().firstFail("234").futureValue shouldBe None

    new TestValidator().firstFail("123").futureValue shouldBe Some(ValidationError("error"))

    new TestValidator().firstFail("345").futureValue shouldBe Some(ValidationError("error1"))
  }

  it should "work with nested Validators" in new Wiring {

    case class User(name: String)

    class InnerValidator extends Validator[String, ValidationError] {
      rule("error") { str => str != "123" }
      ruleAsync("error1") { str => Future(str != "345") }
    }

    class TestValidator extends Validator[User, ValidationError] {
      register {
        new InnerValidator().contramap(_.name)
      }
    }

    new TestValidator().firstFail(User("234")).futureValue shouldBe None

    new TestValidator().firstFail(User("123")).futureValue shouldBe Some(ValidationError("error"))

    new TestValidator().firstFail(User("345")).futureValue shouldBe Some(ValidationError("error1"))
  }

  it should "work with seq validators" in new Wiring {

    case class Tags(tags: immutable.Seq[String])

    class InnerValidator extends Validator[String, ValidationError] {
      rule("error") { str => str != "123" }
      ruleAsync("error1") { str => Future(str != "345") }
    }

    class TestValidator extends Validator[Tags, ValidationError] {
      registerOnSeq(_.tags) {
        new InnerValidator()
      }
    }

    new TestValidator().firstFail(Tags("a" :: "234" :: Nil)).futureValue shouldBe None

    new TestValidator().firstFail(Tags("a" :: "123" :: Nil)).futureValue shouldBe Some(ValidationError("error"))

    new TestValidator().firstFail(Tags("a" :: "345" :: Nil)).futureValue shouldBe Some(ValidationError("error1"))

    new TestValidator().firstFail(Tags("345" :: "234" :: Nil)).futureValue shouldBe Some(ValidationError("error1"))
  }


  it should "work with option validators" in new Wiring {

    case class User(name: Option[String])

    class InnerValidator extends Validator[String, ValidationError] {
      rule("error") { str => str != "123" }
      ruleAsync("error1") { str => Future(str != "345") }
    }

    class TestValidator extends Validator[User, ValidationError] {
      registerOnOpt(_.name) {
        new InnerValidator()
      }
    }

    new TestValidator().firstFail(User(None)).futureValue shouldBe None

    new TestValidator().firstFail(User(Some("234"))).futureValue shouldBe None

    new TestValidator().firstFail(User(Some("123"))).futureValue shouldBe Some(ValidationError("error"))

    new TestValidator().firstFail(User(Some("345"))).futureValue shouldBe Some(ValidationError("error1"))
  }


  it should "work with function validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, ValidationError] {
      rule("bad cat") { str => str == "meow" }
    }

    class DogValidator extends Validator[String, ValidationError] {
      ruleAsync("bad dog") { str => Future(str == "woof") }
    }

    class TestValidator extends Validator[Animal, ValidationError] {
      registerOnFunc {
        case Animal(_, "cat") => new CatValidator().contramap(_.sound)
        case Animal(_, "dog") => new DogValidator().contramap(_.sound)
      }
    }

    new TestValidator().firstFail(Animal("meow", "cat")).futureValue shouldBe None
    new TestValidator().firstFail(Animal("woof", "dog")).futureValue shouldBe None

    new TestValidator().firstFail(Animal("meow", "dog")).futureValue shouldBe Some(ValidationError("bad dog"))
    new TestValidator().firstFail(Animal("woof", "cat")).futureValue shouldBe Some(ValidationError("bad cat"))

  }


  it should "work with mapping validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, ValidationError] {
      rule("bad cat") { str => str == "meow" }
    }

    class DogValidator extends Validator[String, ValidationError] {
      ruleAsync("bad dog") { str => Future(str == "woof") }
    }

    class TestValidator extends Validator[Animal, ValidationError] {
      registerOnMapping(_.`type`)(
        "cat" -> new CatValidator().contramap(_.sound),
        "dog" -> new DogValidator().contramap(_.sound)
      )
    }

    new TestValidator().firstFail(Animal("meow", "cat")).futureValue shouldBe None
    new TestValidator().firstFail(Animal("woof", "dog")).futureValue shouldBe None

    new TestValidator().firstFail(Animal("meow", "dog")).futureValue shouldBe Some(ValidationError("bad dog"))
    new TestValidator().firstFail(Animal("woof", "cat")).futureValue shouldBe Some(ValidationError("bad cat"))

  }


  it should "work with partial validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, ValidationError] {
      rule("bad cat") { str => str == "meow" }
    }

    class DogValidator extends Validator[String, ValidationError] {
      ruleAsync("bad dog") { str => Future(str == "woof") }
    }

    class TestValidator extends Validator[Animal, ValidationError] {
      registerPartial(_.`type`) {
        case "cat" => new CatValidator().contramap(_.sound)
        case "dog" => new DogValidator().contramap(_.sound)
      }
    }

    new TestValidator().firstFail(Animal("meow", "cat")).futureValue shouldBe None
    new TestValidator().firstFail(Animal("woof", "dog")).futureValue shouldBe None

    new TestValidator().firstFail(Animal("meow", "dog")).futureValue shouldBe Some(ValidationError("bad dog"))
    new TestValidator().firstFail(Animal("woof", "cat")).futureValue shouldBe Some(ValidationError("bad cat"))

  }

  "collectFails" should "work none rule" in new Wiring {

  }

  it should "work single rule" in new Wiring {

  }

  it should "work multiple rules" in new Wiring {

  }

  it should "work with async rule" in new Wiring {

  }

  it should "work with multiple async rules" in new Wiring {

  }

  it should "work with nested Validators" in new Wiring {

  }

  it should "work with seq validators" in new Wiring {

  }


  it should "work with option validators" in new Wiring {

  }


  it should "work with function validators" in new Wiring {

  }


  it should "work with mapping validators" in new Wiring {

  }


  it should "work with partial validators" in new Wiring {

  }


  "collectSuccesses" should "work none rule" in new Wiring {

  }

  it should "work single rule" in new Wiring {

  }

  it should "work multiple rules" in new Wiring {

  }

  it should "work with async rule" in new Wiring {

  }

  it should "work with multiple async rules" in new Wiring {

  }

  it should "work with nested Validators" in new Wiring {

  }

  it should "work with seq validators" in new Wiring {

  }


  it should "work with option validators" in new Wiring {

  }


  it should "work with function validators" in new Wiring {

  }


  it should "work with mapping validators" in new Wiring {

  }


  it should "work with partial validators" in new Wiring {

  }


  "firstSuccess"  should "work none rule" in new Wiring {

    class TestValidator extends Validator[String, String] {

    }

    new TestValidator().firstSuccess("234").futureValue shouldBe None

  }

  it should "work single rule" in new Wiring {

    class TestValidator extends Validator[String, ValidationError] {
      rule("error") { str => str == "123" }
    }

    new TestValidator().firstSuccess("234").futureValue shouldBe None

    new TestValidator().firstSuccess("123").futureValue shouldBe Some(ValidationError("error"))

  }

  it should "work multiple rules" in new Wiring {

    class TestValidator extends Validator[String, ValidationError] {
      rule("error") { str => str == "123" }
      rule("error1") { str => str == "345" }
    }

    new TestValidator().firstSuccess("234").futureValue shouldBe None

    new TestValidator().firstSuccess("123").futureValue shouldBe Some(ValidationError("error"))

    new TestValidator().firstSuccess("345").futureValue shouldBe Some(ValidationError("error1"))
  }

  it should "work with async rule" in new Wiring {

    class TestValidator extends Validator[String, ValidationError] {
      ruleAsync("error") { str =>
        Future(str == "123")
      }
    }

    new TestValidator().firstSuccess("234").futureValue shouldBe None

    new TestValidator().firstSuccess("123").futureValue shouldBe Some(ValidationError("error"))

  }

  it should "work with multiple async rules" in new Wiring {

    class TestValidator extends Validator[String, ValidationError] {
      ruleAsync("error") { str =>
        Future(str == "123")
      }

      ruleAsync("error1") { str =>
        Future(str == "345")
      }
    }

    new TestValidator().firstSuccess("234").futureValue shouldBe None

    new TestValidator().firstSuccess("123").futureValue shouldBe Some(ValidationError("error"))

    new TestValidator().firstSuccess("345").futureValue shouldBe Some(ValidationError("error1"))
  }

  it should "work with nested Validators" in new Wiring {

    case class User(name: String)

    class InnerValidator extends Validator[String, ValidationError] {
      rule("error") { str => str == "123" }
      ruleAsync("error1") { str => Future(str == "345") }
    }

    class TestValidator extends Validator[User, ValidationError] {
      register {
        new InnerValidator().contramap(_.name)
      }
    }

    new TestValidator().firstSuccess(User("234")).futureValue shouldBe None

    new TestValidator().firstSuccess(User("123")).futureValue shouldBe Some(ValidationError("error"))

    new TestValidator().firstSuccess(User("345")).futureValue shouldBe Some(ValidationError("error1"))
  }

  it should "work with seq validators" in new Wiring {

    case class Tags(tags: immutable.Seq[String])

    class InnerValidator extends Validator[String, ValidationError] {
      rule("error") { str => str == "123" }
      ruleAsync("error1") { str => Future(str == "345") }
    }

    class TestValidator extends Validator[Tags, ValidationError] {
      registerOnSeq(_.tags) {
        new InnerValidator()
      }
    }

    new TestValidator().firstSuccess(Tags("a" :: "234" :: Nil)).futureValue shouldBe None

    new TestValidator().firstSuccess(Tags("a" :: "123" :: Nil)).futureValue shouldBe Some(ValidationError("error"))

    new TestValidator().firstSuccess(Tags("a" :: "345" :: Nil)).futureValue shouldBe Some(ValidationError("error1"))

    new TestValidator().firstSuccess(Tags("345" :: "234" :: Nil)).futureValue shouldBe Some(ValidationError("error1"))
  }


  it should "work with option validators" in new Wiring {

    case class User(name: Option[String])

    class InnerValidator extends Validator[String, ValidationError] {
      rule("error") { str => str == "123" }
      ruleAsync("error1") { str => Future(str == "345") }
    }

    class TestValidator extends Validator[User, ValidationError] {
      registerOnOpt(_.name) {
        new InnerValidator()
      }
    }

    new TestValidator().firstSuccess(User(None)).futureValue shouldBe None

    new TestValidator().firstSuccess(User(Some("234"))).futureValue shouldBe None

    new TestValidator().firstSuccess(User(Some("123"))).futureValue shouldBe Some(ValidationError("error"))

    new TestValidator().firstSuccess(User(Some("345"))).futureValue shouldBe Some(ValidationError("error1"))
  }


  it should "work with function validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, ValidationError] {
      rule("bad cat") { str => str != "meow" }
    }

    class DogValidator extends Validator[String, ValidationError] {
      ruleAsync("bad dog") { str => Future(str != "woof") }
    }

    class TestValidator extends Validator[Animal, ValidationError] {
      registerOnFunc {
        case Animal(_, "cat") => new CatValidator().contramap(_.sound)
        case Animal(_, "dog") => new DogValidator().contramap(_.sound)
      }
    }

    new TestValidator().firstSuccess(Animal("meow", "cat")).futureValue shouldBe None
    new TestValidator().firstSuccess(Animal("woof", "dog")).futureValue shouldBe None

    new TestValidator().firstSuccess(Animal("meow", "dog")).futureValue shouldBe Some(ValidationError("bad dog"))
    new TestValidator().firstSuccess(Animal("woof", "cat")).futureValue shouldBe Some(ValidationError("bad cat"))

  }


  it should "work with mapping validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, ValidationError] {
      rule("bad cat") { str => str != "meow" }
    }

    class DogValidator extends Validator[String, ValidationError] {
      ruleAsync("bad dog") { str => Future(str != "woof") }
    }

    class TestValidator extends Validator[Animal, ValidationError] {
      registerOnMapping(_.`type`)(
        "cat" -> new CatValidator().contramap(_.sound),
        "dog" -> new DogValidator().contramap(_.sound)
      )
    }

    new TestValidator().firstSuccess(Animal("meow", "cat")).futureValue shouldBe None
    new TestValidator().firstSuccess(Animal("woof", "dog")).futureValue shouldBe None

    new TestValidator().firstSuccess(Animal("meow", "dog")).futureValue shouldBe Some(ValidationError("bad dog"))
    new TestValidator().firstSuccess(Animal("woof", "cat")).futureValue shouldBe Some(ValidationError("bad cat"))

  }


  it should "work with partial validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, ValidationError] {
      rule("bad cat") { str => str != "meow" }
    }

    class DogValidator extends Validator[String, ValidationError] {
      ruleAsync("bad dog") { str => Future(str != "woof") }
    }

    class TestValidator extends Validator[Animal, ValidationError] {
      registerPartial(_.`type`) {
        case "cat" => new CatValidator().contramap(_.sound)
        case "dog" => new DogValidator().contramap(_.sound)
      }
    }

    new TestValidator().firstSuccess(Animal("meow", "cat")).futureValue shouldBe None
    new TestValidator().firstSuccess(Animal("woof", "dog")).futureValue shouldBe None

    new TestValidator().firstSuccess(Animal("meow", "dog")).futureValue shouldBe Some(ValidationError("bad dog"))
    new TestValidator().firstSuccess(Animal("woof", "cat")).futureValue shouldBe Some(ValidationError("bad cat"))

  }



  "++" should "sum validators" in new Wiring {

  }


  trait Wiring extends ConcurrentWirings

}
