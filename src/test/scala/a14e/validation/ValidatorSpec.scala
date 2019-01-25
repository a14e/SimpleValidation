package a14e.validation

import a14e.utils.DefaultSpec
import a14e.validation.results.TextResult

import scala.collection.immutable
import scala.concurrent.Future

class ValidatorSpec extends DefaultSpec {


  "firstFail" should "work none rule" in new Wiring {

    class TestValidator extends Validator[String, String] {

    }

    new TestValidator().firstFail("234").futureValue shouldBe None

  }

  it should "work single rule" in new Wiring {

    class TestValidator extends Validator[String, TextResult] {
      rule("error") { str => str != "123" }
    }

    new TestValidator().firstFail("234").futureValue shouldBe None

    new TestValidator().firstFail("123").futureValue shouldBe Some(TextResult("error"))

  }

  it should "work multiple rules" in new Wiring {

    class TestValidator extends Validator[String, TextResult] {
      rule("error") { str => str != "123" }
      rule("error1") { str => str != "345" }
    }

    new TestValidator().firstFail("234").futureValue shouldBe None

    new TestValidator().firstFail("123").futureValue shouldBe Some(TextResult("error"))

    new TestValidator().firstFail("345").futureValue shouldBe Some(TextResult("error1"))
  }

  it should "work with async rule" in new Wiring {

    class TestValidator extends Validator[String, TextResult] {
      ruleAsync("error") { str =>
        Future(str != "123")
      }
    }

    new TestValidator().firstFail("234").futureValue shouldBe None

    new TestValidator().firstFail("123").futureValue shouldBe Some(TextResult("error"))

  }

  it should "work with multiple async rules" in new Wiring {

    class TestValidator extends Validator[String, TextResult] {
      ruleAsync("error") { str =>
        Future(str != "123")
      }

      ruleAsync("error1") { str =>
        Future(str != "345")
      }
    }

    new TestValidator().firstFail("234").futureValue shouldBe None

    new TestValidator().firstFail("123").futureValue shouldBe Some(TextResult("error"))

    new TestValidator().firstFail("345").futureValue shouldBe Some(TextResult("error1"))
  }

  it should "work with nested Validators" in new Wiring {

    case class User(name: String)

    class InnerValidator extends Validator[String, TextResult] {
      rule("error") { str => str != "123" }
      ruleAsync("error1") { str => Future(str != "345") }
    }

    class TestValidator extends Validator[User, TextResult] {
      register {
        new InnerValidator().contramap(_.name)
      }
    }

    new TestValidator().firstFail(User("234")).futureValue shouldBe None

    new TestValidator().firstFail(User("123")).futureValue shouldBe Some(TextResult("error"))

    new TestValidator().firstFail(User("345")).futureValue shouldBe Some(TextResult("error1"))
  }

  it should "work with seq validators" in new Wiring {

    case class Tags(tags: immutable.Seq[String])

    class InnerValidator extends Validator[String, TextResult] {
      rule("error") { str => str != "123" }
      ruleAsync("error1") { str => Future(str != "345") }
    }

    class TestValidator extends Validator[Tags, TextResult] {
      registerOnSeq(_.tags) {
        new InnerValidator()
      }
    }

    new TestValidator().firstFail(Tags("a" :: "234" :: Nil)).futureValue shouldBe None

    new TestValidator().firstFail(Tags("a" :: "123" :: Nil)).futureValue shouldBe Some(TextResult("error"))

    new TestValidator().firstFail(Tags("a" :: "345" :: Nil)).futureValue shouldBe Some(TextResult("error1"))

    new TestValidator().firstFail(Tags("345" :: "234" :: Nil)).futureValue shouldBe Some(TextResult("error1"))
  }


  it should "work with option validators" in new Wiring {

    case class User(name: Option[String])

    class InnerValidator extends Validator[String, TextResult] {
      rule("error") { str => str != "123" }
      ruleAsync("error1") { str => Future(str != "345") }
    }

    class TestValidator extends Validator[User, TextResult] {
      registerOnOpt(_.name) {
        new InnerValidator()
      }
    }

    new TestValidator().firstFail(User(None)).futureValue shouldBe None

    new TestValidator().firstFail(User(Some("234"))).futureValue shouldBe None

    new TestValidator().firstFail(User(Some("123"))).futureValue shouldBe Some(TextResult("error"))

    new TestValidator().firstFail(User(Some("345"))).futureValue shouldBe Some(TextResult("error1"))
  }


  it should "work with function validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, TextResult] {
      rule("bad cat") { str => str == "meow" }
    }

    class DogValidator extends Validator[String, TextResult] {
      ruleAsync("bad dog") { str => Future(str == "woof") }
    }

    class TestValidator extends Validator[Animal, TextResult] {
      registerOnFunc {
        case Animal(_, "cat") => new CatValidator().contramap(_.sound)
        case Animal(_, "dog") => new DogValidator().contramap(_.sound)
      }
    }

    new TestValidator().firstFail(Animal("meow", "cat")).futureValue shouldBe None
    new TestValidator().firstFail(Animal("woof", "dog")).futureValue shouldBe None

    new TestValidator().firstFail(Animal("meow", "dog")).futureValue shouldBe Some(TextResult("bad dog"))
    new TestValidator().firstFail(Animal("woof", "cat")).futureValue shouldBe Some(TextResult("bad cat"))
  }


  it should "work with mapping validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, TextResult] {
      rule("bad cat") { str => str == "meow" }
    }

    class DogValidator extends Validator[String, TextResult] {
      ruleAsync("bad dog") { str => Future(str == "woof") }
    }

    class TestValidator extends Validator[Animal, TextResult] {
      registerOnMapping(_.`type`)(
        "cat" -> new CatValidator().contramap(_.sound),
        "dog" -> new DogValidator().contramap(_.sound)
      )
    }

    new TestValidator().firstFail(Animal("meow", "cat")).futureValue shouldBe None
    new TestValidator().firstFail(Animal("woof", "dog")).futureValue shouldBe None

    new TestValidator().firstFail(Animal("meow", "dog")).futureValue shouldBe Some(TextResult("bad dog"))
    new TestValidator().firstFail(Animal("woof", "cat")).futureValue shouldBe Some(TextResult("bad cat"))

  }


  it should "work with partial validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, TextResult] {
      rule("bad cat") { str => str == "meow" }
    }

    class DogValidator extends Validator[String, TextResult] {
      ruleAsync("bad dog") { str => Future(str == "woof") }
    }

    class TestValidator extends Validator[Animal, TextResult] {
      registerPartial(_.`type`) {
        case "cat" => new CatValidator().contramap(_.sound)
        case "dog" => new DogValidator().contramap(_.sound)
      }
    }

    new TestValidator().firstFail(Animal("meow", "cat")).futureValue shouldBe None
    new TestValidator().firstFail(Animal("woof", "dog")).futureValue shouldBe None

    new TestValidator().firstFail(Animal("meow", "dog")).futureValue shouldBe Some(TextResult("bad dog"))
    new TestValidator().firstFail(Animal("woof", "cat")).futureValue shouldBe Some(TextResult("bad cat"))

  }


  it should "work with if validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, TextResult] {
      rule("bad cat") { str => str == "meow" }
    }

    class DogValidator extends Validator[String, TextResult] {
      ruleAsync("bad dog") { str => Future(str == "woof") }
    }

    class TestValidator extends Validator[Animal, TextResult] {
      registerIf(_.`type` == "cat") {
        new CatValidator().contramap(_.sound)
      }

      registerIf(_.`type` == "dog") {
        new DogValidator().contramap(_.sound)
      }
    }

    new TestValidator().firstFail(Animal("meow", "cat")).futureValue shouldBe None
    new TestValidator().firstFail(Animal("woof", "dog")).futureValue shouldBe None

    new TestValidator().firstFail(Animal("meow", "dog")).futureValue shouldBe Some(TextResult("bad dog"))
    new TestValidator().firstFail(Animal("woof", "cat")).futureValue shouldBe Some(TextResult("bad cat"))

  }

  "collectFails" should "work with none rule" in new Wiring {

    class TestValidator extends Validator[String, String] {
      rule("123") { str => str != "456" }

    }

    new TestValidator().collectFails("234").futureValue shouldBe Seq.empty[String]
  }

  it should "work single rule" in new Wiring {

    class TestValidator extends Validator[String, String] {


      rule("starts with 1") { str => !str.startsWith("1") }

    }

    new TestValidator().collectFails("0").futureValue shouldBe Seq.empty[String]

    new TestValidator().collectFails("1").futureValue shouldBe Seq("starts with 1")

  }

  it should "work multiple rules" in new Wiring {

    class TestValidator extends Validator[String, String] {

      rule("ends with 2") { str => !str.endsWith("2") }

      rule("starts with 1") { str => !str.startsWith("1") }

    }

    new TestValidator().collectFails("0").futureValue shouldBe Seq.empty[String]

    new TestValidator().collectFails("1").futureValue shouldBe Seq("starts with 1")
    new TestValidator().collectFails("2").futureValue shouldBe Seq("ends with 2")
    new TestValidator().collectFails("142").futureValue shouldBe Seq("ends with 2", "starts with 1")

  }

  it should "work with async rule" in new Wiring {

    class TestValidator extends Validator[String, String] {


      ruleAsync("starts with 1") { str => Future(!str.startsWith("1")) }

    }

    new TestValidator().collectFails("0").futureValue shouldBe Seq.empty[String]

    new TestValidator().collectFails("1").futureValue shouldBe Seq("starts with 1")
  }

  it should "work with multiple async rules" in new Wiring {

    class TestValidator extends Validator[String, String] {

      ruleAsync("ends with 2") { str => Future(!str.endsWith("2")) }

      ruleAsync("starts with 1") { str => Future(!str.startsWith("1")) }
    }

    new TestValidator().collectFails("0").futureValue shouldBe Seq.empty[String]

    new TestValidator().collectFails("1").futureValue shouldBe Seq("starts with 1")
    new TestValidator().collectFails("2").futureValue shouldBe Seq("ends with 2")
    new TestValidator().collectFails("142").futureValue shouldBe Seq("ends with 2", "starts with 1")

  }

  it should "work with nested Validators" in new Wiring {

    class InnerValidator extends Validator[String, String] {

      ruleAsync("ends with 2") { str => Future(!str.endsWith("2")) }

      ruleAsync("starts with 1") { str => Future(!str.startsWith("1")) }
    }

    class TestValidator extends Validator[String, String] {

      register {
        new InnerValidator
      }
    }

    new TestValidator().collectFails("0").futureValue shouldBe Seq.empty[String]

    new TestValidator().collectFails("1").futureValue shouldBe Seq("starts with 1")
    new TestValidator().collectFails("2").futureValue shouldBe Seq("ends with 2")
    new TestValidator().collectFails("142").futureValue shouldBe Seq("ends with 2", "starts with 1")

  }

  it should "work with seq validators" in new Wiring {

    case class Numbers(numbers: List[String])

    class InnerValidator extends Validator[String, String] {

      ruleAsync("ends with 2") { str => Future(!str.endsWith("2")) }

      ruleAsync("starts with 1") { str => Future(!str.startsWith("1")) }
    }

    class TestValidator extends Validator[Numbers, String] {

      registerOnSeq(_.numbers) {
        new InnerValidator
      }
    }

    new TestValidator().collectFails(Numbers("0" :: Nil)).futureValue shouldBe Seq.empty[String]

    new TestValidator().collectFails(Numbers("5" :: "1" :: Nil)).futureValue shouldBe Seq("starts with 1")
    new TestValidator().collectFails(Numbers("6" :: "2" :: Nil)).futureValue shouldBe Seq("ends with 2")
    new TestValidator().collectFails(Numbers("42" :: "1" :: Nil)).futureValue shouldBe Seq("ends with 2", "starts with 1")
  }


  it should "work with option validators" in new Wiring {

    case class Number(number: Option[String])

    class InnerValidator extends Validator[String, String] {

      ruleAsync("ends with 2") { str => Future(!str.endsWith("2")) }

      ruleAsync("starts with 1") { str => Future(!str.startsWith("1")) }
    }

    class TestValidator extends Validator[Number, String] {

      registerOnOpt(_.number) {
        new InnerValidator
      }
    }

    new TestValidator().collectFails(Number(Some("0"))).futureValue shouldBe Seq.empty[String]
    new TestValidator().collectFails(Number(None)).futureValue shouldBe Seq.empty[String]

    new TestValidator().collectFails(Number(Some("1"))).futureValue shouldBe Seq("starts with 1")
    new TestValidator().collectFails(Number(Some("2"))).futureValue shouldBe Seq("ends with 2")
    new TestValidator().collectFails(Number(Some("142"))).futureValue shouldBe Seq("ends with 2", "starts with 1")
  }


  it should "work with function validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, TextResult] {
      rule("bad cat") { str => str == "meow" }
    }

    class DogValidator extends Validator[String, TextResult] {
      ruleAsync("bad dog") { str => Future(str == "woof") }
    }

    class TestValidator extends Validator[Animal, TextResult] {
      registerOnFunc {
        case Animal(_, "cat") => new CatValidator().contramap(_.sound)
        case Animal(_, "dog") => new DogValidator().contramap(_.sound)
      }
    }

    new TestValidator().collectFails(Animal("meow", "cat")).futureValue shouldBe Nil
    new TestValidator().collectFails(Animal("woof", "dog")).futureValue shouldBe Nil

    new TestValidator().collectFails(Animal("meow", "dog")).futureValue shouldBe TextResult("bad dog") :: Nil
    new TestValidator().collectFails(Animal("woof", "cat")).futureValue shouldBe TextResult("bad cat") :: Nil
  }


  it should "work with mapping validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, TextResult] {
      rule("bad cat") { str => str == "meow" }
    }

    class DogValidator extends Validator[String, TextResult] {
      ruleAsync("bad dog") { str => Future(str == "woof") }
    }

    class TestValidator extends Validator[Animal, TextResult] {
      registerOnMapping(_.`type`)(
        "cat" -> new CatValidator().contramap(_.sound),
        "dog" -> new DogValidator().contramap(_.sound)
      )
    }

    new TestValidator().collectFails(Animal("meow", "cat")).futureValue shouldBe Nil
    new TestValidator().collectFails(Animal("woof", "dog")).futureValue shouldBe Nil

    new TestValidator().collectFails(Animal("meow", "dog")).futureValue shouldBe TextResult("bad dog") :: Nil
    new TestValidator().collectFails(Animal("woof", "cat")).futureValue shouldBe TextResult("bad cat") :: Nil

  }


  it should "work with partial validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, TextResult] {
      rule("bad cat") { str => str == "meow" }
    }

    class DogValidator extends Validator[String, TextResult] {
      ruleAsync("bad dog") { str => Future(str == "woof") }
    }

    class TestValidator extends Validator[Animal, TextResult] {
      registerPartial(_.`type`) {
        case "cat" => new CatValidator().contramap(_.sound)
        case "dog" => new DogValidator().contramap(_.sound)
      }
    }

    new TestValidator().collectFails(Animal("meow", "cat")).futureValue shouldBe Nil
    new TestValidator().collectFails(Animal("woof", "dog")).futureValue shouldBe Nil

    new TestValidator().collectFails(Animal("meow", "dog")).futureValue shouldBe TextResult("bad dog") :: Nil
    new TestValidator().collectFails(Animal("woof", "cat")).futureValue shouldBe TextResult("bad cat") :: Nil

  }


  it should "work with if validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, TextResult] {
      rule("bad cat") { str => str == "meow" }
    }

    class DogValidator extends Validator[String, TextResult] {
      ruleAsync("bad dog") { str => Future(str == "woof") }
    }

    class TestValidator extends Validator[Animal, TextResult] {
      registerIf(_.`type` == "cat") {
        new CatValidator().contramap(_.sound)
      }

      registerIf(_.`type` == "dog") {
        new DogValidator().contramap(_.sound)
      }
    }

    new TestValidator().collectFails(Animal("meow", "cat")).futureValue shouldBe Nil
    new TestValidator().collectFails(Animal("woof", "dog")).futureValue shouldBe Nil

    new TestValidator().collectFails(Animal("meow", "dog")).futureValue shouldBe TextResult("bad dog") :: Nil
    new TestValidator().collectFails(Animal("woof", "cat")).futureValue shouldBe TextResult("bad cat") :: Nil

  }

  "collectSuccesses" should "work with none rule" in new Wiring {

    class TestValidator extends Validator[String, String] {
      rule("123") { str => str == "456" }

    }

    new TestValidator().collectSuccesses("234").futureValue shouldBe Seq.empty[String]
  }

  it should "work single rule" in new Wiring {

    class TestValidator extends Validator[String, String] {


      rule("starts with 1") { str => str.startsWith("1") }

    }

    new TestValidator().collectSuccesses("0").futureValue shouldBe Seq.empty[String]

    new TestValidator().collectSuccesses("1").futureValue shouldBe Seq("starts with 1")

  }

  it should "work multiple rules" in new Wiring {

    class TestValidator extends Validator[String, String] {

      rule("ends with 2") { str => str.endsWith("2") }

      rule("starts with 1") { str => str.startsWith("1") }

    }

    new TestValidator().collectSuccesses("0").futureValue shouldBe Seq.empty[String]

    new TestValidator().collectSuccesses("1").futureValue shouldBe Seq("starts with 1")
    new TestValidator().collectSuccesses("2").futureValue shouldBe Seq("ends with 2")
    new TestValidator().collectSuccesses("142").futureValue shouldBe Seq("ends with 2", "starts with 1")

  }

  it should "work with async rule" in new Wiring {

    class TestValidator extends Validator[String, String] {


      ruleAsync("starts with 1") { str => Future(str.startsWith("1")) }

    }

    new TestValidator().collectSuccesses("0").futureValue shouldBe Seq.empty[String]

    new TestValidator().collectSuccesses("1").futureValue shouldBe Seq("starts with 1")
  }

  it should "work with multiple async rules" in new Wiring {

    class TestValidator extends Validator[String, String] {

      ruleAsync("ends with 2") { str => Future(str.endsWith("2")) }

      ruleAsync("starts with 1") { str => Future(str.startsWith("1")) }
    }

    new TestValidator().collectSuccesses("0").futureValue shouldBe Seq.empty[String]

    new TestValidator().collectSuccesses("1").futureValue shouldBe Seq("starts with 1")
    new TestValidator().collectSuccesses("2").futureValue shouldBe Seq("ends with 2")
    new TestValidator().collectSuccesses("142").futureValue shouldBe Seq("ends with 2", "starts with 1")

  }

  it should "work with nested Validators" in new Wiring {

    class InnerValidator extends Validator[String, String] {

      ruleAsync("ends with 2") { str => Future(str.endsWith("2")) }

      ruleAsync("starts with 1") { str => Future(str.startsWith("1")) }
    }

    class TestValidator extends Validator[String, String] {

      register {
        new InnerValidator
      }
    }

    new TestValidator().collectSuccesses("0").futureValue shouldBe Seq.empty[String]

    new TestValidator().collectSuccesses("1").futureValue shouldBe Seq("starts with 1")
    new TestValidator().collectSuccesses("2").futureValue shouldBe Seq("ends with 2")
    new TestValidator().collectSuccesses("142").futureValue shouldBe Seq("ends with 2", "starts with 1")

  }

  it should "work with seq validators" in new Wiring {

    case class Numbers(numbers: List[String])

    class InnerValidator extends Validator[String, String] {

      ruleAsync("ends with 2") { str => Future(str.endsWith("2")) }

      ruleAsync("starts with 1") { str => Future(str.startsWith("1")) }
    }

    class TestValidator extends Validator[Numbers, String] {

      registerOnSeq(_.numbers) {
        new InnerValidator
      }
    }

    new TestValidator().collectSuccesses(Numbers("0" :: Nil)).futureValue shouldBe Seq.empty[String]

    new TestValidator().collectSuccesses(Numbers("5" :: "1" :: Nil)).futureValue shouldBe Seq("starts with 1")
    new TestValidator().collectSuccesses(Numbers("6" :: "2" :: Nil)).futureValue shouldBe Seq("ends with 2")
    new TestValidator().collectSuccesses(Numbers("42" :: "1" :: Nil), 2).futureValue shouldBe Seq("ends with 2", "starts with 1")
  }


  it should "work with option validators" in new Wiring {

    case class Number(number: Option[String])

    class InnerValidator extends Validator[String, String] {

      ruleAsync("ends with 2") { str => Future(str.endsWith("2")) }

      ruleAsync("starts with 1") { str => Future(str.startsWith("1")) }
    }

    class TestValidator extends Validator[Number, String] {

      registerOnOpt(_.number) {
        new InnerValidator
      }
    }

    new TestValidator().collectSuccesses(Number(Some("0"))).futureValue shouldBe Seq.empty[String]
    new TestValidator().collectSuccesses(Number(None)).futureValue shouldBe Seq.empty[String]

    new TestValidator().collectSuccesses(Number(Some("1"))).futureValue shouldBe Seq("starts with 1")
    new TestValidator().collectSuccesses(Number(Some("2"))).futureValue shouldBe Seq("ends with 2")
    new TestValidator().collectSuccesses(Number(Some("142")), 2).futureValue shouldBe Seq("ends with 2", "starts with 1")
  }


  it should "work with function validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, TextResult] {
      rule("bad cat") { str => str != "meow" }
    }

    class DogValidator extends Validator[String, TextResult] {
      ruleAsync("bad dog") { str => Future(str != "woof") }
    }

    class TestValidator extends Validator[Animal, TextResult] {
      registerOnFunc {
        case Animal(_, "cat") => new CatValidator().contramap(_.sound)
        case Animal(_, "dog") => new DogValidator().contramap(_.sound)
      }
    }

    new TestValidator().collectSuccesses(Animal("meow", "cat")).futureValue shouldBe Nil
    new TestValidator().collectSuccesses(Animal("woof", "dog")).futureValue shouldBe Nil

    new TestValidator().collectSuccesses(Animal("meow", "dog")).futureValue shouldBe TextResult("bad dog") :: Nil
    new TestValidator().collectSuccesses(Animal("woof", "cat")).futureValue shouldBe TextResult("bad cat") :: Nil
  }


  it should "work with mapping validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, TextResult] {
      rule("bad cat") { str => str != "meow" }
    }

    class DogValidator extends Validator[String, TextResult] {
      ruleAsync("bad dog") { str => Future(str != "woof") }
    }

    class TestValidator extends Validator[Animal, TextResult] {
      registerOnMapping(_.`type`)(
        "cat" -> new CatValidator().contramap(_.sound),
        "dog" -> new DogValidator().contramap(_.sound)
      )
    }

    new TestValidator().collectSuccesses(Animal("meow", "cat")).futureValue shouldBe Nil
    new TestValidator().collectSuccesses(Animal("woof", "dog")).futureValue shouldBe Nil

    new TestValidator().collectSuccesses(Animal("meow", "dog")).futureValue shouldBe TextResult("bad dog") :: Nil
    new TestValidator().collectSuccesses(Animal("woof", "cat")).futureValue shouldBe TextResult("bad cat") :: Nil

  }


  it should "work with partial validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, TextResult] {
      rule("bad cat") { str => str != "meow" }
    }

    class DogValidator extends Validator[String, TextResult] {
      ruleAsync("bad dog") { str => Future(str != "woof") }
    }

    class TestValidator extends Validator[Animal, TextResult] {
      registerPartial(_.`type`) {
        case "cat" => new CatValidator().contramap(_.sound)
        case "dog" => new DogValidator().contramap(_.sound)
      }
    }

    new TestValidator().collectSuccesses(Animal("meow", "cat")).futureValue shouldBe Nil
    new TestValidator().collectSuccesses(Animal("woof", "dog")).futureValue shouldBe Nil

    new TestValidator().collectSuccesses(Animal("meow", "dog")).futureValue shouldBe TextResult("bad dog") :: Nil
    new TestValidator().collectSuccesses(Animal("woof", "cat")).futureValue shouldBe TextResult("bad cat") :: Nil

  }


  it should "work with if validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, TextResult] {
      rule("bad cat") { str => str != "meow" }
    }

    class DogValidator extends Validator[String, TextResult] {
      ruleAsync("bad dog") { str => Future(str != "woof") }
    }

    class TestValidator extends Validator[Animal, TextResult] {
      registerIf(_.`type` == "cat") {
        new CatValidator().contramap(_.sound)
      }

      registerIf(_.`type` == "dog") {
        new DogValidator().contramap(_.sound)
      }
    }

    new TestValidator().collectSuccesses(Animal("meow", "cat")).futureValue shouldBe Nil
    new TestValidator().collectSuccesses(Animal("woof", "dog")).futureValue shouldBe Nil

    new TestValidator().collectSuccesses(Animal("meow", "dog")).futureValue shouldBe TextResult("bad dog") :: Nil
    new TestValidator().collectSuccesses(Animal("woof", "cat"),2).futureValue shouldBe TextResult("bad cat") :: Nil

  }

  "firstSuccess" should "work none rule" in new Wiring {

    class TestValidator extends Validator[String, String] {

    }

    new TestValidator().firstSuccess("234").futureValue shouldBe None

  }

  it should "work single rule" in new Wiring {

    class TestValidator extends Validator[String, TextResult] {
      rule("error") { str => str == "123" }
    }

    new TestValidator().firstSuccess("234").futureValue shouldBe None

    new TestValidator().firstSuccess("123").futureValue shouldBe Some(TextResult("error"))

  }

  it should "work multiple rules" in new Wiring {

    class TestValidator extends Validator[String, TextResult] {
      rule("error") { str => str == "123" }
      rule("error1") { str => str == "345" }
    }

    new TestValidator().firstSuccess("234").futureValue shouldBe None

    new TestValidator().firstSuccess("123").futureValue shouldBe Some(TextResult("error"))

    new TestValidator().firstSuccess("345").futureValue shouldBe Some(TextResult("error1"))
  }

  it should "work with async rule" in new Wiring {

    class TestValidator extends Validator[String, TextResult] {
      ruleAsync("error") { str =>
        Future(str == "123")
      }
    }

    new TestValidator().firstSuccess("234").futureValue shouldBe None

    new TestValidator().firstSuccess("123").futureValue shouldBe Some(TextResult("error"))

  }

  it should "work with multiple async rules" in new Wiring {

    class TestValidator extends Validator[String, TextResult] {
      ruleAsync("error") { str =>
        Future(str == "123")
      }

      ruleAsync("error1") { str =>
        Future(str == "345")
      }
    }

    new TestValidator().firstSuccess("234").futureValue shouldBe None

    new TestValidator().firstSuccess("123").futureValue shouldBe Some(TextResult("error"))

    new TestValidator().firstSuccess("345").futureValue shouldBe Some(TextResult("error1"))
  }

  it should "work with nested Validators" in new Wiring {

    case class User(name: String)

    class InnerValidator extends Validator[String, TextResult] {
      rule("error") { str => str == "123" }
      ruleAsync("error1") { str => Future(str == "345") }
    }

    class TestValidator extends Validator[User, TextResult] {
      register {
        new InnerValidator().contramap(_.name)
      }
    }

    new TestValidator().firstSuccess(User("234")).futureValue shouldBe None

    new TestValidator().firstSuccess(User("123")).futureValue shouldBe Some(TextResult("error"))

    new TestValidator().firstSuccess(User("345")).futureValue shouldBe Some(TextResult("error1"))
  }

  it should "work with seq validators" in new Wiring {

    case class Tags(tags: immutable.Seq[String])

    class InnerValidator extends Validator[String, TextResult] {
      rule("error") { str => str == "123" }
      ruleAsync("error1") { str => Future(str == "345") }
    }

    class TestValidator extends Validator[Tags, TextResult] {
      registerOnSeq(_.tags) {
        new InnerValidator()
      }
    }

    new TestValidator().firstSuccess(Tags("a" :: "234" :: Nil)).futureValue shouldBe None

    new TestValidator().firstSuccess(Tags("a" :: "123" :: Nil)).futureValue shouldBe Some(TextResult("error"))

    new TestValidator().firstSuccess(Tags("a" :: "345" :: Nil)).futureValue shouldBe Some(TextResult("error1"))

    new TestValidator().firstSuccess(Tags("345" :: "234" :: Nil)).futureValue shouldBe Some(TextResult("error1"))
  }


  it should "work with option validators" in new Wiring {

    case class User(name: Option[String])

    class InnerValidator extends Validator[String, TextResult] {
      rule("error") { str => str == "123" }
      ruleAsync("error1") { str => Future(str == "345") }
    }

    class TestValidator extends Validator[User, TextResult] {
      registerOnOpt(_.name) {
        new InnerValidator()
      }
    }

    new TestValidator().firstSuccess(User(None)).futureValue shouldBe None

    new TestValidator().firstSuccess(User(Some("234"))).futureValue shouldBe None

    new TestValidator().firstSuccess(User(Some("123"))).futureValue shouldBe Some(TextResult("error"))

    new TestValidator().firstSuccess(User(Some("345"))).futureValue shouldBe Some(TextResult("error1"))
  }


  it should "work with function validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, TextResult] {
      rule("bad cat") { str => str != "meow" }
    }

    class DogValidator extends Validator[String, TextResult] {
      ruleAsync("bad dog") { str => Future(str != "woof") }
    }

    class TestValidator extends Validator[Animal, TextResult] {
      registerOnFunc {
        case Animal(_, "cat") => new CatValidator().contramap(_.sound)
        case Animal(_, "dog") => new DogValidator().contramap(_.sound)
      }
    }

    new TestValidator().firstSuccess(Animal("meow", "cat")).futureValue shouldBe None
    new TestValidator().firstSuccess(Animal("woof", "dog")).futureValue shouldBe None

    new TestValidator().firstSuccess(Animal("meow", "dog")).futureValue shouldBe Some(TextResult("bad dog"))
    new TestValidator().firstSuccess(Animal("woof", "cat")).futureValue shouldBe Some(TextResult("bad cat"))

  }


  it should "work with mapping validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, TextResult] {
      rule("bad cat") { str => str != "meow" }
    }

    class DogValidator extends Validator[String, TextResult] {
      ruleAsync("bad dog") { str => Future(str != "woof") }
    }

    class TestValidator extends Validator[Animal, TextResult] {
      registerOnMapping(_.`type`)(
        "cat" -> new CatValidator().contramap(_.sound),
        "dog" -> new DogValidator().contramap(_.sound)
      )
    }

    new TestValidator().firstSuccess(Animal("meow", "cat")).futureValue shouldBe None
    new TestValidator().firstSuccess(Animal("woof", "dog")).futureValue shouldBe None

    new TestValidator().firstSuccess(Animal("meow", "dog")).futureValue shouldBe Some(TextResult("bad dog"))
    new TestValidator().firstSuccess(Animal("woof", "cat")).futureValue shouldBe Some(TextResult("bad cat"))

  }


  it should "work with partial validators" in new Wiring {

    case class Animal(sound: String,
                      `type`: String)

    class CatValidator extends Validator[String, TextResult] {
      rule("bad cat") { str => str != "meow" }
    }

    class DogValidator extends Validator[String, TextResult] {
      ruleAsync("bad dog") { str => Future(str != "woof") }
    }

    class TestValidator extends Validator[Animal, TextResult] {
      registerPartial(_.`type`) {
        case "cat" => new CatValidator().contramap(_.sound)
        case "dog" => new DogValidator().contramap(_.sound)
      }
    }

    new TestValidator().firstSuccess(Animal("meow", "cat")).futureValue shouldBe None
    new TestValidator().firstSuccess(Animal("woof", "dog")).futureValue shouldBe None

    new TestValidator().firstSuccess(Animal("meow", "dog")).futureValue shouldBe Some(TextResult("bad dog"))
    new TestValidator().firstSuccess(Animal("woof", "cat")).futureValue shouldBe Some(TextResult("bad cat"))

  }


  "++" should "merge checks" in new Wiring {

    class TestValidator1 extends Validator[String, TextResult] {
      rule("error") { str =>
        str != "123"
      }
    }

    class TestValidator2 extends Validator[String, TextResult] {
      ruleAsync("error1") { str =>
        Future(str != "345")
      }
    }


    val resultValidator = new TestValidator1 ++ new TestValidator2

    resultValidator.firstFail("234").futureValue shouldBe None

    resultValidator.firstFail("123").futureValue shouldBe Some(TextResult("error"))

    resultValidator.firstFail("345").futureValue shouldBe Some(TextResult("error1"))
  }


  trait Wiring extends ConcurrentWirings

}
