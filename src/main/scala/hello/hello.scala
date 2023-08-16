package hello

object Hello {
  val LowerCaseAlphabet = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
  val length = this.LowerCaseAlphabet.length
  val rand = scala.util.Random

  private val sentence = "Hello, "

  sealed trait Entity {
    def unroll(): String
  }

  case class Person(name: List[String], title: String) extends Entity {
    override def unroll(): String = {
      {
        var str = ""

        for (n <- name) {
          str = str + n + " "
        }

        str
      }
    }
  }

  case class Company(name: List[String]) extends Entity {
    override def unroll(): String = {
      {
        var str = "Empresa "

        for (n <- name) {
          str = str + n + " "
        }

        str
      }
    }
  }

  def makeWord(size: Int): String = {
    LowerCaseAlphabet(rand.nextInt(length)).toUpper +
      (1 to size).map(_ => LowerCaseAlphabet(rand.nextInt(length))).mkString
  }

  def SayHello(setOfEntities: Set[Entity]): Unit = {
    setOfEntities
      .flatMap {
        case person: Person => Set(sentence + person.title + person.unroll())
        case company: Company => Set(sentence + company.unroll())
      }
      .foreach(msg => println(s"$msg"))
  }

  def main(args: Array[String]): Unit = {
    val setSize = 50
    val nameSize = 5
    val wordSize = 9
    val titleSize = 3
    val set = (1 to setSize)
      .map(_ => {
        val name = (1 to rand.nextInt(nameSize)).map(_ => makeWord(wordSize) + " ").toList

        if (rand.nextBoolean()) {
          Person(name, makeWord(titleSize) + ". ")
        } else {
          Company(name)
        }
      }).toSet[Entity]

    SayHello(set)
  }
}