import Anagrams._

import scala.collection.immutable.List
object Main extends App {
  println(fingerPrint(List("ate", "eat", "tea", "pot", "top", "sonja", "jason", "normal",
    "I", "love", "you", "olive")))

  println(wordAnagrams("eta"))

}
