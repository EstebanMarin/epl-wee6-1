package forcomp

import scala.io.{Codec, Source}

object Anagrams extends AnagramsInterface:

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers
    * saying how often the character appears. This list is sorted alphabetically
    * w.r.t. to the character in each pair. All characters in the occurrence
    * list are lowercase.
    *
    * Any list of pairs of lowercase characters and their frequency which is not
    * sorted is **not** an occurrence list.
    *
    * Note: If the frequency of some character is zero, then that character
    * should not be in the list.
    */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words. It is predefined and
    * obtained as a sequence using the utility method `loadDictionary`.
    */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
    *
    * Note: the uppercase and lowercase version of the character are treated as
    * the same character, and are represented as a lowercase character in the
    * occurrence list.
    *
    * Note: you must use `groupBy` to implement this method!
    */
  def wordOccurrences(w: Word): Occurrences =
    w.map(_.toLower)
      .filterNot(_.isWhitespace)
      .groupBy(identity)
      .toList
      .map(x => (x._1, x._2.size))
      .sortWith((x, y) => x._1 < y._1)

  // def combineMap(m1: Map[(Char, Int)], m2: Map[(Char, Int)]) = ???

  /** Converts a sentence into its character occurrence list. */
  def combineMap(m1: Map[Char, Int], m2: Map[Char, Int]): Map[Char, Int] =
    def traMap(m1t: Map[Char, Int], acc: Map[Char, Int]): Map[Char, Int] =
      if m1t.isEmpty then acc
      else
        val head = m1t.head
        if m2.contains(head._1) then
          traMap(
            m1t.removed(head._1),
            acc.updated(head._1, m2(head._1) + head._2)
          )
        else traMap(m1t.removed(head._1), acc ++ Map(head))
    traMap(m1, m2)

  def sentenceOccurrences(s: Sentence): Occurrences =
    s
      .map(_.toLowerCase)
      .map(wordOccurrences)
      .map(_.toMap)
      .fold(Map.empty)(combineMap)
      .toList
      .sortWith((x, y) => x._1 < y._1)

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a
    * sequence of all the words that have that occurrence count. This map serves
    * as an easy way to obtain all the anagrams of a word given its occurrence
    * list.
    *
    * For example, the word "eat" has the following character occurrence list:
    *
    * `List(('a', 1), ('e', 1), ('t', 1))`
    *
    * Incidentally, so do the words "ate" and "tea".
    *
    * This means that the `dictionaryByOccurrences` map will contain an entry:
    *
    * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
    */

  // type Occurrences = List[(Char, Int)]
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy[Occurrences](wordOccurrences)

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences.getOrElse(wordOccurrences(word), Nil)

  /** Returns the list of all subsets of the occurrence list. This includes the
    * occurrence itself, i.e. `List(('k', 1), ('o', 1))` is a subset of
    * `List(('k', 1), ('o', 1))`. It also include the empty subset `List()`.
    *
    * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))`
    * are:
    *
    * List( List(), List(('a', 1)), List(('a', 2)), List(('b', 1)), List(('a',
    * 1), ('b', 1)), List(('a', 2), ('b', 1)), List(('b', 2)), List(('a', 1),
    * ('b', 2)), List(('a', 2), ('b', 2)) )
    *
    * Note that the order of the occurrence list subsets does not matter -- the
    * subsets in the example above could have been displayed in some other
    * order.
    */
  def combinations(occurrences: Occurrences): List[Occurrences] =
    val allOcurrences =
      for
        (c: Char, int: Int) <- occurrences
        v <- (1 to int)
      yield (c -> v)

    def filter(x: Occurrences) =
      x.groupBy(x => x._1)
        .toList
        .map(x => (x._1, x._2.size))
        .toMap

    ((0 to allOcurrences.size) flatMap allOcurrences.combinations).toList
      .map(filter)
      .toSet
      .toList
      .map(x => x.toList)

  /** Subtracts occurrence list `y` from occurrence list `x`.
    *
    * The precondition is that the occurrence list `y` is a subset of the
    * occurrence list `x` -- any character appearing in `y` must appear in `x`,
    * and its frequency in `y` must be smaller or equal than its frequency in
    * `x`.
    *
    * Note: the resulting value is an occurrence - meaning it is sorted and has
    * no zero-entries.
    */
  def subtract(x: Occurrences, y: Occurrences): Occurrences =
    def traY(yT: Occurrences, acc: Occurrences): Occurrences =
      val accTM = acc.toMap
      val yTM = yT.toMap
      if yT.isEmpty then acc
      else
        val head = yTM.head
        val char = head._1
        if accTM.contains(char) then
          traY(
            yTM.removed(char).toList,
            accTM.updated(char, (accTM(head._1) - head._2).abs).toList
          )
        else traY(yT.toMap.removed(char).toList, acc)
    traY(y, x).sortWith((x, y) => x._1 < y._2).filter(x => x._2 > 0)

  /** Returns a list of all anagram sentences of the given sentence.
    *
    * An anagram of a sentence is formed by taking the occurrences of all the
    * characters of all the words in the sentence, and producing all possible
    * combinations of words with those characters, such that the words have to
    * be from the dictionary.
    *
    * The number of words in the sentence and its anagrams does not have to
    * correspond. For example, the sentence `List("I", "love", "you")` is an
    * anagram of the sentence `List("You", "olive")`.
    *
    * Also, two sentences with the same words but in a different order are
    * considered two different anagrams. For example, sentences `List("You",
    * "olive")` and `List("olive", "you")` are different anagrams of `List("I",
    * "love", "you")`.
    *
    * Here is a full example of a sentence `List("Yes", "man")` and its anagrams
    * for our dictionary:
    *
    * List( List(en, as, my), List(en, my, as), List(man, yes), List(men, say),
    * List(as, en, my), List(as, my, en), List(sane, my), List(Sean, my),
    * List(my, en, as), List(my, as, en), List(my, sane), List(my, Sean),
    * List(say, men), List(yes, man) )
    *
    * The different sentences do not have to be output in the order shown above
    * \- any order is fine as long as all the anagrams are there. Every returned
    * word has to exist in the dictionary.
    *
    * Note: in case that the words of the sentence are in the dictionary, then
    * the sentence is the anagram of itself, so it has to be returned in this
    * list.
    *
    * Note: There is only one anagram of an empty sentence.
    */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = ???

object Dictionary:
  def loadDictionary: List[String] =
    val wordstream = Option {
      getClass.getResourceAsStream(
        List("forcomp", "linuxwords.txt").mkString("/", "/", "")
      )
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try
      val s = Source.fromInputStream(wordstream)(Codec.UTF8)
      s.getLines().toList
    catch
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    finally wordstream.close()
