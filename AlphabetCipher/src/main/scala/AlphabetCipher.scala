import scala.language.postfixOps

object AlphabetCipher {
  def cyclicGet(s: IndexedSeq[Char])(index: Int) = {
    val n = s.length
    val indexModN = index % n
    if (index < 0) s(indexModN + n) else s(indexModN)
  }

  private def mkPairs(str: String, other: String) = {
    def cycleString(str: String, length: Int) = Iterator.tabulate(length)(cyclicGet(str))

    val n = str.length
    str.iterator zip (if (n <= other.length) other.iterator else cycleString(other, n))
  }

  val alphabet = 'a' to 'z'
  private val pickLetter: Int => Char = cyclicGet(alphabet)

  private def indexOf(l: Char) = l - 'a'

  private val encodeLetter: (((Char, Char)) => Char) = pickLetter compose { case (l1, l2) => indexOf(l1) + indexOf(l2) }
  private val decodeLetter: (((Char, Char)) => Char) = pickLetter compose { case (l1, l2) => indexOf(l1) - indexOf(l2) }

  def encode(keyword: String, message: String): String = mkPairs(message, keyword) map encodeLetter mkString

  def decode(keyword: String, message: String): String = mkPairs(message, keyword) map decodeLetter mkString

  def decipher(cipher: String, message: String): String = {
    val keyword = decode(message, cipher)

    longestCommonSuffixLength(keyword) match {
      case Some(suffixLength) => keyword dropRight suffixLength
      case None => keyword
    }
  }

  private def suffixes(str: String) = {
    def mkSuffix(from: Int) = (Iterator.range(from, str.length) map str, str.length - from)
    str.indices map mkSuffix toIterator
  }

  private def longestCommonSuffixLength(str: String) = {
    def isPrefix(other: Iterator[Char]) = str.iterator zip other forall { case (c1, c2) => c1 == c2 }

    val properSuffixes = suffixes(str) drop 1
    properSuffixes find { case (suffix, _) => isPrefix(suffix) } map (_._2)
  }

}