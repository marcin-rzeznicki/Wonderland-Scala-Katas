/**
  * Created by rzeznik on 14.11.15.
  */
object Ranks extends Enumeration(2) {
  type Rank = Value
  val `2` = Value("2")
  val `3` = Value("3")
  val `4` = Value("4")
  val `5` = Value("5")
  val `6` = Value("6")
  val `7` = Value("7")
  val `8` = Value("8")
  val `9` = Value("9")
  val `10` = Value("10")
  val Jack = Value("Jack")
  val Queen = Value("Queen")
  val King = Value("King")
  val Ace = Value("Ace")

  val ranks = values.toList
}
