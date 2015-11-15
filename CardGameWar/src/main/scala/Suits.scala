/**
  * Created by rzeznik on 14.11.15.
  */
object Suits extends Enumeration {
  type Suit = Value
  val Spade, Club, Diamond, Heart = Value

  val suits = values.toList
}
