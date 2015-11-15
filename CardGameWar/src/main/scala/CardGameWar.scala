import Ranks._
import Suits._

import scala.Ordering.Implicits._
import scala.util.Random

case class Card(suit: Suit, rank: Rank)

object Card {
  def apply(suit: String, rank: String): Card = Card(Suits.withName(suit), Ranks.withName(rank))

  implicit object CardOrdering extends Ordering[Card] {
    override def compare(x: Card, y: Card): Int = if (x.rank == y.rank) x.suit compare y.suit else x.rank compare y.rank
  }

}

case class Deck(cards: List[Card]) {
  def isEmpty = cards.isEmpty

  def draw = (cards.head, Deck(cards.tail))

  def addToBottom(newCards: Card*) = Deck(cards ++ newCards)
}

case class Player(name: String, deck: Deck) {
  def outOfCards = deck.isEmpty

  def draw = if (outOfCards) None
  else {
    val (card, newDeck) = deck.draw
    Some(card, this.copy(deck = newDeck))
  }

  def takeTwoCards(card1: Card, card2: Card) = this.copy(deck = deck.addToBottom(card1, card2))
}

object CardGameWar {

  // Creates two shuffled decks of cards
  def createDecks: (Deck, Deck) = {
    val allCards =
      new Random shuffle (for {
        suit <- suits
        rank <- ranks
      } yield Card(suit, rank))

    val List(d1, d2) = allCards.grouped(allCards.length / 2).toList
    (Deck(d1), Deck(d2))
  }

  def playRound(player1: Card, player2: Card): Card = {
    player1 max player2
  }

  type Table = ((Card, Card), GameState)

  case class GameState(player1: Player, player2: Player) {
    def drawCards = for {
      (card1, player1State) <- player1.draw
      (card2, player2State) <- player2.draw
    } yield ((card1, card2), GameState(player1State, player2State))

    def hasWinner = player1.outOfCards || player2.outOfCards

    def noWinner = !hasWinner

    def winner = if (player1.outOfCards) Some(player2)
    else if (player2.outOfCards) Some(player1)
    else None
  }

  def playGame(player1: Player, player2: Player): String = {
    val resolveTable = (table: Table) => {
      val ((card1, card2), GameState(player1, player2)) = table
      val (winner, looser) = if (card1 > card2) (player1, player2) else (player2, player1)
      GameState(winner.takeTwoCards(card1, card2), looser)
    }
    val resolveRound = (currentState: GameState) => for {
      table <- currentState.drawCards
      newState = resolveTable(table)
    } yield newState
    val nextRound = (currentState: GameState) => resolveRound(currentState) match {
      case Some(newState) => newState
      case None => currentState
    }

    val initialState = GameState(player1, player2)
    val gameLoop = Iterator.iterate(initialState)(nextRound)

    val endState = gameLoop dropWhile (_.noWinner) next()
    endState.winner.get.name
  }

}

