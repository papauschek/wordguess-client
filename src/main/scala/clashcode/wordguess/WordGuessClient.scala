package clashcode.wordguess

import akka.actor._
import akka.event.Logging
import scala.collection.mutable

import messages._
import scala.util.Random
import scala.io.Source
import java.io.File

case class Guess(word: List[Option[Char]], tried: List[Char])

class WordGuesserClient(playerName: String, gameServer: ActorRef) extends Actor {

  val letters = "abcdefghijklmnopqrstuvwxyz".toCharArray.toSeq

  val guesses = mutable.Set.empty[Guess]
  var lastGuess = Option.empty[Guess]
  var triedLetters = List.empty[Char] // tried chars

  loadState()
  requestGame()


  // Incoming messages from the server are handled here
  override def receive = {
    // When a game was accepted or after a guess was made
    case status: GameStatus =>
      def matches(word: List[Option[Char]], revealed: List[Option[Char]]) : Boolean = {
        word.zip(revealed).forall {
          case (Some(w), Some(r)) => w == r
          case (Some(_), None) => false
          case _ => true
        }
      }

      val currentGuess = handleStatus(status)


      // get statistics for exact match
      val matchingLength = guesses.toSeq.filter(g => g.word.length == currentGuess.word.length)
      val matchingWord = matchingLength.filter(g => matches(currentGuess.word, g.word))

      // current source of letter statistics
      var source = "any"
      val statsSource =
        if (matchingWord.size > 1) {
          source = "exact"
          matchingWord
        }
        else if (matchingLength.size > 1) {
          source = "length"
          matchingLength
        }
        else guesses.toSeq

      // get p of each letter
      val remainingLetters = letters.diff(triedLetters ++ currentGuess.word.flatten)
      val letterP = remainingLetters.map(l => {
        var total = 0.0
        var part = 0.0
        statsSource.foreach(s => {

          // detect if letter part of this word
          val contained = s.word.exists(_.exists(_ == l))
          val solved = s.word.forall(_.isDefined)
          val notContained = (!contained && solved) || s.tried.exists(_ == l)

          if (notContained || contained) {
            total += 1
            if (contained) part += 1
          } else {
            total += 0.01
            part += 0.0001 // no idea about this letter
          }

        })

        val p = part / total.max(1)
        (l, p) // probability of letter
      })

      // guessing next letter
      val bestP = letterP.sortBy(- _._2).head._2
      val bestLetter = Random.shuffle(letterP.filter(_._2 == bestP)).head
      val letter = bestLetter._1
      println("guessing " + letter + ", " + bestP + ", matching " + source + ": " + matchingWord.length)
      triedLetters = triedLetters.::(letter)
      makeGuess(letter)

    // When the game was won
    case GameWon(status) =>
      handleStatus(status)
      println("WON: " + status.letters.map(_.getOrElse('?')).mkString)
      requestGame()
    case GameLost(status) =>
      handleStatus(status)
      println("LOST: " + status.letters.map(_.getOrElse('?')).mkString)
      broadCastMsg(status.gameId + " " + status.letters.map(_.getOrElse('.').toLower).mkString + " " + triedLetters.mkString)
      requestGame()
    case NoAvailableGames() =>
      println("no available games!")
    case NotPlayingError() =>
      println("Error: we're not playing")
    case MsgToAll(msg) =>
    {
      println("broadcast: " + msg)
      try {
        val split = msg.split(" ")
        val letters = split(1).toCharArray.map(c => if (c == '.') Option.empty[Char] else Some(c))
        val tried = split(2).toCharArray
        //lets ignore the chatter ;-)
        //guesses += Guess(letters.toList, tried.distinct.diff(letters.toList.flatten).distinct.toList)
      } catch {
        case ex: Throwable => println("could not parse: " + msg)
      }
    }
  }


  private def handleStatus(status: GameStatus) : Guess = {

    // remove last try
    lastGuess.foreach(last => {
      guesses -= last
    })

    // add result
    //println("we tried letters: " + triedLetters)
    val otherLetters =  triedLetters.diff(status.letters.flatten.map(_.toLower) )
    //println("missing: " + otherLetters)
    val currentGuess = Guess(status.letters.toList.map(_.map(_.toLower)), otherLetters)
    guesses += currentGuess
    //println("guesses: " + guesses.size + ", without tries: " + guesses.count(_.tried.length == 0))

    // store last guess if not won
    if (status.remainingTries == 0 || status.letters.forall(_.isDefined))
    {
      lastGuess = None
      // save new guess
      saveState()
    }
    else
      lastGuess = Some(currentGuess)

    // log result
    val wordRepresentation = currentGuess.word.map(optC => optC.getOrElse('_')).mkString
    println("Word: " + wordRepresentation + ", tried: " + currentGuess.tried.mkString + ", remaining tries: " + status.remainingTries)

    currentGuess
  }

  // Request a game from the server; start by doing this
  def requestGame() {
    println("request game? ")
    //if (readLine() == "x") context.system.shutdown()
    lastGuess = None
    triedLetters = List.empty
    gameServer ! RequestGame(playerName)
  }
  // You try to guess the word by making guesses
  def makeGuess(letter: Char) {
    gameServer ! MakeGuess(letter)
  }

  private def serialize(guess: Guess) = {
    guess.word.map(_.getOrElse(".")).mkString + ";" + guess.tried.mkString
  }

  def loadState() {

    val file = new File("state.txt")
    if (!file.exists()) return

    try
    {
      val src = Source.fromFile(file)
      src.getLines().foreach(line => {
        val split = line.split(";")
        val letters = split(0).toCharArray.map(c => if (c == '.') Option.empty[Char] else Some(c))
        println(split.toList)
        val tried = if (split.length >= 2) split(1).toCharArray.toSeq else Seq.empty
        if (tried.length > 0)
          guesses += Guess(letters.toList, tried.toList)
      })
      println("loaded " + guesses.size)
    } catch{
      case ex: Throwable =>println(ex)
    }
  }

  def saveState() {
    val file = new File("state.txt")
    val out_stream = new java.io.PrintStream(file)
    guesses.foreach(g =>
      out_stream.println(serialize(g)))
    out_stream.close
  }

  // You can send a message to all other players (to chat?)
  def broadCastMsg(msg: String) {
    gameServer ! SendToAll(msg)
  }

}



