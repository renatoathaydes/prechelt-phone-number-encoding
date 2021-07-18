// Source: https://github.com/tolks/scala-test-drive/blob/master/Matcher.scala
package com.ak.scala.test

import collection.mutable.ListBuffer
import io.Source

/**
 * Implementation of the numbers-to-words algorithm described at
 * http://www.flownet.com/ron/papers/lisp-java/
 * Done to testdrive scala.
 * author: anatoli
 * Date: 27/12/10
 */

class Matcher (dict: WordsDictionary) {
// this class contains the algorithm that actually does the matching

  def findWords(phone: List[Int], alreadyMatched : List[Word]) : List[List[Word]] = {
    if (phone.size == 0) {
      return List(alreadyMatched)
    }
    var answers = new ListBuffer[List[Word]]();
    val matches = dict.findMatchingWords(phone)
    if (!matches.isEmpty)
    {
      for (wordMatch <- matches) {
        if (wordMatch.digits.size == phone.size) {
          answers += (alreadyMatched ++ List(wordMatch))
        } else {
          answers ++= findWords(phone.takeRight(phone.size - wordMatch.digits.size), alreadyMatched ++ List(wordMatch));
        }
      }
    } else {
      if (matches.isEmpty && (alreadyMatched.isEmpty || !alreadyMatched.last.isDigit)){
        val wordMatch = new Word(List(phone.head), phone.head.toString);
        answers ++= (findWords(phone.takeRight(phone.size - 1), alreadyMatched ++ List(wordMatch)))
      }
    }
    return answers.toList;
  }
}

object Matcher {

  def createDigitList(phoneString: String) : List[Int] = {
    val ret = new ListBuffer[Int]()
    for (charachter <- phoneString) {
      if (charachter.isDigit) {
        ret += charachter.getNumericValue
      }
    }
    ret.toList
  }

  def main(args: Array[String]) {
    val dict = new WordsDictionary("path/to/dictionary.txt")
    val srcFile = Source.fromFile("path/to/input.txt")
    for(val line <- srcFile.getLines) { // gives you an iterator
      val digits: List[Int] = createDigitList(line)
      val found = new Matcher(dict).findWords(digits, List())
      found.foreach(l => {
        print(line + ": ")
        l.foreach(w => {Console.print(w.view + " ")});
        println
      })
    }
  }
}

object WordsDictionary {

  val keyMap = Map(
      'E' -> 0, 'J' -> 1,'N' -> 1,'Q' -> 1,'R' -> 2,'W' -> 2,'X' -> 2,'D' -> 3,'S' -> 3,'Y' -> 3,'F' -> 4,'T' -> 4,
      'A' -> 5,'M' -> 5,'C' -> 6,'I' -> 6,'V' -> 6,'B' -> 7,'K' -> 7,'U' -> 7,'L' -> 8,'O' -> 8,'P' -> 8,'G' -> 9,
      'H' -> 9,'Z' -> 9
    )

  def convertToWord(someText:String): Word = {
      var numbers = new ListBuffer[Int]();
      for (character <- someText){
        keyMap.get(character.toUpper).foreach(v => numbers+=v )
      }
      new Word(numbers.toList, someText);
  }
}

class WordsDictionary(fileName:String) {
// This class represents a dictionary of words that is structured as a tree.
// Each node has 10 edges (for each of 10 digits) and the list of words where each word 'encodes' the path from the top
// of the tree.
// I.e. if the path you took to get to the node is 5-1-3 then the node will have 'and' in its list of words.
// All the tree operations are defined inside Node class


  val top = new Node();
  {
    val srcFile = Source.fromFile(fileName)
    for(val line <- srcFile.getLines) { // gives you an iterator
      top.insert(WordsDictionary.convertToWord(line))
    }
  }

  def findMatchingWords(phone: List[Int]) : List[Word] = {
    top.findMatchingWords(phone)
  }
}

class Node {
  val load = new ListBuffer[Word]()
  val leafs:Array[Option[Node]] = Array.fill(10)(None)

  def insert(word:Word):Unit = insert(word.digits, word)
  def insert(digits:List[Int], word:Word): Unit = {
    digits match {
      case head::tail => {
        leafs(head) match {
          case Some(node) => node.insert(tail,word)
          case None => {
            val node = new Node()
            leafs(head) = new Some(node)
            node.insert(tail,word)}
        }
      }
      case _ => load += word
    }
  }

  def findMatchingWords(digits: List[Int]) : List[Word] = digits match {
    case head::tail => load.toList ++ (leafs(head) match {
      case None => None
      case Some(node) => node.findMatchingWords(tail)
    })
    case _ => load.toList
  }
}
class Word (val digits: List[Int], val view: String) {

  def isDigit:Boolean = {
    digits.size == 1 && digits.head.toString.equals(view);
  }
}
