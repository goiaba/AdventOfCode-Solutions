package day04

import java.security.MessageDigest


/*
 * This solution uses the MD5 calculation from https://gist.github.com/amscotti/2467512 
 */
object Solution extends App {
  val digest = MessageDigest.getInstance("MD5")
 
  def md5hash(text: String) = digest.digest(text.getBytes).map("%02x".format(_)).mkString
  
  //TODO: Parallelize this computation
  def hashThatStartsWith(secretKey: String, startsWith: String) = {
    def rec(currentNumber: Int): Option[String] = {
      if (md5hash(secretKey+currentNumber).startsWith(startsWith)) Some(currentNumber.toString())
      else rec(currentNumber+1)
    }
    rec(1)
  }
    
  def hashThatStartsWithFiveZeros(secretKey: String) = hashThatStartsWith(secretKey, "00000")

  def hashThatStartsWithSixZeros(secretKey: String) = hashThatStartsWith(secretKey, "000000")
  
  println(hashThatStartsWithFiveZeros("yzbqklnj"))
  println(hashThatStartsWithSixZeros("yzbqklnj"))
}