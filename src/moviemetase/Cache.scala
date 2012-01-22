package moviemetase

import java.security.MessageDigest
import java.io.InputStream

object FileCache {
  val dir = App.dataDir("cache")
  
  val md = MessageDigest.getInstance("MD5")
  
  def key(s: String): String =
    md.digest( s.toLowerCase.getBytes("UTF-8") ).map("%02X" format _).mkString("")
}

trait FileCache[A] {
  def exists(k: String): Boolean
  
  def put(k: String, a: A): Unit
  
  def get(k: String): A
  def read(k: String): InputStream
  
  def remove(k: String): Unit
}

//TODO impl