package moviemetase

import java.security.MessageDigest
import java.io.InputStream
import java.io.File

object FileCache {
  val dir = App.dataDir("cache")
  
  val md = MessageDigest.getInstance("MD5")
  
  def fileKey(s: String): String = md.digest( s.toString.getBytes("UTF-8") ).map("%02X" format _).mkString("")
}

trait FileCache[A] {
  import FileCache._
  
  def file(k: String): File = new File(dir, fileKey(k)) 
  
  def exists(k: String): Boolean = file(k).exists
  
  def put(k: AnyRef, a: A): Unit = {
    
    
  }
  
  def get(k: String): A
  def read(k: String): InputStream
  
  def remove(k: String): Unit
}

//TODO impl