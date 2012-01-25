package moviemetase

import java.security.MessageDigest
import java.io.InputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.awt.image.BufferedImage
import javax.imageio.ImageIO


object FileCache {
  val dir = App.dataDir("cache")
  
//  val md = MessageDigest.getInstance("MD5")
//  def md5(s: String): String = FileCache.md.digest( s.toString.getBytes("UTF-8") ).map("%02X" format _).mkString("")
}

trait FileCache[A] {
  import FileCache._
  
  def name(k: String): String =
    """\W""".r.replaceAllIn(k, _ => "_")

  def file(k: String): File =
    new File(dir, name(k)) 
  
  def exists(k: String): Boolean =
    file(k).exists
  
  def read(k: String): Option[FileInputStream] =
    if (file(k).exists) Some( new FileInputStream(file(k)) ) // fail if not readable
    else None

  def write(k: String): Option[FileOutputStream] =
    Some( new FileOutputStream(file(k)) ) // fail if not writeable

  def remove(k: String): Unit =
    if (exists(k)) file(k).delete()

  def put(k: String, a: A): Unit
  def get(k: String): Option[A]
}

class ImageCache extends FileCache[BufferedImage] with Logging {
  val logID = "ImageCache"
  
  val format = "JPEG"
  
  def get(k: String): Option[BufferedImage] =
    read(k).map(f => {
      val img = ImageIO.read(f)
      f.close()
      img
    })
  
  def put(k: String, img: BufferedImage): Unit =
    write(k).foreach(f => {
      ImageIO.write(img, format, f)
      f.close()
    })
}