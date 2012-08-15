package moviemetase.test

object UrlDecode {
  def main(args: Array[String]): Unit = {
    
    val url = args(0)
    
    println( url )
    println( java.net.URLDecoder.decode(url, "UTF-8") )
  }
}