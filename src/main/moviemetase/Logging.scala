package moviemetase

import java.io.PrintWriter
import java.util.Date

object LogLevel {
  val Trace = LogLevel(1, "trace")
  val Info  = LogLevel(2, "info")
  val Warn  = LogLevel(3, "warn")
  val Error = LogLevel(4, "ERROR")
}
case class LogLevel(id: Int, name: String)

object Logging {
  @volatile var out: PrintWriter = new PrintWriter( System.out )
  @volatile var level: LogLevel = LogLevel.Info
  //var TimestampFormat: java.text.DateFormat = new java.text.SimpleDateFormat("HH:mm:ss")
}

trait Logging {
  def logLevel: LogLevel  = Logging.level 
  def logOut: PrintWriter = Logging.out 
  def logID: String
    
  final def isLogging(lvl: LogLevel): Boolean = (lvl.id >= logLevel.id)
  final def trace(msg: =>String, infos: List[(String,Any)] = Nil): Unit = log(LogLevel.Trace, msg, infos)
  final def info(msg: String, infos: List[(String,Any)] = Nil): Unit = log(LogLevel.Info, msg, infos)
  final def warn(msg: String, infos: List[(String,Any)] = Nil): Unit = log(LogLevel.Warn, msg, infos)
  final def error(thrown: Throwable, msg: String): Unit = log(LogLevel.Error, msg, ("exception" -> thrown) :: Nil)
  final def error(msg: String, infos: List[(String,Any)] = Nil): Unit = log(LogLevel.Error, msg, infos)
  
  private val buf = new StringBuffer()
  final def log(lvl: LogLevel, msg: String, infos: List[(String,Any)]) {
    if (!isLogging(lvl))
      return
    
    buf setLength 0 // reset

    //buf append "[" append Thread.currentThread.getName append "] "
    buf append "[" append lvl.name append "] "
    //buf append Logging.TimestampFormat.format( new Date() ) 
    buf append " " append logID append " " //append "\t"
    buf append msg
    
    if (!infos.isEmpty) {
      buf append " {"
      for ((k,v) <- infos)
        buf append k append "='" append v append "'"
      buf append "}"
      
      infos.collect({case (_, t:Throwable) => t}).map(t => {
        val sw = new java.io.StringWriter()
        val pw = new java.io.PrintWriter(sw)
        t.printStackTrace(pw)
        buf append "\n" append sw.toString
      })
    }
    
    val s = buf.toString
    logOut synchronized {
      logOut.println( s )
      logOut.flush()
    }
  }
}