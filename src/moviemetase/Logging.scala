package moviemetase

import java.io.PrintWriter
import java.util.Date

object LogLevel {
  val Trace = LogLevel(2,  "trace")
  val Info  = LogLevel(4,  "info")
  val Warn  = LogLevel(8,  "warn")
  val Error = LogLevel(16, "ERROR")
}

case class LogLevel(id: Int, label: String) {
  override def equals(a: Any): Boolean = a match {
    case LogLevel(x, _) => (x == id)
    case _ => false
  }
}

object Logging {
  var out: PrintWriter = new PrintWriter( System.out )
  
  @volatile var level: LogLevel = LogLevel.Info

  var TimestampFormat: java.text.DateFormat = new java.text.SimpleDateFormat("HH:mm:ss")
}

trait Logging {
  def logID:  String
  
  final def isLogging(lvl: LogLevel): Boolean = (lvl.id >= Logging.level.id)
  
  private val logBuf = new StringBuffer()
  
  final def trace(msg: =>String, infos: List[(String,Any)] = Nil): Unit = log(LogLevel.Trace, msg, infos)
  final def info(msg: String,    infos: List[(String,Any)] = Nil): Unit = log(LogLevel.Info,  msg, infos)
  final def warn(msg: String,    infos: List[(String,Any)] = Nil): Unit = log(LogLevel.Warn,  msg, infos)
  final def error(msg: String,   infos: List[(String,Any)] = Nil): Unit = log(LogLevel.Error, msg, infos)
  
  final def log(lvl: LogLevel, msg: String, infos: List[(String,Any)]): Unit = {
    if (!isLogging(lvl))
      return
    
    logBuf setLength 0 // reset

    logBuf append "[" append Thread.currentThread.getName append "] "
    logBuf append "[" append lvl.label append "] "
    logBuf append Logging.TimestampFormat.format( new Date() ) 
    logBuf append " " append logID append "\t"
    logBuf append msg
    
    if (!infos.isEmpty) {
      logBuf append " {"
      for ((k,v) <- infos)
        logBuf append k append "='" append v append "'"
      logBuf append "}"
    }
        
    Logging.out synchronized {
      Logging.out.println( logBuf.toString )
      Logging.out.flush()
    }
  }
}