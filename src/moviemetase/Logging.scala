package moviemetase

object LogLevel {
  val Trace = LogLevel(2,  "trace")
  val Info  = LogLevel(4,  "info")
  val Warn  = LogLevel(8,  "warn ")
  val Error = LogLevel(16, "ERROR")
}
case class LogLevel(id: Int, label: String)

object Logging {
  var out: java.io.PrintWriter = new java.io.PrintWriter( System.out )
  
  var MaxLogLineLength = 300
  var LogLineBreak = "\n  "
  
  var TimestampFormat: java.text.DateFormat = new java.text.SimpleDateFormat("HH:mm:ss")
}

trait Logging {
  def logID:  String
  var minLogLevel = LogLevel.Trace // LogLevel.Info
    
  private val logBuf = new StringBuffer()
    
  final def trace(msg: String, infos: List[(String,Any)] = Nil): Unit = log(LogLevel.Trace, msg, infos)
  final def info(msg: String,  infos: List[(String,Any)] = Nil): Unit = log(LogLevel.Info,  msg, infos)
  final def warn(msg: String,  infos: List[(String,Any)] = Nil): Unit = log(LogLevel.Warn,  msg, infos)
  final def error(msg: String, infos: List[(String,Any)] = Nil): Unit = log(LogLevel.Error, msg, infos) 
  
  final def isLogging(lvl: LogLevel): Boolean = (lvl.id >= minLogLevel.id)
     
  final def log(lvl: LogLevel, msg: String, infos: List[(String,Any)]): Unit = {
    if (isLogging(lvl)) {
      logBuf.setLength(0) // reset
      logBuf append msg
      
      if (!infos.isEmpty) {
        logBuf append " {"
        logBuf append infos.map( info => info._1+"="+info._2 ).mkString("; ")
        logBuf append "}"
      }
      
      val thread = Thread.currentThread().getName()
      val time = Logging.TimestampFormat.format( new java.util.Date() )
      Logging.out.println("[" + thread + "][" + lvl.label + "] " + time + " " + logID + "\t" + logBuf.toString) //.grouped(Logging.MaxLogLineLength).mkString(Logging.LogLineBreak))
    }
  }
}