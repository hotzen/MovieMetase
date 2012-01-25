package moviemetase

object LogLevel {
  val Trace = LogLevel(2,  "trace")
  val Info  = LogLevel(4,  "info ")
  val Warn  = LogLevel(8,  "warn ")
  val Error = LogLevel(16, "ERROR")
}

case class LogLevel(id: Int, label: String) {
  override def equals(a: Any): Boolean = a match {
    case LogLevel(x, _) => (x == id)
    case _ => false
  }
}

object Logging {
  var out: java.io.PrintWriter = new java.io.PrintWriter( System.out )
  
  @volatile
  var level: LogLevel = LogLevel.Trace //LogLevel.Info
    
  var TimestampFormat: java.text.DateFormat = new java.text.SimpleDateFormat("HH:mm:ss")
}

trait Logging {
  def logID:  String
      
  private val logBuf = new StringBuffer()
    
  final def trace(msg: String, infos: List[(String,Any)] = Nil): Unit = log(LogLevel.Trace, msg, infos)
  final def info(msg: String,  infos: List[(String,Any)] = Nil): Unit = log(LogLevel.Info,  msg, infos)
  final def warn(msg: String,  infos: List[(String,Any)] = Nil): Unit = log(LogLevel.Warn,  msg, infos)
  final def error(msg: String, infos: List[(String,Any)] = Nil): Unit = log(LogLevel.Error, msg, infos) 
    
  final def isLogging(lvl: LogLevel): Boolean = (lvl.id >= Logging.level.id)

  final def log(lvl: LogLevel, msg: String, infos: List[(String,Any)]): Unit = {
    if (!isLogging(lvl))
      return
    
    logBuf setLength 0 // reset

    logBuf append "[" append Thread.currentThread.getName append "] "
    logBuf append "[" append lvl.label append "] "
    logBuf append Logging.TimestampFormat.format( new java.util.Date() ) 
    logBuf append " " append logID append "\t"
    logBuf append msg
    
    if (!infos.isEmpty) {
      logBuf append " {"
      logBuf append infos.map( info => info._1+"="+info._2 ).mkString("; ")
      logBuf append "}"
    }
    
    Logging.out println logBuf.toString
  }
}