package moviemetase

object XmlRpc {
  def methodCall(method: String, params: List[Any]): String = {
    val sb = new StringBuilder()
    sb append "<methodCall>\n"
      sb append "<methodName>" append method append "</methodName>\n"
      sb append "<params>\n"
      for (param <- params) {
        sb append "<param>\n" append "<value>"
        sb append value(param)
        sb append "</value>" append "</param>\n"
      }
      sb append "</params>\n"
    sb append "</methodCall>\n" 
    sb.toString
  }
  
  def value(v: Any): String = v match {
    case b:Boolean  => "<boolean>" + (if (b) "1" else "0") + "</boolean>"
    case d:Double   => "<double>" + ("%1.0f" format d) + "</double>" //TODO XmlRpc Double-Format
    case i:Integer  => "<int>" + i + "</int>"
    
    case a:Array[_] => {
      val sb = new StringBuilder
      sb append "<array>" append "<data>\n" 
      for (av <- a) {
        sb append "<value>" append value(av) append "</value>\n"
      }
      sb append "</data>" append "</array>\n"
      sb.toString
    }
    
    case s:List[(String,_)] => {
      val sb = new StringBuilder
      sb append "<struct>\n" 
      for ( (sn,sv) <- s) {
        sb append "<member>\n"
        sb append "<name>"  append sn        append "</name>\n"
        sb append "<value>" append value(sv) append "</value>\n"
        sb append "</member>\n"
      }
      sb append "</struct>\n"
      sb.toString
    }
    
    case null       => "<nil/>"
    case s:String   => "<string>" + s + "</string>"
    case x          => "<string>" + x + "</string>" //TODO fallback to string?
  }
}