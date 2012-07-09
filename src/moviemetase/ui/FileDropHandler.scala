package moviemetase
package ui

import scala.swing.Publisher
import scala.swing.event.Event
import javax.swing.{JComponent, TransferHandler}
import java.awt.datatransfer.{Transferable, DataFlavor}
import java.net.URI
import java.io.{File => JFile}
  
object FileDropHandler {
  
  case class FilesDropped(files: List[JFile]) extends Event
}

class FileDropHandler extends TransferHandler with Publisher {
  import FileDropHandler._
  
  val stdFlavor = DataFlavor.javaFileListFlavor
  val nixFlavor = new DataFlavor("text/uri-list;class=java.lang.String")
    
  override def canImport(comp: JComponent, flavors: Array[DataFlavor]): Boolean =
    flavors.exists(flavor =>
      (flavor == stdFlavor) ||
      (flavor == nixFlavor)
    )
  
  override def importData(comp: JComponent, t: Transferable): Boolean = {
    val flavors = t.getTransferDataFlavors()
    try {
      val files = if (flavors.exists(_ == stdFlavor)) {
        val data = t.getTransferData(stdFlavor)
        importStdFileList( data )
      } else if (flavors.exists(_ == nixFlavor)) {
        val data = t.getTransferData(nixFlavor)
        importNixFileList( data )
      } else List()
      
      publish( FilesDropped(files) )
      !files.isEmpty
    }
    catch {
      case (e: Exception) => {
        e.printStackTrace()
        false
      }
    }
  }
  
  private def importStdFileList(data: Any): List[JFile] = {
    val files = new scala.collection.mutable.ListBuffer[JFile]
    
    data match {
      case dataList: java.util.List[_] => {
        val it = dataList.iterator
        while (it.hasNext) {
          val o = it.next.asInstanceOf[AnyRef]
          if (o.isInstanceOf[JFile])
            files append o.asInstanceOf[JFile]
        }
      }
      case dataRef: AnyRef => {
        Console.err.println("FileDropHandler: invalid data " + dataRef.getClass.getName)
      }
    }

    files.toList
  }
    
    
  private def importNixFileList(data: Any): List[JFile] = {
    
    def clean(rawLine: String): Option[String] = {
      val line = rawLine.trim
      if (line.length == 0 || line == "#")
      None
      else
      Some(line)
    }
    
    def asURI(line: String): Option[URI] = {
      try   { Some(new URI(line)) }
      catch { case e:Exception => println(e); None }
    }
    
    def asFile(uri: URI): Option[JFile] = {
      try   { Some(new JFile(uri)) }
      catch { case e:Exception => println(e); None }
    }
    
    data.asInstanceOf[java.lang.String].split("\n")
     .toList flatMap clean flatMap asURI flatMap asFile
  }
}