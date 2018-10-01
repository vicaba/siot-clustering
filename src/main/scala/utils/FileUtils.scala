package utils
import java.io.{File, FileInputStream, FileOutputStream}

object FileUtils {

  def copyFile(srcFile: String, dstFile: String): Unit = {
    val src   = new File(srcFile)
    val _dest = new File(dstFile)
    val dest =
      if (_dest.isFile) _dest
      else new File(_dest.getAbsolutePath + "/" + src.getName)

    println(dest.getAbsolutePath)

    new FileOutputStream(dest) getChannel () transferFrom (new FileInputStream(src) getChannel (), 0, Long.MaxValue)
  }


}
