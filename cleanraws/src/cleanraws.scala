import java.io.File

object RawCleaner {

  def main(args: Array[String]) {
    val directory = InputHandler.getDirectory(args);
    println("Working directory: " + directory)
    Purger.purgeDirectory(directory);
  }

  private def checkProceed {
    println("Proceed? [Y/N]")
    val standardIn = System.console()
    val answer = standardIn.readLine();
    if (answer != "Y") {
      println("Aborted!")
      sys.exit();
    }
  }

  private object InputHandler {

    def getDirectory(args: Array[String]): File = {
      val dir = new File(getDirectoryPath(args))
      if (dir.isDirectory())
        dir
      else {
        println(dir + " is not a directory. Aborting.")
        sys.exit()
      }
    }

    private def getDirectoryPath(args: Array[String]): String = {
      if (args.isEmpty) currentDirectory else currentDirectory + "/" + args(0)
    }

    private def currentDirectory = System.getProperty("user.dir");

  }

  private object Purger {

    def purgeDirectory(directory: File) {
      val allPictureFiles = directory.listFiles().filter(isPictureFile)
      val filesToDelete = getFilesToDelete(allPictureFiles)
      if(filesToDelete.isEmpty)
        println("There are no raws to delete. Finished")
      else {
    	  println("Raws that will be deleted are marked red. Proceed?")
    	  printOverview(allPictureFiles, filesToDelete)
    	  checkProceed
    	  filesToDelete.values.foreach(_.delete());
    	  println("Deleted " + filesToDelete.size + " raw files. Finished.")
      }
    }

    private def isPictureFile(f: File) = {
      isFileType(f, ".jpg") || isFileType(f, ".nef")
    }

    private def isFileType(f: File, fileExtension: String) = {
      f.getName().toLowerCase().endsWith(fileExtension);
    }

    private def getNameWithoutExtension(filename: String) = {
      filename.substring(0, filename.lastIndexOf("."))
    }

    private def getFilesToDelete(allPictureFiles: Array[File]) = {
      val (jpgs, nefs) = allPictureFiles.partition(f => isFileType(f, ".jpg"))
      val jpgNameToFile = jpgs map { t => (getNameWithoutExtension(t.getName()), t) } toMap
      val nefNameToFile = nefs map { t => (getNameWithoutExtension(t.getName()), t) } toMap

      for { x <- nefNameToFile if (!jpgNameToFile.contains(x._1)) } yield x
    }

    def printOverview(allPictureFiles: Array[File], filesToDelete: Map[String, File]) {
      val output = allPictureFiles.map(f =>
        if (filesToDelete.contains(getNameWithoutExtension(f.getName()))) "\u001B[31m" + f.getName()
        else "\u001B[0m" + f.getName())
      println(output.mkString("\n") + "\u001B[0m")
    }
    
  }
  
}
