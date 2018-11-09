import sbt._

import java.io.File
import org.apache.commons.vfs2.FileObject
import org.apache.commons.vfs2.FileSystemException
import org.apache.commons.vfs2.FileSystemOptions
import org.apache.commons.vfs2.Selectors
import org.apache.commons.vfs2.impl.StandardFileSystemManager
import org.apache.commons.vfs2.provider.sftp.SftpFileSystemConfigBuilder

/**
  * The class SFTPUtil containing uploading, downloading, checking if file exists
  * and deleting functionality using Apache Commons VFS (Virtual File System)
  * Library
  *
  * @author Ashok
  *
  */
object SftpUtility {
  def main(args: Array[String]): Unit = {
    val hostName           = "PutYourHostNameHere"
    val username           = "PutYourUserNameForHostHere"
    val password           = "PutYourPasswordForHostHere"
    val localFilePath      = "C:\\FakePath\\FakeFile.txt"
    val remoteFilePath     = "/FakeRemotePath/FakeRemoteFile.txt"
    val remoteTempFilePath = "/FakeRemoteTempPath/FakeRemoteTempFile.txt"
    upload(hostName, username, password, localFilePath, remoteFilePath)
    exist(hostName, username, password, remoteFilePath)
    download(hostName, username, password, localFilePath, remoteFilePath)
    move(hostName, username, password, remoteFilePath, remoteTempFilePath)
    delete(hostName, username, password, remoteFilePath)
  }

  /**
    * Method to upload a file in Remote server
    *
    * @param hostName
    *            HostName of the server
    * @param username
    *            UserName to login
    * @param password
    *            Password to login
    * @param localFilePath
    *            LocalFilePath. Should contain the entire local file path -
    *            Directory and Filename with \\ as separator
    * @param remoteFilePath
    *            remoteFilePath. Should contain the entire remote file path -
    *            Directory and Filename with / as separator
    */
  def upload(hostName: String,
             username: String,
             password: String,
             localFilePath: String,
             remoteFilePath: String): Unit = {
    val file = new File(localFilePath)
    if (!file.exists) throw new RuntimeException("Error. Local file not found")
    val manager = new StandardFileSystemManager
    try {
      manager.init()
      // Create local file object
      val localFile = manager.resolveFile(file.getAbsolutePath)
      // Create remote file object
      val remoteFile =
        manager.resolveFile(createConnectionString(hostName, username, password, remoteFilePath), createDefaultOptions)
      /*
       * use createDefaultOptions() in place of fsOptions for all default
       * options - Ashok.
       */ // Copy local file to sftp server
      remoteFile.copyFrom(localFile, Selectors.SELECT_SELF)
      System.out.println("File upload success")
    } catch {
      case e: Exception =>
        throw new RuntimeException(e)
    } finally manager.close()
  }

  def move(hostName: String,
           username: String,
           password: String,
           remoteSrcFilePath: String,
           remoteDestFilePath: String): Boolean = {
    val manager = new StandardFileSystemManager
    try {
      manager.init()
      // Create remote object
      val remoteFile = manager.resolveFile(createConnectionString(hostName, username, password, remoteSrcFilePath),
                                           createDefaultOptions)
      val remoteDestFile = manager.resolveFile(createConnectionString(hostName, username, password, remoteDestFilePath),
                                               createDefaultOptions)
      if (remoteFile.exists) {
        remoteFile.moveTo(remoteDestFile)

        System.out.println("Move remote file success")
        true
      } else {
        System.out.println("Source file doesn't exist")
        false
      }
    } catch {
      case e: Exception =>
        throw new RuntimeException(e)
    } finally manager.close()
  }

  /**
    * Method to download the file from remote server location
    *
    * @param hostName
    *            HostName of the server
    * @param username
    *            UserName to login
    * @param password
    *            Password to login
    * @param localFilePath
    *            LocalFilePath. Should contain the entire local file path -
    *            Directory and Filename with \\ as separator
    * @param remoteFilePath
    *            remoteFilePath. Should contain the entire remote file path -
    *            Directory and Filename with / as separator
    */
  def download(hostName: String,
               username: String,
               password: String,
               localFilePath: String,
               remoteFilePath: String): Unit = {
    val manager = new StandardFileSystemManager
    try {
      manager.init()
      val localFile = manager.resolveFile(localFilePath)
      val remoteFile =
        manager.resolveFile(createConnectionString(hostName, username, password, remoteFilePath), createDefaultOptions)
      localFile.copyFrom(remoteFile, Selectors.SELECT_SELF)
      System.out.println("File download success")
    } catch {
      case e: Exception =>
        throw new RuntimeException(e)
    } finally manager.close()
  }

  def createFolder(hostName: String, username: String, password: String, remoteFilePath: String): Unit = {
    val manager = new StandardFileSystemManager
    try {
      manager.init()
      // Append _downlaod_from_sftp to the given file name.
      //String downloadFilePath = localFilePath.substring(0, localFilePath.lastIndexOf(".")) + "_downlaod_from_sftp" + localFilePath.substring(localFilePath.lastIndexOf("."), localFilePath.length());
      // Create local file object. Change location if necessary for new downloadFilePath
      val remoteFile =
        manager.resolveFile(createConnectionString(hostName, username, password, remoteFilePath), createDefaultOptions)
      remoteFile.createFolder()
      System.out.println("Folder creation success")
    } catch {
      case e: Exception =>
        throw new RuntimeException(e)
    } finally manager.close()
  }

  /**
    * Method to delete the specified file from the remote system
    *
    * @param hostName
    *            HostName of the server
    * @param username
    *            UserName to login
    * @param password
    *            Password to login
    * @param localFilePath
    *            LocalFilePath. Should contain the entire local file path -
    *            Directory and Filename with \\ as separator
    * @param remoteFilePath
    *            remoteFilePath. Should contain the entire remote file path -
    *            Directory and Filename with / as separator
    */
  def delete(hostName: String, username: String, password: String, remoteFilePath: String): Unit = {
    val manager = new StandardFileSystemManager
    try {
      manager.init()
      val remoteFile =
        manager.resolveFile(createConnectionString(hostName, username, password, remoteFilePath), createDefaultOptions)
      if (remoteFile.exists) {
        remoteFile.delete
        System.out.println("Delete remote file success")
      }
    } catch {
      case e: Exception =>
        throw new RuntimeException(e)
    } finally manager.close()
  }

  /**
    * Method to check if the remote file exists in the specified remote
    * location
    *
    * @param hostName
    *            HostName of the server
    * @param username
    *            UserName to login
    * @param password
    *            Password to login
    * @param remoteFilePath
    *            remoteFilePath. Should contain the entire remote file path -
    *            Directory and Filename with / as separator
    * @return Returns if the file exists in the specified remote location
    */
  def exist(hostName: String, username: String, password: String, remoteFilePath: String): Boolean = {
    val manager = new StandardFileSystemManager
    try {
      manager.init()
      val remoteFile =
        manager.resolveFile(createConnectionString(hostName, username, password, remoteFilePath), createDefaultOptions)
      System.out.println("File exist: " + remoteFile.exists)
      remoteFile.exists
    } catch {
      case e: Exception =>
        throw new RuntimeException(e)
    } finally manager.close()
  }

  /**
    * Generates SFTP URL connection String
    *
    * @param hostName
    *            HostName of the server
    * @param username
    *            UserName to login
    * @param password
    *            Password to login
    * @param remoteFilePath
    *            remoteFilePath. Should contain the entire remote file path -
    *            Directory and Filename with / as separator
    * @return concatenated SFTP URL string
    */
  def createConnectionString(hostName: String, username: String, password: String, remoteFilePath: String): String =
    "sftp://" + username + ":" + password + "@" + hostName + "/" + remoteFilePath

  /**
    * Method to setup default SFTP config
    *
    * @return the FileSystemOptions object containing the specified
    *         configuration options
    * @throws FileSystemException
    */ @throws[FileSystemException]
  def createDefaultOptions: FileSystemOptions = { // Create SFTP options
    val opts = new FileSystemOptions
    // SSH Key checking
    SftpFileSystemConfigBuilder.getInstance.setStrictHostKeyChecking(opts, "no")
    /*
     * Using the following line will cause VFS to choose File System's Root
     * as VFS's root. If I wanted to use User's home as VFS's root then set
     * 2nd method parameter to "true"
     */ // Root directory set to user home
    SftpFileSystemConfigBuilder.getInstance.setUserDirIsRoot(opts, false)
    // Timeout is count by Milliseconds
    SftpFileSystemConfigBuilder.getInstance.setTimeout(opts, 10000)
    opts
  }
}
