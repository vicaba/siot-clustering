package test

import scala.collection.JavaConverters._

class TableList(descriptions: List[String]) {
  private val javaTableList = new JavaTableList(descriptions.asJava)

  def addRow(elements: List[String]): Unit = javaTableList.addRow(elements.asJava)

  def print(numOfTabs: Int = 0): Unit = println(javaTableList.toString(numOfTabs))

  def toString(numOfTabs: Int = 0): String = javaTableList.toString(numOfTabs)
}
