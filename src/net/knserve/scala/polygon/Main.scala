package net.knserve.scala.polygon

import java.awt._
import java.awt.event._
import scala.collection.immutable.List

object Main {
	
	def main(args: Array[String]): Unit = {
		val drw = new Draw
		val thr = new Thread( drw )
		thr.start
	}

}