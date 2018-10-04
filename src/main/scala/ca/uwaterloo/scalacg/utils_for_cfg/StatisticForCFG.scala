package ca.uwaterloo.scalacg.utils_for_cfg

import ca.uwaterloo.scalacg.config.Statistics
import java.io._



trait StatisticForCFG extends Statistics{


  var COTCount = 0


  def StatisticToFile(hammockCount:Int,nodeCount:Int):Unit ={

    val spw = new PrintWriter("Statistic_for_CFG.txt")
    val sps = new PrintStream("Statistic_for_CFG.txt")

    sps.println("Statistic For Control Flow Graph:")
    sps.println("-----------------------------------")
    sps.println("")

    sps.print("COT Count: ")
    sps.println(COTCount)

    sps.print("Node Count: ")
    sps.println(nodeCount)

    sps.print("Hammock Count: ")
    sps.println(hammockCount)

  }

}
