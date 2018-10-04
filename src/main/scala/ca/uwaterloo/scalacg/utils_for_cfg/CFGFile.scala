package ca.uwaterloo.scalacg.utils_for_cfg

import java.io.{PrintStream, PrintWriter}


trait CFGFile extends GlobalConstant{

  cfg = cfg :+ "digraph{"

  def CreateCFGFile ={

    val pwcff = new PrintWriter("ControlFlowGraph.txt")

    val pscff = new PrintStream("ControlFlowGraph.txt")

    cfg = cfg :+ "}"

    for(c <- cfg){

      pscff.println(c)

    }

    pscff.close()

  }


}
