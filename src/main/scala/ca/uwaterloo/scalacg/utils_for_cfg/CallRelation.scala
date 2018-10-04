package ca.uwaterloo.scalacg.utils_for_cfg


import java.io.{PrintStream,PrintWriter}


trait CallRelation extends GlobalConstant{

  def CallRelationToFile:Unit={

    val crpw = new PrintWriter("CallRelation.txt")
    val crps = new PrintStream("CallRelation.txt")

    for(cc <- CallerToCallee){

      crps.println(cc._1 + " -> " + cc._2)

    }

  }

}
