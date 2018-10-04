package ca.uwaterloo.scalacg.utils_for_cfg

import java.io.{PrintStream, PrintWriter}


trait HammockToFile extends Collector{

  def CreateHammockFile ={

    val pwml = new PrintWriter("DataInMainline.txt")
    val pwhb = new PrintWriter("DataInHammockBlock.txt")

    val outml = new PrintStream("DataInMainline.txt")
    val outhb = new PrintStream("DataInHammockBlock.txt")


    for(m <- mainLine){

      outml.println(m)

    }

    for(hb <- hammockBlock){

      for(h <- hb){

        outhb.println(h)

      }

      outhb.println("END")

    }

    outml.close()
    outhb.close()


  }


}
