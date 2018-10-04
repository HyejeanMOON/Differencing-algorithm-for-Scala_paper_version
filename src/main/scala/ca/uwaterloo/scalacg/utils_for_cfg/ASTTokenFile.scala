package ca.uwaterloo.scalacg.utils_for_cfg

import java.io.{PrintStream,PrintWriter}


trait ASTTokenFile extends GlobalConstant{

  def CreateASTTokenFile:Unit={

    val pwatf = new PrintWriter("ASTToken.txt")
    val psatf = new PrintStream("ASTToken.txt")

    psatf.println(ast)

    psatf.close()

  }

}
