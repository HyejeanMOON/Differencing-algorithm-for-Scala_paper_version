package ca.uwaterloo.scalacg.utils_for_cfg

trait GlobalConstant {

  //-----------------
  //ASTToken

  var ast:String = ""


  //-----------------
  //CFG

  var cfg:Array[String] = Array()



  //-----------------
  //statics

  var cfgNodeCount = 0
  var classCount = 0
  var objectCount = 0
  var traitCount = 0
  var methodCount = 0


  //-----------------

  var CallerToCallee:Array[(String,String)] = Array()

}
