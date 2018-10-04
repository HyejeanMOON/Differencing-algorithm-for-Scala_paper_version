package ca.uwaterloo.scalacg.AST



trait HammockMethod extends NodeMethod{


  var hammockID:Int = 0

  class Hammock extends Node(node = "Hammock"){

    var hbID = hammockID
    hammockID += 1

    override def getID: Int = {

      hbID

    }

    override def getNodeString: String = {

      val NODE = "Hammock_" + hbID

      NODE

    }

  }

}

