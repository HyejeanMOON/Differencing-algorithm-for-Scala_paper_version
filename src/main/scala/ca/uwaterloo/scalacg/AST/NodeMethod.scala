package ca.uwaterloo.scalacg.AST

import ca.uwaterloo.scalacg.utils_for_cfg.{CFGFile, GlobalConstant}
import scala.collection.mutable.Set


trait NodeMethod extends GlobalConstant with CFGFile{

  var positionInNodeOfList:Int = 0
  var ID:Int = 0
  var nodeOfList = Array[Node]()
  
  class Node(node:String) {

    val position = positionInNodeOfList
    positionInNodeOfList += 1
    val IDofNode = ID
    ID += 1

    final def graphing:String = {

      val NODE = "\"" + getLastNode.toString +"__"+ getLastNode.getID + "\"" + " -> "+
        "\"" + node +"__" + IDofNode + "\"" + ";"

      NODE
    }

    final def specificLastNodeGraphing(lastNode:Node):String ={

      val NODE = "\"" + lastNode.toString +"__"+ lastNode.getID + "\"" + " -> "+
        "\"" + node +"__" + IDofNode + "\"" + ";"

      NODE

    }

    final def getPosition:Int ={
      position
    }

    override def toString:String={
      node + "__" + getID
    }

    def getNodeString:String = {
      node
    }


    def getID:Int ={
      IDofNode
    }

    final def getLastNode:Node ={
      nodeOfList.take(position).last
    }

  }

  final def SearchInNodeOfList(str:String):Option[Node] ={

    val node = nodeOfList.find(_.getNodeString == str)
    node

  }

  final def SearchInNodeOfListByID(ID:Int):Option[Node] ={

    val node = nodeOfList.find(_.getID == ID)
    node

  }

  def SaveNode(node:Node)={

    nodeOfList = nodeOfList :+ node

  }

}
