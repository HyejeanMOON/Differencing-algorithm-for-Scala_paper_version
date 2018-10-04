package ca.uwaterloo.scalacg.utils_for_cfg

import ca.uwaterloo.scalacg.AST.NodeMethod


trait Collector extends NodeMethod{

  var isHammock = false

  var hammockBlock:Array[Array[Node]] = Array()
  var mainLine:Array[Node] = Array()
  var tempOfHammockBlock:Array[Node] = Array()

  def CollectionOfMainline(node:Node) ={

    mainLine = mainLine :+ node

  }

  def CollectionOfTempHammockBlock(node:Node) ={

    tempOfHammockBlock = tempOfHammockBlock :+ node

  }

  def ResetTemphammockBloc ={

    tempOfHammockBlock = Array()

  }

  def CollectionOfHammockBlock ={

    hammockBlock = hammockBlock :+ tempOfHammockBlock

    ResetTemphammockBloc

  }


}
