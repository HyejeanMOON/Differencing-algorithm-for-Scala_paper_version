package ca.uwaterloo.scalacg.AST


import ca.uwaterloo.scalacg.config.CallGraphCollections
import ca.uwaterloo.scalacg.util.TreeTraversal
import ca.uwaterloo.scalacg.utils_for_cfg._
import probe.{CallEdge, CallGraph}

import scala.reflect.io.AbstractFile




trait AST_Traverse extends TreeTraversal with Collector with NodeMethod with HammockMethod with CallGraphCollections
  with ASTTokenFile with Extractor with HammockToFile with StatisticForCFG with CallRelation {

  import global._

  import StructuralExtractors._
  import ExpressionExtractors._

  var node: String = ""
  var entryNode: String = ""
  var exitNode: String = ""
  var condString: String = ""
  var level = 0
  var ASlevel = 0
  var ASNode = ""


  def BuildCFG: Unit = {


    //trees.foreach(ast_traverse(_))   // MOON


    for (t <- trees) {

      ast = ast + showRaw(t)

    }
    //--------------------------------
/*
    for {
      callSite <- callGraph.keys
      source = callSite.enclMethod
      target <- callGraph(callSite)
      sourceFile = if (callSite.position.isDefined) relativize(callSite.position.source.file) else "unknown"

    } {

      val isSourceApp = isApplication(source)
      val isTargetApp = isApplication(target)





    }
*/
    /*
    for(cc <- CallerToCallee){

      println("CS " + cc._1 + " -> " + cc._2)

    }




*/


    //------------------
    println("")
    println("by MOON")
    println("")
    println("Creating AST token file...")
    CreateASTTokenFile
    println("Creating Hammock file...")
    CreateHammockFile
    println("Creating Call Relation file...")
    CallRelationToFile
    println("Creating Statistic for CFG file...")

    var NodeCount = nodeOfList.length
    var HammockCount = hammockID

    StatisticToFile(HammockCount, NodeCount)


  }

  /**
    * Get the relative path for an absolute source file path.
    */
  private def relativize(file: AbstractFile) = {
    file.toString.replaceFirst(".+/build_src/[^/]+/", "")
  }

  /**
    * Get the relative path for an absolute source file path.
    */
  private def relativize(absolute: String) = {
    absolute.replaceFirst(".+/build_src/[^/]+/", "")
  }





  def ast_traverse(tree: Tree): Unit = {

      tree match {

        case p@PackageDef(pids, stats) =>
          //不为包设置Node

          stats.foreach(ast_traverse(_))


        case c@ClassDef(mods, name, tparams, impl) if !name.toString.contains("anonfun")  =>

          level += 1
          COTCount += 1

          if (mods.toString().contains("module")) {

            entryNode = c.symbol.pkg + "." + name.toString + ".ObjectEntry"
            exitNode = c.symbol.pkg + "." + name.toString + ".ObjectExit"

          }
          else if (mods.isTrait) {

            entryNode = c.symbol.pkg + "." + name.toString + ".TraitEntry"
            exitNode = c.symbol.pkg + "." + name.toString + ".TraitExit"

          } else {

            if (c.symbol.isAbstractClass) {

              entryNode = c.symbol.pkg + "." + name.toString + ".AbstractClassEntry"
              exitNode = c.symbol.pkg + "." + name.toString + ".AbstractClassExit"

            } else if (c.symbol.isCaseClass) {

              entryNode = c.symbol.pkg + "." + name.toString + ".CaseClassEntry"
              exitNode = c.symbol.pkg + "." + name.toString + ".CaseClassExit"

            } else if (c.symbol.isClass) {

              entryNode = c.symbol.pkg + "." + name.toString + ".ClassEntry"
              exitNode = c.symbol.pkg + "." + name.toString + ".ClassExit"

            }

          }

          val ExitNode = exitNode

          val ENTRY = new Node(entryNode)


          if (level == 1) {

            CollectionOfMainline(ENTRY)
            SaveNode(ENTRY)

            isHammock = true

            c.children.foreach(ast_traverse(_))

            isHammock = false

            val EXIT = new Node(ExitNode)
            CollectionOfMainline(EXIT)
            SaveNode(EXIT)


          }
          else if (level != 1 && isHammock) {

            CollectionOfTempHammockBlock(ENTRY)
            SaveNode(ENTRY)

            if (level == 2) {

              val hammockNode = new Hammock
              CollectionOfMainline(hammockNode)

            }


            c.children.foreach(ast_traverse(_))

            val EXIT = new Node(ExitNode)
            CollectionOfTempHammockBlock(EXIT)
            SaveNode(EXIT)


          }

          if (level == 1) {

            CollectionOfHammockBlock

          }

          level -= 1


/*
      case  c @ ClassDef(mods,name,tparams,impl) if  mods.isSynthetic =>

        c.children.foreach(ast_traverse(_))  //OK
*/

        case t@Template(parents, self, body) =>
          /*
        for(p <- parents){

          node = p.toString()
          if(level == 1){
            val NODE = new Node(node)
            CollectionOfMainline(NODE)
            SaveNode(NODE)
          }else{
            val NODE = new Node(node)
            CollectionOfTempHammockBlock(NODE)
            SaveNode(NODE)
          }

        }
*/
          body.foreach(ast_traverse(_))


        case d@DefDef(mods, name, tparams, vparamss, tpt, rhs) if !name.toString.contains("init")
          && !d.symbol.isPrimaryConstructor && !mods.isDeferred && !mods.isFinal && !mods.isSynthetic && !mods.toString().contains("accessor") && !d.symbol.isAnonymousFunction =>

          level += 1

          if (mods.isOverride) {

            entryNode = d.symbol.pkg + "." + d.symbol.cls + "." + name.toString + ".Override.defEntry"
            exitNode = d.symbol.pkg + "." + d.symbol.cls + "." + name.toString + ".Override.defExit"

          } else {

            entryNode = d.symbol.pkg + "." + d.symbol.cls + "." + name.toString + ".defEntry"
            exitNode = d.symbol.pkg + "." + d.symbol.cls + "." + name.toString + ".defExit"

          }

          val ExitNode = exitNode

/*
          if (level == 1) {

            val ENTRY = new Node(entryNode)
            CollectionOfMainline(ENTRY)
            SaveNode(ENTRY)


            for (v <- vparamss) {

              val vpName = "InputParams." + v.toString()
              val vpNode = new Node(vpName)
              CollectionOfTempHammockBlock(vpNode)
              SaveNode(vpNode)

            }


            val tptName = "OutputParams." + tpt.symbol.name.toString
            val tptNode = new Node(tptName)
            CollectionOfTempHammockBlock(tptNode)
            SaveNode(tptNode)


            val hammockNode = new Hammock
            CollectionOfMainline(hammockNode)


            isHammock = true

            rhs.children.foreach(ast_traverse(_)) //OK

            isHammock = false

            val EXIT = new Node(ExitNode)
            CollectionOfMainline(EXIT)
            SaveNode(EXIT)

          }*/


            val ENTRY = new Node(entryNode)
            CollectionOfTempHammockBlock(ENTRY)
            SaveNode(ENTRY)

/*
            for (v <- vparamss) {

              val vpName = "InputParams." + v.toString()
              val vpNode = new Node(vpName)
              CollectionOfTempHammockBlock(vpNode)
              SaveNode(vpNode)

            }

            val tptName = "OutputParams." + tpt.symbol.name.toString
            val tptNode = new Node(tptName)
            CollectionOfTempHammockBlock(tptNode)
            SaveNode(tptNode)

*/

            val hammockNode = new Hammock
            CollectionOfMainline(hammockNode)


            rhs.children.foreach(ast_traverse(_)) //OK

            val EXIT = new Node(ExitNode)
            CollectionOfTempHammockBlock(EXIT)
            SaveNode(EXIT)

            CollectionOfHammockBlock



          level -= 1


        case DefDef(mods,name,tparams,vparamss,tpt,rhs) if  mods.isSynthetic=>

          rhs.children.foreach(ast_traverse(_))


        case Block(stats, expr) =>
          stats.foreach(ast_traverse(_))

          if (!expr.isEmpty) {

            expr.children.foreach(ast_traverse(_))

          }


        case LabelDef(name, _, rhs) =>
          ASNode = name.toString

          rhs.children.foreach(ast_traverse(_))

        case v @ ValDef(mods, termName, _, rhs) if !mods.toString().contains("syn") =>

          ASNode = ""
          rhs.children.foreach(ast_traverse(_))


          if (!ASNode.contains("new")) {

            if (mods.isMutable) {

              node = "Mutable." + termName.toString + "." + ASNode

            } else {

              node = termName.toString + "." + ASNode

            }

            if (!isHammock) {

              val NODE = new Node(node)

              CollectionOfMainline(NODE)
              SaveNode(NODE)

            } else {

              val NODE = new Node(node)

              CollectionOfTempHammockBlock(NODE)
              SaveNode(NODE)

            }

          } else {

            val NODE = new Node(node)

            if (!isHammock) {

              CollectionOfMainline(NODE)
              SaveNode(NODE)

            } else {

              CollectionOfTempHammockBlock(NODE)
              SaveNode(NODE)

            }

          }


        case i@If(cond, thenp, elsep) =>

          entryNode = "ifEntry"
          exitNode = "ifExit"

          val ENTRY = new Node(entryNode)


          /*
        val condString = "cond." //+ Decompose(cond)
        val COND = new Node(condString)*/
          val EXIT = new Node(exitNode)


          if (!isHammock) {

            CollectionOfMainline(ENTRY)
            SaveNode(ENTRY)
            /*
          CollectionOfMainline(COND)
          SaveNode(COND)
*/
            thenp.children.foreach(ast_traverse(_))
            elsep.children.foreach(ast_traverse(_))

            CollectionOfMainline(EXIT)
            SaveNode(EXIT)

          } else {

            CollectionOfTempHammockBlock(ENTRY)
            SaveNode(ENTRY)
            /*
          CollectionOfTempHammockBlock(COND)
          SaveNode(COND)
*/
            thenp.children.foreach(ast_traverse(_))
            elsep.children.foreach(ast_traverse(_))

            CollectionOfTempHammockBlock(EXIT)
            SaveNode(EXIT)

          }

        case ExWhile(cond, stats) =>

          entryNode = "whileEntry"
          exitNode = "whileExit"

          val ENTRY = new Node(entryNode)
          val EXIT = new Node(exitNode)

          ASNode = ""
          cond.children.foreach(ast_traverse(_))

          val condStr = "cond." + ASNode
          val COND = new Node(condStr)

          if (!isHammock) {

            CollectionOfMainline(ENTRY)
            SaveNode(ENTRY)
            CollectionOfMainline(COND)
            SaveNode(COND)

            stats.foreach(ast_traverse(_))

            CollectionOfMainline(EXIT)
            SaveNode(EXIT)

          } else {

            CollectionOfTempHammockBlock(ENTRY)
            SaveNode(ENTRY)
            CollectionOfTempHammockBlock(COND)
            SaveNode(COND)

            stats.foreach(ast_traverse(_))

            CollectionOfTempHammockBlock(EXIT)
            SaveNode(EXIT)

          }
        /*
      case Apply(Select(Ident(caller),callee),rhs)  =>
        ASNode = caller.toString + "." + callee.toString
        rhs.foreach(ast_traverse(_))

        val NODE = new Node(ASNode)

        if(!isHammock){

          CollectionOfMainline(NODE)
          SaveNode(NODE)

        }else{

          CollectionOfTempHammockBlock(NODE)
          SaveNode(NODE)

        }
*/
        case t@Throw(expr) =>

          entryNode = "throwEntry"
          exitNode = "throwExit"

          val ENTRY = new Node(entryNode)
          val EXIT = new Node(exitNode)

          if (!isHammock) {

            CollectionOfMainline(ENTRY)
            SaveNode(ENTRY)

            expr.children.foreach(ast_traverse(_)) //OK

            CollectionOfMainline(EXIT)
            SaveNode(EXIT)

          } else {

            CollectionOfTempHammockBlock(ENTRY)
            SaveNode(ENTRY)

            expr.children.foreach(ast_traverse(_)) //OK

            CollectionOfTempHammockBlock(EXIT)
            SaveNode(EXIT)

          }


        case ExPredefPrint("print", rhs) =>
          val p = "scala.Predef.print"
          //rhs.foreach(ast_traverse(_))
          //ASNode = ""
          rhs.foreach(ast_traverse(_))

          node = p + ASNode

          val NODE = new Node(node)

          if (!isHammock) {

            CollectionOfMainline(NODE)

          } else {

            CollectionOfTempHammockBlock(NODE)

          }
          SaveNode(NODE)

        case ExPredefPrint("println", rhs) =>
          val p = "scala.Predef.println"
          //rhs.foreach(ast_traverse(_))

          //ASNode = ""
          rhs.foreach(ast_traverse(_))

          node = p + ASNode

          val NODE = new Node(node)

          if (!isHammock) {

            CollectionOfMainline(NODE)

          } else {

            CollectionOfTempHammockBlock(NODE)

          }
          SaveNode(NODE)

        case ExPredefPrint("printf", rhs) =>
          val p = "scala.Predef.printf"
          //rhs.foreach(ast_traverse(_))

          //ASNode = ""
          rhs.foreach(ast_traverse(_))

          node = p + ASNode

          val NODE = new Node(node)

          if (!isHammock) {

            CollectionOfMainline(NODE)

          } else {

            CollectionOfTempHammockBlock(NODE)

          }
          SaveNode(NODE)


        case f@ExForeach(cond, block) =>

          entryNode = "forEntry"
          exitNode = "forExit"

          val ENTRY = new Node(entryNode)
          val EXIT = new Node(exitNode)

          val condStr = "cond." + cond

          val COND = new Node(condStr)

          if (!isHammock) {

            CollectionOfMainline(ENTRY)
            SaveNode(ENTRY)
            CollectionOfMainline(COND)
            SaveNode(COND)

            block.foreach(ast_traverse(_))

            CollectionOfMainline(EXIT)
            SaveNode(EXIT)

          } else {

            CollectionOfTempHammockBlock(ENTRY)
            SaveNode(ENTRY)
            CollectionOfTempHammockBlock(COND)
            SaveNode(COND)

            block.foreach(ast_traverse(_))

            CollectionOfTempHammockBlock(EXIT)
            SaveNode(EXIT)

          }


        case e@ExDoWhile(cond, stats) =>

          entryNode = "doWhileEntry"
          exitNode = "doWhileExit"


          val ENTRY = new Node(entryNode)

          //ASNode = ""
          cond.children.foreach(ast_traverse(_))
          val condStr = "cond." + ASNode

          val COND = new Node(condStr)

          val EXIT = new Node(exitNode)


          if (!isHammock) {

            CollectionOfMainline(ENTRY)
            SaveNode(ENTRY)
            CollectionOfMainline(COND)
            SaveNode(COND)

            stats.foreach(ast_traverse(_))

            CollectionOfMainline(EXIT)
            SaveNode(EXIT)

          } else {

            CollectionOfTempHammockBlock(ENTRY)
            SaveNode(ENTRY)
            CollectionOfTempHammockBlock(COND)
            SaveNode(COND)

            stats.foreach(ast_traverse(_))

            CollectionOfTempHammockBlock(EXIT)
            SaveNode(EXIT)

          }


        case Match(selector, cases) =>

          entryNode = "matchEntry"
          exitNode = "matchExit"


          val ENTRY = new Node(entryNode)

          if (!isHammock) {

            CollectionOfMainline(ENTRY)
            SaveNode(ENTRY)

            selector.children.foreach(ast_traverse(_))

            for (cs <- cases) {

              cs match {

                case c@CaseDef(pat, guard, body) =>
                  val caseEntry = "caseEntry"
                  val caseExit = "caseExit"
                  val cENTRY = new Node(caseEntry)
                  CollectionOfMainline(cENTRY)
                  SaveNode(cENTRY)


                  pat.children.foreach(ast_traverse(_))
                  guard.children.foreach(ast_traverse(_))
                  body.children.foreach(ast_traverse(_))

                  val cEXIT = new Node(caseExit)
                  CollectionOfMainline(cEXIT)
                  SaveNode(cEXIT)

                case _ =>

              }

            }

            val EXIT = new Node(exitNode)
            CollectionOfMainline(EXIT)
            SaveNode(EXIT)

          } else {

            CollectionOfTempHammockBlock(ENTRY)
            SaveNode(ENTRY)

            selector.children.foreach(ast_traverse(_))

            for (cs <- cases) {

              cs match {

                case c@CaseDef(pat, guard, body) =>
                  val caseEntry = "caseEntry"
                  val caseExit = "caseExit"
                  val cENTRY = new Node(caseEntry)
                  CollectionOfTempHammockBlock(cENTRY)
                  SaveNode(cENTRY)

                  pat.children.foreach(ast_traverse(_))
                  guard.children.foreach(ast_traverse(_))
                  body.children.foreach(ast_traverse(_))

                  val cEXIT = new Node(caseExit)
                  CollectionOfTempHammockBlock(cEXIT)
                  SaveNode(cEXIT)

              }

            }

            val EXIT = new Node(exitNode)
            CollectionOfTempHammockBlock(EXIT)
            SaveNode(EXIT)

          }


        case Try(block, catches, finalizer) =>

          entryNode = "tryEntry"
          exitNode = "tryExit"

          val ENTRY = new Node(entryNode)

          if (!isHammock) {

            CollectionOfMainline(ENTRY)
            SaveNode(ENTRY)

            block.children.foreach(ast_traverse(_))

            if (!catches.isEmpty) {

              val catcheEntryNode = "catchEntry"
              val catcheExitNode = "catchExit"

              val cENTRY = new Node(catcheEntryNode)

              CollectionOfMainline(cENTRY)
              SaveNode(cENTRY)

              for (cs <- catches) {

                cs match {

                  case c@CaseDef(_, _, body) =>
                    body.children.foreach(ast_traverse(_))

                  case _ =>

                }

              }

              val cEXIT = new Node(catcheExitNode)

              CollectionOfMainline(cEXIT)
              SaveNode(cEXIT)

            }

            if (!finalizer.isEmpty) {

              val finalizerEntryNode = "finalizerEntry"
              val finalizerExitNode = "finalizerExit"

              val fENTRY = new Node(finalizerEntryNode)
              CollectionOfMainline(fENTRY)
              SaveNode(fENTRY)

              finalizer.children.foreach(ast_traverse(_))

              val fEXIT = new Node(finalizerExitNode)
              CollectionOfMainline(fEXIT)
              SaveNode(fEXIT)

            }

            val EXIT = new Node(exitNode)
            CollectionOfMainline(EXIT)
            SaveNode(EXIT)

          } else {

            CollectionOfTempHammockBlock(ENTRY)
            SaveNode(ENTRY)

            if (!catches.isEmpty) {

              val catchEntryNode = "catchEntry"
              val cENTRY = new Node(catchEntryNode)

              CollectionOfTempHammockBlock(cENTRY)
              SaveNode(cENTRY)

              val catchExitNode = "catchExit"

              for (cs <- catches) {

                cs match {

                  case c@CaseDef(_, _, body) =>
                    body.children.foreach(ast_traverse(_))

                  case _ =>

                }

              }

              val cEXIT = new Node(catchExitNode)
              CollectionOfTempHammockBlock(cEXIT)
              SaveNode(cEXIT)

            }

            if (!finalizer.isEmpty) {

              val finalizerEntryNode = "finalizerEntry"
              val finalizerExitNode = "finalizerExit"

              val fENTRY = new Node(finalizerEntryNode)
              CollectionOfTempHammockBlock(fENTRY)
              SaveNode(fENTRY)

              finalizer.children.foreach(ast_traverse(_))


              val fEXIT = new Node(finalizerExitNode)
              CollectionOfTempHammockBlock(fEXIT)
              SaveNode(fEXIT)

            }

            val EXIT = new Node(exitNode)
            CollectionOfTempHammockBlock(EXIT)
            SaveNode(EXIT)

          }


        case Assign(lhs, rhs) =>

          lhs.children.foreach(ast_traverse(_))
          rhs.children.foreach(ast_traverse(_))

        case Function(vparams, rhs) =>

          //vparams.foreach(ast_traverse(_))

          rhs.children.foreach(ast_traverse(_))


        case Return(expr) =>
          expr.children.foreach(ast_traverse(_))


        case ArrayValue(elemtpt, elems) =>
          elemtpt.children.foreach(ast_traverse(_))
          elems.foreach(ast_traverse(_))


        case UnApply(fun, args) =>
          fun.children.foreach(ast_traverse(_))
          args.foreach(ast_traverse(_))


        case Typed(expr, tpt) =>
          expr.children.foreach(ast_traverse(_))
          tpt.children.foreach(ast_traverse(_))

        case New(tpt) =>

          ASlevel += 1

          if (ASNode != "") {

            ASNode = ASNode + "." + "new." + tpt.symbol.pkg + "." + tpt.toString()

          } else {

            ASNode = "new." + tpt.symbol.pkg + "." + tpt.toString()

          }

          ASlevel -= 1

          if (ASlevel == 0) {

            val NODE = new Node(node)

            if (!isHammock) {

              CollectionOfMainline(NODE)
              SaveNode(NODE)

            } else {

              CollectionOfTempHammockBlock(NODE)
              SaveNode(NODE)

            }

          }


        case Super(qual, mix) =>

          ASlevel += 1

          if (ASNode != "") {

            ASNode = ASNode + "." + "super." + qual.toString() + "." + mix.toString

          } else {

            ASNode = "super." + qual.toString() + "." + mix.toString

          }

          ASlevel -= 1

          if (ASlevel == 0) {

            val ASNODE = new Node(ASNode)

            if (!isHammock) {

              CollectionOfMainline(ASNODE)
              SaveNode(ASNODE)
              ASNode = ""

            } else {

              CollectionOfTempHammockBlock(ASNODE)
              SaveNode(ASNODE)
              ASNode = ""

            }

          }


        case a: Apply =>

          ASlevel += 1
          a.fun.children.foreach(ast_traverse(_))
          a.args.foreach(ast_traverse(_))

          ASlevel -= 1

          if (ASlevel == 0) {

            if (!isHammock) {

              val ASNODE = new Node(ASNode)
              CollectionOfMainline(ASNODE)
              SaveNode(ASNODE)

              ASNode = ""

            } else {

              val ASNODE = new Node(ASNode)
              CollectionOfTempHammockBlock(ASNODE)
              SaveNode(ASNODE)

              ASNode = ""

            }
          }

        case s: Select =>

          ASlevel += 1

          s.qualifier.children.foreach(ast_traverse(_))
          if (s.name.toString != "") {

            ASNode = ASNode + "." + s.name.toString

          }

          ASlevel -= 1

          if (ASlevel == 0) {


            val ASNODE = new Node(ASNode)

            if (!isHammock) {

              CollectionOfMainline(ASNODE)
              SaveNode(ASNODE)
              ASNode = ""

            } else {

              CollectionOfTempHammockBlock(ASNODE)
              SaveNode(ASNODE)
              ASNode = ""

            }

          }

        /*
      case Typed(expr,tpt) =>
        expr.children.foreach(ast_traverse(_))
        tpt.children.foreach(ast_traverse(_))
*/

        case Bind(name, body) =>
          node = name.toString

          val NODE = new Node(node)

          if (!isHammock) {

            CollectionOfMainline(NODE)
            SaveNode(NODE)

          } else {

            CollectionOfTempHammockBlock(NODE)
            SaveNode(NODE)

          }

          body.children.foreach(ast_traverse(_))


        case t: TypeApply =>
          ASlevel += 1
          //t.fun.children.foreach(ast_traverse(_))
          t.args.foreach(ast_traverse(_))
          ASlevel -= 1

        case i: Ident =>
          ASlevel += 1

          if (ASNode != "") {

            ASNode = ASNode + "." + i.toString()

          }
          if (ASNode == "") {

            ASNode = i.toString()

          }

          ASlevel -= 1

          if (ASlevel == 0) {

            val ASNODE = new Node(ASNode)

            if (!isHammock) {

              CollectionOfMainline(ASNODE)
              SaveNode(ASNODE)
              ASNode = ""

            } else {

              CollectionOfTempHammockBlock(ASNODE)
              SaveNode(ASNODE)
              ASNode = ""

            }

          }


        case t@This(qual) =>

          ASlevel += 1

          //println("This " + qual.toString)

          if (ASNode != "") {

            ASNode = ASNode + "." + qual.toString

          }
          if (ASNode == "") {

            ASNode = qual.toString

          }

          ASlevel -= 1

          if (ASlevel == 0) {

            val ASNODE = new Node(ASNode)

            if (!isHammock) {

              CollectionOfMainline(ASNODE)
              SaveNode(ASNODE)
              ASNode = ""

            } else {

              CollectionOfTempHammockBlock(ASNODE)
              SaveNode(ASNODE)
              ASNode = ""

            }

          }


        case Literal(str) =>

          ASlevel += 1

          if (ASNode != "") {

            ASNode = ASNode + "." + str.toString()

          }
          if (ASNode == "") {

            ASNode = str.toString()

          }

          ASlevel -= 1

          if (ASlevel == 0) {

            val ASNODE = new Node(ASNode)

            if (!isHammock) {

              CollectionOfMainline(ASNODE)
              SaveNode(ASNODE)
              ASNode = ""

            } else {

              CollectionOfTempHammockBlock(ASNODE)
              SaveNode(ASNODE)
              ASNode = ""

            }

          }


        case EmptyTree =>

        case _ =>


      }
    }



  /*


  def Decompose(cond:Tree):String = {

    cond match{



      case ExAnd(lhs,rhs) =>
        lhs.children.foreach(Decompose(_))
        condString = condString + ".And"
        rhs.children.foreach(Decompose(_))

      case ExOr(lhs,rhs) =>
        lhs.children.foreach(Decompose(_))
        condString = condString + ".Or"
        rhs.children.foreach(Decompose(_))

      case ExNot(t) =>

        if(condString != ""){
          condString = condString + "Not."
        }else{
          condString = "Not."
        }

        t.children.foreach(Decompose(_))

      case ExGreater(s) =>
        if(condString != ""){
          condString = condString + "." + s
        }else{
          condString = s
        }



      case ExLess(s) =>
        if(condString != ""){
          condString = condString + "." + s
        }else{
          condString = s
        }


      case ExGreaterEq(s) =>
        if(condString != ""){
          condString = condString + "." + s
        }else{
          condString = s
        }


      case ExLessEq(s) =>
        if(condString != ""){
          condString = condString + "." + s
        }else{
          condString = s
        }


      case ExEQEQorBANGEQ(s) =>
        if(condString != ""){
          condString = condString + "." + s
        }else{
          condString = s
        }


      case Select(Ident(tartgetName),termName) =>
        if(condString != ""){
          condString = condString + "." + tartgetName.toString + "." + termName.toString
        }else{
          condString = tartgetName.toString + "." + termName.toString
        }

      case ExRhsOfValDef(s) =>
        if(condString != ""){

          condString = condString + "." + s

        }else{

          condString = s

        }


      case New(tpt) =>
        if(condString != ""){

          condString =  condString + "." + "new." + tpt.symbol.pkg + "." + tpt.toString()

        }else{

          condString = "new." + tpt.symbol.pkg + "." + tpt.toString()

        }


      case Literal(num)  =>
        if(condString != ""){

          condString = condString + "." + num.toString

        }else{

          condString = num.toString

        }


      case a: Apply =>
        a.fun.children.foreach(Decompose(_))
        if(a.args.nonEmpty){
          a.args.foreach(Decompose(_))
        }


      case s: Select =>
        s.qualifier.children.foreach(Decompose(_))

        if(s.name.toString != ""){

          if(condString != ""){

            condString = condString + s.name.toString

          }else{

            condString = s.name.toString

          }

        }

        val cs = ResearchCallSites(condString)

        if(cs.nonEmpty){

          for(c <- cs){

            condString = condString + "-" + c

          }

        }
/*
      case t: TypeApply =>
        //t.fun.children.foreach(Decompose(_))
        t.args.foreach(Decompose(_))
*/
      case i: Ident =>

        if(condString != ""){

          condString =  condString + "." + i.toString()

        }else{

          condString = i.toString()

        }


      case t: This =>

        if(condString != ""){

          condString = condString + "." + t.qual.toString

        }else{

          condString = t.qual.toString

        }



       /*
      case Apply(Select(constantName,newTermName),List(Ident(termName))) =>
        //Apply(Select(Literal(Constant("hello")),newTermName("$plus")),List(Ident(newTermName("s")))
        //"hello" + s   s is def
        condString = constantName.toString() + "." + newTermName.toString + "." + termName.toString


      case Select(Apply(Select(Select(_,scalaPredef),termName),List(typeName,tName)),methoName) =>
        condString = typeName.toString() + "." + tName.toString() + "." + scalaPredef.toTypeName + "." + termName + "." + methoName.toString
       */

      case _ =>

    }

    condString

  }
*/

  def ResetCondString = {

    condString = ""

  }


  def ResearchCallSites(name: String): Array[String] = {

    var returnString: Array[String] = Array()

    for (cc <- CallerToCallee) {

      println("cc  " + cc._1 + cc._2)

      if (cc._1.contains(name)) {

        returnString = returnString :+ cc._2

        println("Callsite " + cc._2)
      }

    }

    returnString

  }


}
