package ca.uwaterloo.scalacg.AST

import ca.uwaterloo.scalacg.config.Global

trait Extractor extends Global{

  import global._
  import global.definitions._


  object StructuralExtractors{



    //识别ValDef的右手边的Tree，
    object ExRhsOfValDef{

      def unapply(tree:Apply):Option[String] = tree match{

        case Apply(Select(Ident(tName),termName),List(Literal(Constant(num)))) =>
          //右手边是一个变量与一个常数的计算时， 例如 v1 = v2 + 2
          Some(termName.toString + "-" + termName.toString + "-" + num.toString)

        case Apply(Select(Ident(tName),termName),List(Ident(pName))) =>
          //右手边两个都是变量的时候，例如 v1 = v2 + v3
          Some(termName.toString + "-" + termName.toString + "-" + pName.toString)


        case _ => None

      }

    }



    object ExPredefPrint{

      def unapply(tree:Apply):Option[(String,List[Tree])] = tree match{
        // extract print , println and printf from AST
        case Apply(Select(Select(This(typeName),scalaPredefName),termName),rhs)
          if typeName.toString == ("scala") && scalaPredefName.toString.contains("Predef") =>
          Some(termName.toString,rhs)

        case _ => None

      }

    }


    object ExForeach{

      def unapply(tree:Apply):Option[(String,List[Tree])] = tree match{

        case Apply(TypeApply(Select(Apply(Select(Apply(Select(Select(This(scalaName),predefName),intwrapperName),firstName),toName),secondElement),foreachName),_),block)
          if predefName.toString.contains("Predef") && intwrapperName.toString.contains("int") =>
          //for example: for(i <- 0 to 20){println(i)} , the keyword is "intWrapper"
          val returnString = firstName.toString() + "." + toName + "." + secondElement.toString()
          Some(returnString,block)

        case Apply(TypeApply(Select(Select(This(tName),termName),foreachName),_),block)
          if foreachName.toString.contains("foreach") =>
          val returnString = termName.toString + ".foreach"
          Some(returnString,block)


        case Apply(TypeApply(Select(Apply(Select(Select(_,predefName),augmentStringName),List(Ident(name))),foreachName),_),block)
          if foreachName.toString.contains("foreach") =>     // tName and termName is Name of Object , termName is Name of parameter
          //for example: val hello:List[String] = ....;for(i< - hello){println(i)}  , the keyword is "foreach"
          val returnString = name.toString + ".foreach"
          Some(returnString,block)

        case _ => None

      }

    }

    object ExScalaPredef {
      /** Extracts method calls from scala.Predef. */
      def unapply(tree: Select): Option[String] = tree match {
        case Select(Select(This(scalaName), predefName), symName) //Select:A factory method for Select nodes.
          if (scalaName.toString == "scala" && predefName.toString == "scala.this.Predef") =>
          Some(symName.toString)
        case _ => None
      }
    }


    object ExEnsuredExpression {
      /** Extracts the 'ensuring' contract from an expression. */
      def unapply(tree: Apply): Option[(Tree, Symbol, Tree)] = tree match {
        case Apply(
        Select(
        Apply(
        TypeApply( //TypeApply : Explicit type application.
        ExScalaPredef("any2Ensuring"),
        TypeTree() :: Nil),
        body :: Nil),
        ensuringName),
        (Function((vd@ValDef(_, _, _, EmptyTree)) :: Nil, contractBody)) :: Nil)
          if ("ensuring".equals(ensuringName.toString)) => Some((body, vd.symbol, contractBody))
        case _ => None
      }
    }

    object ExAssertEQExpression {
      /** Extracts the 'assert' contract from an expression. */
      def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
        case Apply(ExScalaPredef("assert"), Apply(Select(lhs, methName), rhs :: Nil) :: omsg)
          if ("eq" == methName.toString) =>
          Some((lhs, rhs))
        case t =>
          None
      }
    }

    object ExAssertNEExpression {
      /** Extracts the 'assert' contract from an expression. */
      def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
        case Apply(ExScalaPredef("assert"), Apply(Select(lhs, methName), rhs :: Nil) :: omsg)
          if ("ne" == methName.toString) =>
          Some((lhs, rhs))
        case t =>
          None
      }
    }

    object ExRequiredExpression {
      /** Extracts the 'require' contract from an expression (only if it's the
        * first call in the block). */
      def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
        case Block(Apply(ExScalaPredef("require"), contractBody :: Nil) :: rest, body) =>
          if (rest.isEmpty)
            Some((body, contractBody))
          else
            Some((Block(rest, body), contractBody))

        case Apply(ExScalaPredef("require"), contractBody :: Nil) =>
          Some((Block(Literal(Constant(()))), contractBody))
        case t =>
          None
      }
    }


    //Extract Constructor from AST
    object ExConstructor {

      def unapply(tree: Template): Option[(List[Tree], List[Tree])] = tree match {

        case Template(parents, _, body) =>
          Some(parents, body)

        case _ => None

      }

    }




    object ExWhile {
      /** Extracts (cond, stmts) from a while loop */
      def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
        // do body while(cond) ==> LabelDef(L$, List(), if(cond) { body; L$() })
        case lab@LabelDef(_, List(), If(cond, Block(stats, Apply(fun: Ident, List())), _))
          if lab.symbol eq fun.symbol => Some(cond, stats)

        case lab@LabelDef(_, List(), If(cond, Apply(fun: Ident, List()), _))
          if lab.symbol eq fun.symbol => Some(cond, Nil)

        case _ => None

      }
    }


    object ExDoWhile {
      /** Extracts (cond, stmts) from a while loop */
      def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
        // do body while(cond) ==> LabelDef(L$, List(), { body; if (cond) L$() })
        case lab@LabelDef(_, List(), Block(stats, If(cond, Apply(fun: Ident, List()), _)))
          if lab.symbol eq fun.symbol => Some(cond, stats)

        case lab@LabelDef(_, List(), If(cond, Apply(fun: Ident, List()), _))
          if lab.symbol eq fun.symbol => Some(cond, Nil)

        case _ => None

      }
    }


    object  ExTypeApply{

      def unapply(tree:TypeApply):Option[(String,String)]  = tree match{

        case TypeApply(Select(Select(This(_),name),termName),_) =>
          Some(name.toString,termName.toString)

        case _ => None

      }

    }

/*
    object ExConPlus{  //FIXME
      // hello: List[String] = List("hello") ;
      // extract hello = hello :+ "world"
      def unapply(tree:Apply):Option[(String,List[Tree])] = tree match{

        case Apply(ExTypeApply(name,termName),block) =>
          var returnString = name.toString + "." + termName.toString
          Some(returnString,block)


        case _ => None

      }

    }
    */

  }


  object ExpressionExtractors {

    object ExAnd {

      def unapply(tree:Apply):Option[(Tree,Tree)] = tree match{

        case Apply(Select(lhs,termName),List(rhs)) if termName.toString == "$amp$amp" =>
          Some(lhs,rhs)

        case _ => None

      }

    }


    object ExOr {

      def unapply(tree:Apply):Option[(Tree,Tree)] = tree match{

        case Apply(Select(lhs,termName),List(rhs)) if termName.toString == "$bar$bar" =>
          Some(lhs,rhs)

        case _ => None

      }

    }


    object ExNot{

      def unapply(tree:Select):Option[(Tree)] = tree match{

        case Select(lhs,termName) if termName.toString == "unary_$bang" =>
          Some(lhs)

        case _ => None

      }

    }


    object ExGreater{
/*
      def unapply(tree:Apply):Option[(String)] = tree match{

        case Apply(Select(Select(_,lName),termName),List(Literal(num))) if termName.toString == "$greater" =>
          Some(lName.toString+ "." +termName.toString + "." + num.toString) //右手边为常数的情况


        case Apply(Select(Select(_,lName),termName),List(Select(_,rName)))   if termName.toString == "$greater" =>
          Some(lName.toString+ "." +termName.toString + "." + rName.toString) // 右手边为变量的情况

        case _ => None

      }
      */

      def unapply(tree:Apply):Option[(String)] = tree match{

        case Apply(Select(Ident(targetName),termName),List(Literal(Constant(num)))) if termName.toString == "$greater" =>
          Some(targetName.toString + "." + termName.toString + "." + num.toString)

        case Apply(Select(Ident(targetName),termName),List(Select(_,rName))) if termName.toString == "$greater" =>
          Some(targetName.toString + "." + termName.toString + "." + rName.toString)

        case _ => None

      }


    }


    object ExLess{

      def unapply(tree:Apply):Option[String] = tree match{

        case Apply(Select(Select(_,lName),termName),List(Literal(num))) if termName.toString == "$less" =>
          Some(lName.toString+ "." +termName.toString + "." + num.toString) //右手边为常数的情况


        case Apply(Select(Select(_,lName),termName),List(Select(_,rName)))   if termName.toString == "$less" =>
          Some(lName.toString+ "." +termName.toString + "." + rName.toString) // 右手边为变量的情况

        case _ => None

      }

    }


    object ExGreaterEq{

      def unapply(tree:Apply):Option[String] = tree match{

        case Apply(Select(Select(_,lName),termName),List(Literal(num))) if termName.toString == "$greater$eq" =>
          Some(lName.toString+ "." +termName.toString + "." + num.toString) //右手边为常数的情况


        case Apply(Select(Select(_,lName),termName),List(Select(_,rName)))   if termName.toString == "$greater$eq" =>
          Some(lName.toString+ "." +termName.toString + "." + rName.toString) // 右手边为变量的情况

        case _ => None

      }

    }


    object ExLessEq{

      def unapply(tree:Apply):Option[String] = tree match{

        case Apply(Select(Select(_,lName),termName),List(Literal(num))) if termName.toString == "$less$eq" =>
          Some(lName.toString+ "." +termName.toString + "." + num.toString) //右手边为常数的情况


        case Apply(Select(Select(_,lName),termName),List(Select(_,rName)))   if termName.toString == "$less$eq" =>
          Some(lName.toString+ "." +termName.toString + "." + rName.toString) // 右手边为变量的情况

        case _ => None

      }



    }


    object ExEQEQorBANGEQ{
      // extract $eq$eq, for example, println(v1 == v2) or println(v1 != v2)

      def unapply(tree:Apply):Option[String] = tree match{

        case Apply(Select(Ident(firstElement),termName),List(Ident(secondElement)))
          if termName.toString == "$eq$eq" || termName.toString == "$bang$eq" =>
          Some(firstElement.toString + "." + termName + "." + secondElement.toString)

        case _ => None


      }


    }


    //-------------------------------
    //There are code is suitable for Scala Compiler 2.10.0 M3 version. Do Not Use.

    /*
    object ExAnd {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(s @ Select(lhs, _), List(rhs)) if (s.symbol == Boolean_and) =>
          Some((lhs,rhs))
        case _ => None
      }
    }

    object ExOr {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(s @ Select(lhs, _), List(rhs)) if (s.symbol == Boolean_or) =>
          Some((lhs,rhs))
        case _ => None
      }
    }

    object ExNot {
      lazy val Boolean_not = getMember(BooleanClass, nme.UNARY_!)
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(s @ Select(t, _), List()) if (s.symbol == Boolean_not) =>
          Some(t)
        case _ => None
      }
    }

    object ExEq {
      def unapply(tree: Apply): Option[(Tree, Tree)] = tree match {
        case Apply(s @ Select(lhs, _), List(rhs)) if (s.symbol == definitions.Object_eq) =>
          Some((lhs, rhs))
        case _ => None
      }
    }

    object ExNe {
      def unapply(tree: Apply): Option[(Tree, Tree)] = tree match {
        case Apply(s @ Select(lhs, _), List(rhs)) if (s.symbol == definitions.Object_ne) =>
          Some((lhs, rhs))
        case _ => None
      }
    }

    object ExNew {
      def unapply(tree: Apply): Option[(Symbol, Type, List[Tree])] = tree match {
        case a @ Apply(s @ Select(New(what), name), args) if (s.symbol.name == nme.CONSTRUCTOR) =>
          Some((s.symbol, what.tpe, args))
        case _ => None
      }
    }


*/

  }



}
