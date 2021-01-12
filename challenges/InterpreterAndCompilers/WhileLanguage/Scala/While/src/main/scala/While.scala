import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.io.StdIn

sealed abstract class BinOp

case class AddOp() extends BinOp

case class SubOp() extends BinOp

case class MulOp() extends BinOp

case class DivOp() extends BinOp

sealed abstract class RelOp

case class LessThanOp() extends RelOp

case class GreaterThanOp() extends RelOp

sealed abstract class BoolOp

case class AndOp() extends BoolOp

case class OrOp() extends BoolOp

sealed abstract class Token

case class Var(s: String) extends Token

case class Num(n: BigInt) extends Token

case class OpenBracket() extends Token

case class CloseBracket() extends Token

case class True() extends Token

case class False() extends Token

case class And() extends Token

case class Or() extends Token

case class GreaterThan() extends Token

case class LessThan() extends Token

case class Add() extends Token

case class Sub() extends Token

case class Mul() extends Token

case class Div() extends Token

sealed abstract class BinExp

case class Const(n: BigInt) extends BinExp

case class Variable(v: String) extends BinExp

case class Exp(exp1: BinExp, op: BinOp, exp2: BinExp) extends BinExp

sealed abstract class BoolExp

case class TrueExp() extends BoolExp

case class FalseExp() extends BoolExp

case class RelExp(exp1: BinExp, op: RelOp, exp2: BinExp) extends BoolExp

case class CompoundExp(exp1: BoolExp, op: BoolOp, exp2: BoolExp) extends BoolExp

sealed abstract class Construct

case class Assignment(v: String, exp: BinExp) extends Construct

case class While(cond: BoolExp, constructs: Seq[Construct]) extends Construct

case class If(cond: BoolExp, ifSeq: Seq[Construct], elseSeq: Seq[Construct]) extends Construct

object While extends App {

  def tokenize(xs: Seq[Char]) = {

    def parseChars(s: Seq[Char]) = {

      def handleAlphabets(s1: Seq[Char]) = {
        s1.takeWhile(Character.isLetter).filterNot(_.isWhitespace) match {
          case Seq('t', 'r', 'u', 'e') => (True(), 4)
          case Seq('f', 'a', 'l', 's', 'e') => (False(), 5)
          case Seq('a', 'n', 'd') => (And(), 3)
          case Seq('o', 'r') => (Or(), 2)
          case v => (Var(v.filterNot(_.isWhitespace).mkString), v.length)
        }
      }

      val s1 = s.dropWhile(_ == ' ')

      if (s1.isEmpty) {
        None
      } else {
        val (token, dropCount) = s1.head match {
          case '(' => (OpenBracket(), 1)
          case ')' => (CloseBracket(), 1)

          case '+' => (Add(), 1)
          case '-' => (Sub(), 1)
          case '*' => (Mul(), 1)
          case '/' => (Div(), 1)

          case '<' => (LessThan(), 1)
          case '>' => (GreaterThan(), 1)

          case _ =>
            if (Character.isDigit(s1.head)) {
              val pre = s.takeWhile(Character.isDigit)
              val num = BigInt(pre.mkString)
              (Num(num), pre.length)
            }
            else if (Character.isLetter(s1.head)) {
              handleAlphabets(s1)
            }
            else {
              throw new Exception("Unexpected character!")
            }
        }
        Some(token, s1.drop(dropCount).dropWhile(c => c == ' '))
      }
    }

    Seq.unfold(xs.dropWhile(_ == ' '))(parseChars)

  }

  def getAssignment(value: Seq[Char]): (Seq[Char], Seq[Char]) = {
    (Seq.empty, Seq.empty)
  }

  @tailrec
  def getEnclosedTokens(ts: Seq[Token], cnt: Int, acc: Seq[Token]): (Seq[Token], Seq[Token]) = {
    if (ts.isEmpty)
      (acc, Seq.empty)
    else
      ts.head match {
        case CloseBracket() if cnt == 0 => (acc, ts.tail)
        case CloseBracket() => getEnclosedTokens(ts, cnt - 1, acc)
        case OpenBracket() => getEnclosedTokens(ts, cnt + 1, acc)
        case _ => getEnclosedTokens(ts.tail, cnt, acc :+ ts.head)
      }
  }

  def createBinExp(ts: Seq[Token]): BinExp = {
    def getOp(token: Token) = {
      token match {
        case Add() => AddOp()
        case Sub() => SubOp()
        case Mul() => MulOp()
        case Div() => DivOp()
      }
    }

    ts match {
      case Seq(Num(n)) => Const(n)
      case Seq(Var(v)) => Variable(v)
      case Seq(OpenBracket(), _*) =>
        val (enc, rest) = getEnclosedTokens(ts.tail, 0, Seq.empty)
        val binExp = createBinExp(enc)
        if (rest.isEmpty)
          binExp
        else
          rest.head match {
            case Add() | Sub() | Mul() | Div() => Exp(binExp, getOp(rest.head), createBinExp(rest.tail))
            case _ => throw new Exception("Error while creating binary expression")
          }
      case Seq(Num(l), Mul(), Num(r), _*) => createBinExp(ts.drop(3) :+ Num(l * r))
      case Seq(Num(l), Div(), Num(r), _*) => createBinExp(ts.drop(3) :+ Num(l / r))

      case Seq(Num(l), op@(Add() | Sub() | Mul() | Div()), OpenBracket(), _*) =>
        val (enc, rest) = getEnclosedTokens(ts.drop(3), 0, Seq.empty)
        val binExp = Exp(Const(l), getOp(op), createBinExp(enc))
        rest match {
          case _ if rest.isEmpty => binExp
          case Seq(op1@(Add() | Sub() | Mul() | Div()), _*) => Exp(binExp, getOp(op1), createBinExp(rest.tail))
        }

      case Seq(Var(v), op@(Add() | Sub() | Mul() | Div()), OpenBracket(), _*) =>
        val (enc, rest) = getEnclosedTokens(ts.drop(3), 0, Seq.empty)
        val binExp = Exp(Variable(v), getOp(op), createBinExp(enc))
        rest match {
          case _ if rest.isEmpty => binExp
          case Seq(op1@(Mul() | Div()), _*) => Exp(Variable(v), getOp(op), Exp(createBinExp(enc), getOp(op1), createBinExp(rest.tail)))
          case Seq(op1@(Add() | Sub() | Mul() | Div()), _*) => Exp(Exp(Variable(v), getOp(op), createBinExp(enc)), getOp(op1), createBinExp(rest.tail))
        }

      case Seq(Num(l), op@(Add() | Sub() | Mul() | Div()), Num(r), _*) =>
        Exp(Const(l), getOp(op), createBinExp(Num(r) +: ts.drop(2)))

      case Seq(Var(lv), op@(Add() | Sub() | Mul() | Div()), Var(rv)) =>
        Exp(Variable(lv), getOp(op), Variable(rv))

      case Seq(Var(lv), op@(Mul() | Div()), Var(rv), op2@(Add() | Sub() | Mul() | Div()), _*) =>
        Exp(Exp(Variable(lv), getOp(op), Variable(rv)), getOp(op2), createBinExp(ts.drop(4)))

      case Seq(Var(lv), op@(Mul() | Div()), Num(r), op2@(Add() | Sub() | Mul() | Div()), _*) =>
        Exp(Exp(Variable(lv), getOp(op), Const(r)), getOp(op2), createBinExp(ts.drop(4)))

      case Seq(Var(lv), op@(Add() | Sub() | Mul() | Div()), Num(r), _*) =>
        Exp(Variable(lv), getOp(op), createBinExp(Num(r) +: ts.drop(3)))

      case Seq(Num(l), op@(Add() | Sub() | Mul() | Div()), Var(rv), _*) =>
        Exp(Const(l), getOp(op), createBinExp(Var(rv) +: ts.drop(3)))

      case Seq(Var(v), op@(Add() | Sub() | Mul() | Div()), _*) =>
        Exp(Variable(v), getOp(op), createBinExp(ts.drop(2)))

    }
  }

  def evalBinExp(map: Map[String, BigInt], exp: BinExp): BigInt = {
    exp match {
      case Const(l) => l
      case Variable(v) => map(v.filterNot(_.isWhitespace))
      case Exp(Const(l), AddOp(), Const(r)) => l + r
      case Exp(Const(l), SubOp(), Const(r)) => l - r
      case Exp(Const(l), MulOp(), Const(r)) => l * r
      case Exp(Const(l), DivOp(), Const(r)) => l / r
      case Exp(Variable(v), op, Const(r)) => evalBinExp(map, Exp(Const(map(v)), op, Const(r)))
      case Exp(Variable(lv), op, Variable(rv)) => evalBinExp(map, Exp(Const(map(lv)), op, Const(map(rv))))
      case Exp(Const(c), op, exp@Exp(_, _, _)) =>
        val r = evalBinExp(map, exp)
        evalBinExp(map, Exp(Const(c), op, Const(r)))
      case Exp(Variable(v), op, exp@Exp(_, _, _)) =>
        val l = map(v)
        val r = evalBinExp(map, exp)
        evalBinExp(map, Exp(Const(l), op, Const(r)))
      case Exp(exp@Exp(_, _, _), op, Const(c)) =>
        val r = evalBinExp(map, exp)
        evalBinExp(map, Exp(Const(c), op, Const(r)))
      case Exp(exp@Exp(_, _, _), op, Variable(v)) =>
        val l = map(v)
        val r = evalBinExp(map, exp)
        evalBinExp(map, Exp(Const(l), op, Const(r)))
      case Exp(exp1@Exp(_, _, _), op, exp2@Exp(_, _, _)) =>
        val l = evalBinExp(map, exp1)
        val r = evalBinExp(map, exp2)
        evalBinExp(map, Exp(Const(l), op, Const(r)))
    }
  }

  def createRelExp(ts: Seq[Token]): BoolExp = {
    val xs = ts.takeWhile(t => t != LessThan() && t != GreaterThan())
    val ys = ts.drop(xs.length).dropWhile(_ == ' ')

    if (ys.isEmpty)
      TrueExp()
    else
      ys.head match {
        case LessThan() => RelExp(createBinExp(xs), LessThanOp(), createBinExp(ys.tail.dropWhile(_ == ' ')))
        case GreaterThan() => RelExp(createBinExp(xs), GreaterThanOp(), createBinExp(ys.tail.dropWhile(_ == ' ')))
      }
  }

  def createBoolExp(ts: Seq[Token]): BoolExp = {
    val xs = ts.takeWhile(t => t != And() && t != Or())
    val ys = ts.drop(xs.length)

    if (ys.isEmpty)
      createRelExp(xs)
    else {
      val exp1 = createRelExp(xs)
      val exp2 = createBoolExp(ys.tail)
      val op = ys.head match {
        case And() => AndOp()
        case Or() => OrOp()
      }
      CompoundExp(exp1, op, exp2)
    }
  }

  def evalBoolExp(map: Map[String, BigInt], exp: BoolExp): Boolean = {
    exp match {
      case TrueExp() => true
      case FalseExp() => false
      case RelExp(exp1, LessThanOp(), exp2) => evalBinExp(map, exp1) < evalBinExp(map, exp2)
      case RelExp(exp1, GreaterThanOp(), exp2) => evalBinExp(map, exp1) > evalBinExp(map, exp2)
      case CompoundExp(exp1, AndOp(), exp2) => evalBoolExp(map, exp1) && evalBoolExp(map, exp2)
      case CompoundExp(exp1, OrOp(), exp2) => evalBoolExp(map, exp1) || evalBoolExp(map, exp2)
    }
  }

  def parseCode(code: Seq[Char], cs: Seq[Construct]): Seq[Construct] = {

    @tailrec
    def readBlock(cnt: Int, s: Seq[Char], acc: Seq[Char]): (Seq[Char], Seq[Char]) = {
      if (s.isEmpty && acc.isEmpty) {
        (acc, Seq.empty)
      }
      else {
        (cnt, s.head) match {
          case (1, '}') => (acc, s.tail)
          case (_, '}') => readBlock(cnt - 1, s.tail, acc :+ s.head)
          case (_, '{') => readBlock(cnt + 1, s.tail, acc :+ s.head)
          case (_, _) => readBlock(cnt, s.tail, acc :+ s.head)
        }
      }
    }

    //    def getStatements (chs: Seq[Char]) = {
    //
    //      def readStatement(chs:Seq[Char]) : Option[(Seq[Construct], Seq[Char])] = {
    //        val (pre, suf) = chs.span(c => c != ';')
    //        if (trimLeadingSpaces(pre).isEmpty) {
    //          None
    //        } else {
    //          Some(parseCode(pre, Seq.empty), suf)
    //        }
    //      }
    //
    //      Seq.concat(Seq.unfold(chs)(readStatement))
    //
    //    }

    def readStatementsBlock(xs: Seq[Char]) = {
      val (block, rest) = readBlock(1, xs, Seq.empty)
      (parseCode(block, Seq.empty), rest)
    }

    @tailrec
    def getAssignment(code: Seq[Char], variable: Seq[Char]): (Seq[Char], Seq[Char]) = {
      code match {
        case Seq(':', '=', _*) => (variable.dropWhile(_ == ' '), code.drop(2).filterNot(_.isWhitespace))
        case _ => getAssignment(code.tail.filterNot(_.isWhitespace), if (code.head.isWhitespace) variable else variable :+ code.head)
      }
    }

    def parseWhileConstruct(xs: Seq[Char]) = {
      val (cond, exp) = xs.span(c => c != ')') //skip the end bracket
      val exp1 = exp.tail.dropWhile(c => c == ' ')
      exp1 match {
        case Seq('d', 'o', _*) =>
          val exp2 = exp1.drop(2).dropWhile(c => c == ' ' || c == '{')
          val (ss, exp3) = readStatementsBlock(exp2)
          val boolExp = createBoolExp(tokenize(cond))
          parseCode(exp3, cs :+ While(boolExp, ss))

        case _ => throw new Exception("Parse error!")
      }

    }

    def parseIfConstruct(xs: Seq[Char]) = {
      val (cond, exp) = xs.dropWhile(c => c == '(' || c == ' ').span(c => c != ')')
      val exp1 = exp.dropWhile(c => c == ')' || c == ' ')

      exp1 match {
        case Seq('t', 'h', 'e', 'n', _*) =>
          val (ifSeq, exp3) = readStatementsBlock(exp1.drop(4).dropWhile(c => c == '{' || c == ' '))
          val exp4 = exp3.dropWhile(c => c == ')' || c == ' ')
          exp4 match {
            case Seq('e', 'l', 's', 'e', _*) =>
              val (elseSeq, exp6) = readStatementsBlock(exp4.drop(4).dropWhile(c => c == '{' || c == ' '))
              parseCode(exp6, cs :+ If(createBoolExp(tokenize(cond)), ifSeq, elseSeq))
          }
        case _ => throw new Exception("Error in parsing if statement")
      }
    }


    def parseAssignmentConstruct(xs: Seq[Char]) = {
      val (v, expr) = getAssignment(xs, Seq.empty)
      val ex = createBinExp(tokenize(expr))
      Assignment(v.mkString, ex)
    }

    val chs = code.dropWhile(c => c == ' ' || c == ';' || c == '}')
    chs match {
      case _ if chs.isEmpty => cs
      case Seq('w', 'h', 'i', 'l', 'e', _*) => parseWhileConstruct(chs.drop(5).dropWhile(c => c == ' ' || c == '('))
      case Seq('i', 'f', _*) => parseIfConstruct(chs.drop(2).dropWhile(c => c == ' ' || c == '('))
      case xs =>
        val (pre, suf) = xs.span(c => c != ';')
        val ac = parseAssignmentConstruct(pre)
        parseCode(suf, cs :+ ac)
    }

  }

  def evaluateConstructs(map: Map[String, BigInt], cs: Seq[Construct]): Map[String, BigInt] = {
    if (cs.isEmpty)
      map
    else
      cs.head match {
        case Assignment(v, exp) =>
          evaluateConstructs(map  + (v -> evalBinExp(map, exp)), cs.tail)
        case While(cond, wcs) =>
          if (evalBoolExp(map, cond))
            evaluateConstructs(evaluateConstructs(map, wcs), cs)
          else
            evaluateConstructs(map, cs.tail)
        case If(cond, ifSeq, elseSeq) =>
          evaluateConstructs(
            if (evalBoolExp(map, cond))
              evaluateConstructs(map, ifSeq)
            else
              evaluateConstructs(map, elseSeq), cs.tail)
      }
  }

  def readAllLines() = {

    @tailrec
    def reread(xs: Seq[Char]): Seq[Char] = {

      val s = StdIn.readLine()

      s match {
        case _ if s.startsWith(":exit") => xs
        case null => xs
        case _ => reread(xs ++ s.toSeq)
      }
    }

    reread(Seq.empty)

  }

  val all = readAllLines()

  val cs = parseCode(all, Seq.empty)
  val map = evaluateConstructs(Map.empty, cs)
  ListMap(map.toSeq.sortBy(_._1):_*).foreach(kv => {
    System.out.print(kv._1)
    System.out.print(' ')
    System.out.println(kv._2)
  })

}
