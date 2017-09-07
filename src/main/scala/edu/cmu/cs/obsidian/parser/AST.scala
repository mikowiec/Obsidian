package edu.cmu.cs.obsidian.parser

import scala.util.parsing.input.{NoPosition, Position}
import edu.cmu.cs.obsidian.lexer.Token
import edu.cmu.cs.obsidian.parser.Parser.Identifier

trait HasLocation {
    var loc: Position = NoPosition
    def setLoc(t: Token): this.type = { loc = t.pos; this }
    def setLoc(other: HasLocation): this.type = { loc = other.loc; this }
    def setLoc(id: (String, Position)): this.type = { loc = id._2; this }
}

sealed abstract class AST[T]() extends HasLocation

sealed abstract class Statement[T]() extends AST[T]

/* All expressions are statements. We relegate the pruning of expressions
 * that don't have effects to a later analysis */
sealed abstract class Expression[T]() extends Statement[T]

/* this is to circumnavigate type erasure: it makes it possible to match on the exact
 * type of a Declarations at runtime */
sealed trait DeclarationTag
object TypeDeclTag extends DeclarationTag
object FieldDeclTag extends DeclarationTag
object ContractDeclTag extends DeclarationTag
object StateDeclTag extends DeclarationTag
object FuncDeclTag extends DeclarationTag
object ConstructorDeclTag extends DeclarationTag
object TransactionDeclTag extends DeclarationTag

sealed abstract class Declaration[T]() extends AST[T] {
    val name: String
    val tag: DeclarationTag
}
sealed abstract class InvokableDeclaration[T]() extends Declaration[T] {
    val args: Seq[VariableDecl[T]]
    val retType: Option[T]
    val body: Seq[Statement[T]]
    val ensuresState: Option[Set[String]]
}

/* Expressions */
case class Variable[T](name: String) extends Expression[T]
case class NumLiteral[T](value: Int) extends Expression[T]
case class StringLiteral[T](value: String) extends Expression[T]
case class TrueLiteral[T]() extends Expression[T]
case class FalseLiteral[T]() extends Expression[T]
case class This[T]() extends Expression[T]
case class Parent[T]() extends Expression[T]
case class Conjunction[T](e1: Expression[T], e2: Expression[T]) extends Expression[T]
case class Disjunction[T](e1: Expression[T], e2: Expression[T]) extends Expression[T]
case class LogicalNegation[T](e: Expression[T]) extends Expression[T]
case class Add[T](e1: Expression[T], e2: Expression[T]) extends Expression[T]
case class Subtract[T](e1: Expression[T], e2: Expression[T]) extends Expression[T]
case class Divide[T](e1: Expression[T], e2: Expression[T]) extends Expression[T]
case class Multiply[T](e1: Expression[T], e2: Expression[T]) extends Expression[T]
case class Equals[T](e1: Expression[T], e2: Expression[T]) extends Expression[T]
case class GreaterThan[T](e1: Expression[T], e2: Expression[T]) extends Expression[T]
case class GreaterThanOrEquals[T](e1: Expression[T], e2: Expression[T]) extends Expression[T]
case class LessThan[T](e1: Expression[T], e2: Expression[T]) extends Expression[T]
case class LessThanOrEquals[T](e1: Expression[T], e2: Expression[T]) extends Expression[T]
case class NotEquals[T](e1: Expression[T], e2: Expression[T]) extends Expression[T]
case class Dereference[T](e: Expression[T], f: String) extends Expression[T]
case class LocalInvocation[T](name: String, args: Seq[Expression[T]]) extends Expression[T]
case class Invocation[T](recipient: Expression[T], name: String, args: Seq[Expression[T]]) extends Expression[T]
case class Construction[T](name: String, args: Seq[Expression[T]]) extends Expression[T]

/* statements and control flow constructs */
case class VariableDecl[T](typ: T, varName: String) extends Statement[T]
case class VariableDeclWithInit[T](typ: T, varName: String, e: Expression[T]) extends Statement[T]
case class Return[T]() extends Statement[T]
case class ReturnExpr[T](e: Expression[T]) extends Statement[T]
case class Transition[T](newStateName: String, updates: Seq[(Variable[T], Expression[T])]) extends Statement[T]
case class Assignment[T](assignTo: Expression[T], e: Expression[T]) extends Statement[T]
case class Throw[T]() extends Statement[T]
case class If[T](eCond: Expression[T], s: Seq[Statement[T]]) extends Statement[T]
case class IfThenElse[T](eCond: Expression[T], s1: Seq[Statement[T]], s2: Seq[Statement[T]]) extends Statement[T]
case class TryCatch[T](s1: Seq[Statement[T]], s2: Seq[Statement[T]]) extends Statement[T]
case class Switch[T](e: Expression[T], cases: Seq[SwitchCase[T]]) extends Statement[T]
case class SwitchCase[T](stateName: String, body: Seq[Statement[T]]) extends AST[T]

/* Declarations */
case class TypeDecl[T](name: String, typ: T) extends Declaration[T] {
    val tag: DeclarationTag = TypeDeclTag
}

case class Field[T](isConst: Boolean, typ: T, name: String) extends Declaration[T] {
    val tag: DeclarationTag = FieldDeclTag
}

case class Constructor[T](name: String,
                       args: Seq[VariableDecl[T]],
                       ensuresState: Option[Set[String]],
                       body: Seq[Statement[T]]) extends InvokableDeclaration[T] {
    val retType: Option[T] = None
    val tag: DeclarationTag = ConstructorDeclTag
}
case class Func[T](name: String,
                args: Seq[VariableDecl[T]],
                retType: Option[T],
                body: Seq[Statement[T]]) extends InvokableDeclaration[T] {
    val ensuresState: Option[Set[String]] = None
    val tag: DeclarationTag = FuncDeclTag
}
case class Transaction[T](name: String,
                       args: Seq[VariableDecl[T]],
                       retType: Option[T],
                       availableIn: Seq[Identifier],
                       ensures: Seq[Ensures[T]],
                       ensuresState: Option[Set[String]],
                       body: Seq[Statement[T]]) extends InvokableDeclaration[T] {
    val tag: DeclarationTag = TransactionDeclTag
}
case class State[T](name: String, declarations: Seq[Declaration[T]]) extends Declaration[T] {
    val tag: DeclarationTag = StateDeclTag
}

case class Ensures[T](expr: Expression[T]) extends AST[T]

sealed abstract trait ContractModifier extends HasLocation
case class IsOwned() extends ContractModifier
case class IsShared() extends ContractModifier
case class IsMain() extends ContractModifier

case class Import[T](name: String) extends AST[T]

case class Contract[T](mod: Option[ContractModifier],
                    name: String,
                    declarations: Seq[Declaration[T]]) extends Declaration[T] {
    val tag: DeclarationTag = ContractDeclTag
}

/* Program */
case class Program[T](imports: Seq[Import[T]], contracts: Seq[Contract[T]]) extends AST[T]
