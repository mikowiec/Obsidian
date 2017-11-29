package edu.cmu.cs.obsidian.parser

import scala.collection.{Map, Seq}
import scala.collection.immutable.TreeMap
import edu.cmu.cs.obsidian.typecheck._

sealed trait DeclarationTable {
    def name: String

    /* merely returns "this" if this is a [ContractTable] already,
     * or gets the [ContractTable] of a [StateTable] */
    def contract: ContractTable

    def ast: AST
    def astRaw: AST

    /* looks for a contract called [name] that's in scope
     * (either globally or in this particular contract) */
    def lookupContract(name: String): Option[ContractTable]
    def lookupField(name: String): Option[Field]
    def lookupTransaction(name: String): Option[Transaction]
    def lookupFunction(name: String): Option[Func]

    def lookupFieldRaw(name: String): Option[Field]
    def lookupTransactionRaw(name: String): Option[Transaction]
    def lookupFunctionRaw(name: String): Option[Func]
    def simpleType: SimpleType

    def indexDecl[T, TCast](decls: Seq[Declaration], tag: DeclarationTag): Map[String, TCast] = {
        var lookup = new TreeMap[String, TCast]()

        for (decl <- decls if decl.tag == tag) {
            lookup = lookup.updated(decl.name, decl.asInstanceOf[TCast])
        }

        lookup
    }
}

class StateTable(
        astNodeRaw: State,
        lexicallyInsideOf: ContractTable) extends DeclarationTable {

    private var astNode: State = _
    private var fieldLookup: Map[String, Field] = _
    private var txLookup: Map[String, Transaction] = _
    private var funLookup: Map[String, Func] = _

    def updateAstNode(ast: State): Unit = {
        astNode = ast
        fieldLookup = indexDecl[ObsidianType, Field](ast.declarations, FieldDeclTag)
        txLookup = indexDecl[ObsidianType, Transaction](ast.declarations, TransactionDeclTag)
        funLookup = indexDecl[ObsidianType, Func](ast.declarations, FuncDeclTag)
    }

    val fieldLookupRaw: Map[String, Field] = {
        indexDecl[ParsableType, Field](astNodeRaw.declarations, FieldDeclTag)
    }

    val txLookupRaw: Map[String, Transaction] = {
        indexDecl[ParsableType, Transaction](astNodeRaw.declarations, TransactionDeclTag)
    }

    val funLookupRaw: Map[String, Func] = {
        indexDecl[ParsableType, Func](astNodeRaw.declarations, FuncDeclTag)
    }

    def name: String = astNodeRaw.name

    def simpleType = StateType(contract.name, astNodeRaw.name)

    def astRaw : State = astNodeRaw
    def ast: State = astNode

    def contract: ContractTable = lexicallyInsideOf

    def lookupContract(name: String): Option[ContractTable] = contract.lookupContract(name)

    def lookupField(name: String): Option[Field] = {
        fieldLookup.get(name) match {
            case x@Some(_) => x
            case None => lexicallyInsideOf.lookupField(name)
        }
    }
    def lookupTransaction(name: String): Option[Transaction] = {
        txLookup.get(name) match {
            case x@Some(_) => x
            case None => lexicallyInsideOf.lookupTransaction(name)
        }
    }
    def lookupFunction(name: String): Option[Func] = {
        funLookup.get(name) match {
            case x@Some(_) => x
            case None => lexicallyInsideOf.lookupFunction(name)
        }
    }

    def lookupFieldRaw(name: String): Option[Field] = {
        fieldLookupRaw.get(name) match {
            case x@Some(_) => x
            case None => lexicallyInsideOf.lookupFieldRaw(name)
        }
    }
    def lookupTransactionRaw(name: String): Option[Transaction] = {
        txLookupRaw.get(name) match {
            case x@Some(_) => x
            case None => lexicallyInsideOf.lookupTransactionRaw(name)
        }
    }
    def lookupFunctionRaw(name: String): Option[Func] = {
        funLookupRaw.get(name) match {
            case x@Some(_) => x
            case None => lexicallyInsideOf.lookupFunctionRaw(name)
        }
    }
}

class ContractTable(
        astNodeRaw: Contract,
        symbolTable: SymbolTable,
        parentContract: Option[ContractTable]) extends DeclarationTable {

    def this(astNodeRaw: Contract, symbolTable: SymbolTable) =
        this(astNodeRaw, symbolTable, None)

    def this(astNodeRaw: Contract, symbolTable: SymbolTable, parentContract: ContractTable) =
        this(astNodeRaw, symbolTable, Some(parentContract))

    private var astNode: Contract = _
    private var fieldLookup: Map[String, Field] = _
    private var txLookup: Map[String, Transaction] = _
    private var funLookup: Map[String, Func] = _

    def simpleType = JustContractType(name)

    def updateAstNode(ast: Contract): Unit = {
        astNode = ast
        fieldLookup = indexDecl[ObsidianType, Field](ast.declarations, FieldDeclTag)
        txLookup = indexDecl[ObsidianType, Transaction](ast.declarations, TransactionDeclTag)
        funLookup = indexDecl[ObsidianType, Func](ast.declarations, FuncDeclTag)
    }

    val fieldLookupRaw: Map[String, Field] = {
        indexDecl[ParsableType, Field](astNodeRaw.declarations, FieldDeclTag)
    }

    val txLookupRaw: Map[String, Transaction] = {
        indexDecl[ParsableType, Transaction](astNodeRaw.declarations, TransactionDeclTag)
    }

    val funLookupRaw: Map[String, Func] = {
        indexDecl[ParsableType, Func](astNodeRaw.declarations, FuncDeclTag)
    }

    val stateLookup: Map[String, StateTable] = {
        indexDecl[ParsableType, State](astNodeRaw.declarations, StateDeclTag).mapValues(
            (st: State) => new StateTable(st, this)
        )
    }

    val childContractLookup: Map[String, ContractTable] = {
        indexDecl[ParsableType, Contract](astNodeRaw.declarations, ContractDeclTag).mapValues(
            (ct: Contract) => new ContractTable(ct, symbolTable, this)
        )
    }

    def name: String = astNode.name

    def contract: ContractTable = this

    /* resolves a contract from the point of view of this contract. Two cases:
     * 1) [name] refers to a global contract and it's in the symbol table
     * 2) [name] refers to a child/nested contract of this contract
     */
    def lookupContract(name: String): Option[ContractTable] = {
        if (name == this.name) Some(this) else
        (symbolTable.contract(name), childContract(name)) match {
            case (_, Some(ct)) => Some(ct)
            case (Some(ct), _) => Some(ct)
            case _ => None
        }
    }

    def ast: Contract = astNode
    def astRaw: Contract = astNodeRaw
    def lookupField(name: String): Option[Field] = fieldLookup.get(name)
    def lookupTransaction(name: String): Option[Transaction] = txLookup.get(name)
    def lookupFunction(name: String): Option[Func] = funLookup.get(name)
    def lookupFieldRaw(name: String): Option[Field] = fieldLookupRaw.get(name)
    def lookupTransactionRaw(name: String): Option[Transaction] = txLookupRaw.get(name)
    def lookupFunctionRaw(name: String): Option[Func] = funLookupRaw.get(name)

    def state(name: String): Option[StateTable] = stateLookup.get(name)
    def possibleStates: Set[String] = stateLookup.values.map(_.name).toSet

    def childContract(name: String): Option[ContractTable] =
        childContractLookup.get(name)

    def parent: Option[ContractTable] = parentContract
    def hasParent: Boolean = parent.isDefined

    def constructors: Seq[Constructor] = {
        ast.declarations.filter(_.tag == ConstructorDeclTag)
                        .map(_.asInstanceOf[Constructor])
    }
}

class SymbolTable(astNodeRaw: Program) {
    var contractLookup: Map[String, ContractTable] = {
        var table = TreeMap[String, ContractTable]()
        for (contract <- astNodeRaw.contracts) {
            table = table.updated(contract.name, new ContractTable(contract, this))
        }
        table
    }

    def updateAstNode(ast: Program): Unit = {
        astNode = ast
    }

    private var astNode: Program = _

    def ast: Program = astNode
    def astRaw: Program = astNodeRaw

    /* only retrieves top level contracts (i.e. not nested) */
    def contract: Function[String, Option[ContractTable]] = contractLookup.get
}
