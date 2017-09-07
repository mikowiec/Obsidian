package edu.cmu.cs.obsidian.parser

import scala.collection.{Map, Seq}
import scala.collection.immutable.TreeMap
import edu.cmu.cs.obsidian.typecheck._

sealed trait DeclarationTable {
    def name: String

    /* merely returns "this" if this is a [ContractTable] already,
     * or gets the [ContractTable] of a [StateTable] */
    def contract: ContractTable

    def ast: AST[ResolvedType]
    def astRaw: AST[ParsableType]

    /* looks for a contract called [name] that's in scope
     * (either globally or in this particular contract) */
    def lookupContract(name: String): Option[ContractTable]
    def lookupField(name: String): Option[Field[ResolvedType]]
    def lookupTransaction(name: String): Option[Transaction[ResolvedType]]
    def lookupFunction(name: String): Option[Func[ResolvedType]]

    def lookupFieldRaw(name: String): Option[Field[ParsableType]]
    def lookupTransactionRaw(name: String): Option[Transaction[ParsableType]]
    def lookupFunctionRaw(name: String): Option[Func[ParsableType]]
    def simpleType: SimpleType

    def indexDecl[T, TCast](decls: Seq[Declaration[T]], tag: DeclarationTag): Map[String, TCast] = {
        var lookup = new TreeMap[String, TCast]()

        for (decl <- decls if decl.tag == tag) {
            lookup = lookup.updated(decl.name, decl.asInstanceOf[TCast])
        }

        lookup
    }
}

class StateTable(
        astNodeRaw: State[ParsableType],
        lexicallyInsideOf: ContractTable) extends DeclarationTable {

    private var astNode: State[ResolvedType] = _
    private var fieldLookup: Map[String, Field[ResolvedType]] = _
    private var txLookup: Map[String, Transaction[ResolvedType]] = _
    private var funLookup: Map[String, Func[ResolvedType]] = _

    def updateAstNode(ast: State[ResolvedType]): Unit = {
        astNode = ast
        fieldLookup = indexDecl[ResolvedType, Field[ResolvedType]](ast.declarations, FieldDeclTag)
        txLookup = indexDecl[ResolvedType, Transaction[ResolvedType]](ast.declarations, TransactionDeclTag)
        funLookup = indexDecl[ResolvedType, Func[ResolvedType]](ast.declarations, FuncDeclTag)
    }

    val fieldLookupRaw: Map[String, Field[ParsableType]] = {
        indexDecl[ParsableType, Field[ParsableType]](astNodeRaw.declarations, FieldDeclTag)
    }

    val txLookupRaw: Map[String, Transaction[ParsableType]] = {
        indexDecl[ParsableType, Transaction[ParsableType]](astNodeRaw.declarations, TransactionDeclTag)
    }

    val funLookupRaw: Map[String, Func[ParsableType]] = {
        indexDecl[ParsableType, Func[ParsableType]](astNodeRaw.declarations, FuncDeclTag)
    }

    def name: String = astNodeRaw.name

    def simpleType = StateType(contract.name, astNodeRaw.name)

    def astRaw : State[ParsableType] = astNodeRaw
    def ast: State[ResolvedType] = astNode

    def contract: ContractTable = lexicallyInsideOf

    def lookupContract(name: String): Option[ContractTable] = contract.lookupContract(name)

    def lookupField(name: String): Option[Field[ResolvedType]] = {
        fieldLookup.get(name) match {
            case x@Some(_) => x
            case None => lexicallyInsideOf.lookupField(name)
        }
    }
    def lookupTransaction(name: String): Option[Transaction[ResolvedType]] = {
        txLookup.get(name) match {
            case x@Some(_) => x
            case None => lexicallyInsideOf.lookupTransaction(name)
        }
    }
    def lookupFunction(name: String): Option[Func[ResolvedType]] = {
        funLookup.get(name) match {
            case x@Some(_) => x
            case None => lexicallyInsideOf.lookupFunction(name)
        }
    }

    def lookupFieldRaw(name: String): Option[Field[ParsableType]] = {
        fieldLookupRaw.get(name) match {
            case x@Some(_) => x
            case None => lexicallyInsideOf.lookupFieldRaw(name)
        }
    }
    def lookupTransactionRaw(name: String): Option[Transaction[ParsableType]] = {
        txLookupRaw.get(name) match {
            case x@Some(_) => x
            case None => lexicallyInsideOf.lookupTransactionRaw(name)
        }
    }
    def lookupFunctionRaw(name: String): Option[Func[ParsableType]] = {
        funLookupRaw.get(name) match {
            case x@Some(_) => x
            case None => lexicallyInsideOf.lookupFunctionRaw(name)
        }
    }
}

class ContractTable(
        astNodeRaw: Contract[ParsableType],
        symbolTable: SymbolTable,
        parentContract: Option[ContractTable]) extends DeclarationTable {

    def this(astNodeRaw: Contract[ParsableType], symbolTable: SymbolTable) =
        this(astNodeRaw, symbolTable, None)

    def this(astNodeRaw: Contract[ParsableType], symbolTable: SymbolTable, parentContract: ContractTable) =
        this(astNodeRaw, symbolTable, Some(parentContract))

    private var astNode: Contract[ResolvedType] = _
    private var fieldLookup: Map[String, Field[ResolvedType]] = _
    private var txLookup: Map[String, Transaction[ResolvedType]] = _
    private var funLookup: Map[String, Func[ResolvedType]] = _

    def simpleType = JustContractType(name)

    def updateAstNode(ast: Contract[ResolvedType]): Unit = {
        astNode = ast
        fieldLookup = indexDecl[ResolvedType, Field[ResolvedType]](ast.declarations, FieldDeclTag)
        txLookup = indexDecl[ResolvedType, Transaction[ResolvedType]](ast.declarations, TransactionDeclTag)
        funLookup = indexDecl[ResolvedType, Func[ResolvedType]](ast.declarations, FuncDeclTag)
    }

    val fieldLookupRaw: Map[String, Field[ParsableType]] = {
        indexDecl[ParsableType, Field[ParsableType]](astNodeRaw.declarations, FieldDeclTag)
    }

    val txLookupRaw: Map[String, Transaction[ParsableType]] = {
        indexDecl[ParsableType, Transaction[ParsableType]](astNodeRaw.declarations, TransactionDeclTag)
    }

    val funLookupRaw: Map[String, Func[ParsableType]] = {
        indexDecl[ParsableType, Func[ParsableType]](astNodeRaw.declarations, FuncDeclTag)
    }

    val stateLookup: Map[String, StateTable] = {
        indexDecl[ParsableType, State[ParsableType]](astNodeRaw.declarations, StateDeclTag).mapValues(
            (st: State[ParsableType]) => new StateTable(st, this)
        )
    }

    val childContractLookup: Map[String, ContractTable] = {
        indexDecl[ParsableType, Contract[ParsableType]](astNodeRaw.declarations, ContractDeclTag).mapValues(
            (ct: Contract[ParsableType]) => new ContractTable(ct, symbolTable, this)
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

    def ast: Contract[ResolvedType] = astNode
    def astRaw: Contract[ParsableType] = astNodeRaw
    def lookupField(name: String): Option[Field[ResolvedType]] = fieldLookup.get(name)
    def lookupTransaction(name: String): Option[Transaction[ResolvedType]] = txLookup.get(name)
    def lookupFunction(name: String): Option[Func[ResolvedType]] = funLookup.get(name)
    def lookupFieldRaw(name: String): Option[Field[ParsableType]] = fieldLookupRaw.get(name)
    def lookupTransactionRaw(name: String): Option[Transaction[ParsableType]] = txLookupRaw.get(name)
    def lookupFunctionRaw(name: String): Option[Func[ParsableType]] = funLookupRaw.get(name)

    def state(name: String): Option[StateTable] = stateLookup.get(name)
    def possibleStates: Set[String] = stateLookup.values.map(_.name).toSet

    def childContract(name: String): Option[ContractTable] =
        childContractLookup.get(name)

    def parent: Option[ContractTable] = parentContract
    def hasParent: Boolean = parent.isDefined

    def constructors: Seq[Constructor[ParsableType]] = {
        ast.declarations.filter(_.tag == ConstructorDeclTag)
                        .map(_.asInstanceOf[Constructor[ParsableType]])
    }
}

class SymbolTable(astNodeRaw: Program[ParsableType]) {
    var contractLookup: Map[String, ContractTable] = {
        var table = TreeMap[String, ContractTable]()
        for (contract <- astNodeRaw.contracts) {
            table = table.updated(contract.name, new ContractTable(contract, this))
        }
        table
    }

    def updateAstNode(ast: Program[ResolvedType]): Unit = {
        astNode = ast
    }

    private var astNode: Program[ResolvedType] = _

    def ast: Program[ResolvedType] = astNode
    def astRaw: Program[ParsableType] = astNodeRaw

    /* only retrieves top level contracts (i.e. not nested) */
    def contract: Function[String, Option[ContractTable]] = contractLookup.get
}
