package edu.cmu.cs.obsidian.typecheck

import edu.cmu.cs.obsidian.parser._

import scala.collection.immutable.{TreeMap, TreeSet}

/* The only purpose of this compilation phase at the moment is to disambiguate
 * path-types. For example, If [T] is defined as a dependent type of [C], then
 * inside of [C], the types [T] and [this.T] refer to the same thing. This
 * must be clarified. */

/* Important Note: be sure to take into account the fact that AST nodes need a location.
 * To construct a new AST node in this file, explicitly set the location using [setLoc] */

object AstTransformer {

    type FoldFunc[TFrom, TTo] = Int
    type Context = Map[String, ParsableType]
    val emptyContext = new TreeMap[String, ParsableType]()

    def transformProgram(table: SymbolTable): Unit = {
        for (c <- table.astRaw.contracts.map(_.name)) {
            transformContract(table, table.contract(c).get)
        }
        val newImports = table.astRaw.imports.map(_.copy[ResolvedType]())

        val newContracts = table.astRaw.contracts.map(c => table.contract(c.name).get.ast)
        val newProgram = table.astRaw.copy[ResolvedType](imports = newImports, contracts = newContracts)
        table.updateAstNode(newProgram)
    }

    def transformContract(table: SymbolTable, cTable: ContractTable): Unit = {
        var newDecls: Seq[Declaration[ResolvedType]] = Nil
        for (d <- cTable.astRaw.declarations) {
            val newDecl: Declaration[ResolvedType] = d.tag match {
                case TransactionDeclTag =>
                    transformTransaction(table, cTable, d.asInstanceOf[Transaction[ParsableType]])
                case FuncDeclTag =>
                    transformFunc(table, cTable, d.asInstanceOf[Func[ParsableType]])
                case ConstructorDeclTag =>
                    transformConstructor(table, cTable, d.asInstanceOf[Constructor[ParsableType]])
                case FieldDeclTag =>
                    transformField(table, cTable, d.asInstanceOf[Field[ParsableType]])
                case StateDeclTag =>
                    val stateTable = cTable.state(d.asInstanceOf[State[ParsableType]].name).get
                    transformState(table, stateTable)
                    stateTable.ast
                case ContractDeclTag =>
                    val contractTable = cTable.childContract(d.asInstanceOf[Contract[ParsableType]].name).get
                    transformContract(table, contractTable)
                    contractTable.ast
                case TypeDeclTag => null
            }
            newDecls = newDecl +: newDecls
        }

        newDecls = newDecls.reverse

        val newContract = cTable.astRaw.copy[ResolvedType](declarations = newDecls)
        cTable.updateAstNode(newContract)
    }

    def transformState(table: SymbolTable, sTable: StateTable): Unit = {
        var newDecls: Seq[Declaration[ResolvedType]] = Nil
        for (d <- sTable.astRaw.declarations) {
            val newDecl: Declaration[ResolvedType] = d.tag match {
                case TransactionDeclTag =>
                    transformTransaction(table, sTable, d.asInstanceOf[Transaction[ParsableType]])
                case FuncDeclTag =>
                    transformFunc(table, sTable, d.asInstanceOf[Func[ParsableType]])
                case ConstructorDeclTag =>
                    transformConstructor(table, sTable, d.asInstanceOf[Constructor[ParsableType]])
                case FieldDeclTag =>
                    transformField(table, sTable, d.asInstanceOf[Field[ParsableType]])
                case StateDeclTag => null
                case ContractDeclTag => null
                case TypeDeclTag => null
            }
            newDecls = newDecl +: newDecls
        }

        newDecls = newDecls.reverse

        val newContract = sTable.astRaw.copy[ResolvedType](declarations = newDecls)
        sTable.updateAstNode(newContract)
    }

    def transformField(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            f: Field[ParsableType]): Field[ResolvedType] = {
        f.copy[ResolvedType](typ = transformType(table, lexicallyInsideOf, emptyContext, f.typ))
    }

    def transformExpression(e: Expression[ParsableType]): Expression[ResolvedType] = {
        e match {
            case v: Variable[ParsableType] => v.copy[ResolvedType]()
            case n: NumLiteral[ParsableType] => n.copy[ResolvedType]()
            case s: StringLiteral[ParsableType] => s.copy[ResolvedType]()
            case _: TrueLiteral[ParsableType] => new TrueLiteral[ResolvedType]()
            case _: FalseLiteral[ParsableType] => new FalseLiteral[ResolvedType]()
            case _: This[ParsableType] => new This[ResolvedType]()
            case _: Parent[ParsableType] => new Parent[ResolvedType]()
            case c: Conjunction[ParsableType] =>
                new Conjunction[ResolvedType](transformExpression(c.e1), transformExpression(c.e2))
            case d: Disjunction[ParsableType] =>
                new Disjunction[ResolvedType](transformExpression(d.e1), transformExpression(d.e2))
            case n: LogicalNegation[ParsableType] =>
                new LogicalNegation[ResolvedType](transformExpression(n.e))
            case a: Add[ParsableType] =>
                new Add[ResolvedType](transformExpression(a.e1), transformExpression(a.e2))
            case s: Subtract[ParsableType] =>
                new Subtract[ResolvedType](transformExpression(s.e1), transformExpression(s.e2))
            case d: Divide[ParsableType] =>
                new Divide[ResolvedType](transformExpression(d.e1), transformExpression(d.e2))
            case m: Multiply[ParsableType] =>
                new Multiply[ResolvedType](transformExpression(m.e1), transformExpression(m.e2))
            case eq: Equals[ParsableType] =>
                new Equals[ResolvedType](transformExpression(eq.e1), transformExpression(eq.e2))
            case g: GreaterThan[ParsableType] =>
                new GreaterThan[ResolvedType](transformExpression(g.e1), transformExpression(g.e2))
            case g: GreaterThanOrEquals[ParsableType] =>
                new GreaterThanOrEquals[ResolvedType](transformExpression(g.e1), transformExpression(g.e2))
            case l: LessThan[ParsableType] =>
                new LessThan[ResolvedType](transformExpression(l.e1), transformExpression(l.e2))
            case l: LessThanOrEquals[ParsableType] =>
                new LessThanOrEquals[ResolvedType](transformExpression(l.e1), transformExpression(l.e2))
            case ne: NotEquals[ParsableType] =>
                new NotEquals[ResolvedType](transformExpression(ne.e1), transformExpression(ne.e2))
            case d: Dereference[ParsableType] => d.copy(e = transformExpression(d.e))
            case i: LocalInvocation[ParsableType] =>
                i.copy(args = i.args.map(eArg => transformExpression(eArg)))
            case i: Invocation[ParsableType] =>
                i.copy(recipient = transformExpression(i.recipient), args = i.args.map(eArg => transformExpression(eArg)))
            case c: Construction[ParsableType] =>
                c.copy(args = c.args.map(eArg => transformExpression(eArg)))
        }
    }

    def startContext(args: Seq[VariableDecl[ParsableType]]): Context = {
        var startContext = emptyContext
        for (a <- args) {
            startContext = startContext.updated(a.varName, a.typ)
        }
        startContext
    }

    def transformArgs(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            args: Seq[VariableDecl[ParsableType]]): Seq[VariableDecl[ResolvedType]] = {
        var newArgs: Seq[VariableDecl[ResolvedType]] = Nil
        val context = startContext(args)
        for (a <- args) {
            val aNew = a.copy(typ = transformType(table, lexicallyInsideOf, context - a.varName, a.typ))
            newArgs = aNew +: newArgs
        }
        newArgs.reverse
    }

    def transformTransaction(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            t: Transaction[ParsableType]): Transaction[ResolvedType] = {
        val context = startContext(t.args)

        val newRetType = t.retType.map(transformType(table, lexicallyInsideOf, context, _))
        val newArgs = transformArgs(table, lexicallyInsideOf, t.args)

        val newEnsures = t.ensures.map(en => en.copy[ResolvedType](expr = transformExpression(en.expr)))
        t.copy[ResolvedType](retType = newRetType, args = newArgs, ensures = newEnsures,
                             body = transformBody(table, lexicallyInsideOf, context, t.body))
    }

    def transformConstructor(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            c: Constructor[ParsableType]): Constructor[ResolvedType] = {

        val newArgs = transformArgs(table, lexicallyInsideOf, c.args)
        val context = startContext(c.args)

        c.copy(args = newArgs, body = transformBody(table, lexicallyInsideOf, context, c.body))
    }

    def transformFunc(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            f: Func[ParsableType]): Func[ResolvedType] = {

        val newArgs = transformArgs(table, lexicallyInsideOf, f.args)
        val context = startContext(f.args)
        val newRetType = f.retType.map(transformType(table, lexicallyInsideOf, context, _))

        f.copy(retType = newRetType, args = newArgs,
               body = transformBody(table, lexicallyInsideOf, context, f.body))

    }

    def transformBody(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            inScope: Context,
            b: Seq[Statement[ParsableType]]): Seq[Statement[ResolvedType]] = {
        b match {
            case Seq() => Seq()
            case s +: rest =>
                val (sNew, inScopeNew) = transformStatement(table, lexicallyInsideOf, inScope, s)
                sNew +: transformBody(table, lexicallyInsideOf, inScopeNew, rest)
        }
    }

    def transformStatement(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            context: Context,
            s: Statement[ParsableType]): (Statement[ResolvedType], Context) = {
        s match {
            case oldDecl@VariableDecl(typ, varName) =>
                val newTyp = transformType(table, lexicallyInsideOf, context, typ)
                (oldDecl.copy(typ = newTyp).setLoc(oldDecl), context.updated(varName, typ))
            case oldDecl@VariableDeclWithInit(typ, varName, e) =>
                val newTyp = transformType(table, lexicallyInsideOf, context, typ)
                val newDecl = oldDecl.copy(typ = newTyp, e = transformExpression(e)).setLoc(oldDecl)
                (newDecl, context.updated(varName, typ))
            case oldIf@If(eCond, sIf) =>
                val sIfNew = transformBody(table, lexicallyInsideOf, context, sIf)
                val newIf = oldIf.copy(s = sIfNew, eCond = transformExpression(eCond)).setLoc(oldIf)
                (newIf, context)
            case oldIf@IfThenElse(eCond, s1, s2) =>
                val s1New = transformBody(table, lexicallyInsideOf, context, s1)
                val s2New = transformBody(table, lexicallyInsideOf, context, s2)
                val newIf = oldIf.copy(
                    s1 = s1New,
                    s2 = s2New,
                    eCond = transformExpression(eCond)
                ).setLoc(oldIf)
                (newIf, context)
            case oldTry@TryCatch(s1, s2) =>
                val s1New = transformBody(table, lexicallyInsideOf, context, s1)
                val s2New = transformBody(table, lexicallyInsideOf, context, s2)
                val newIf = oldTry.copy(s1 = s1New, s2 = s2New).setLoc(oldTry)
                (newIf, context)
            case oldSwitch@Switch(e, cases) =>
                val newCases = cases.map(_case => {
                    val newBody = transformBody(table, lexicallyInsideOf, context, _case.body)
                    _case.copy(body = newBody).setLoc(oldSwitch)
                })
                val newSwitch = oldSwitch.copy(e = transformExpression(e),
                                               cases = newCases).setLoc(oldSwitch)
                (newSwitch, context)
            case e: Expression[ParsableType] => (transformExpression(e), context)
        }
    }

    /* adds the modifier from [t] to [tr], assuming that [table] is the
     * symbol table of the type [tr] */
    private def addModifier(
                               tr: RawType,
                               table: DeclarationTable,
                               mods: Seq[TypeModifier]): ResolvedType = {

        val defaultMod = table.contract.ast.mod match {
            case Some(m) => m
            case None => IsOwned()
        }

        /* if a reference is 'readonly', it is labeled as such; otherwise, it is
         * 'owned'/'shared', based on the declaration of the contract itself */
        if (mods.exists(_.isInstanceOf[IsReadOnly])) {
            ReadOnlyRef(table, tr)
        } else if (defaultMod.isInstanceOf[IsOwned]) {
            OwnedRef(table, tr)
        } else {
            // TODO: are main contracts always deemed shared by the type system?
            SharedRef(table, tr)
        }
    }

    def transformType(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            context: Context,
            t: ParsableType): ResolvedType = {
        t match {
            case ParsableBoolType() => BoolType()
            case ParsableIntType() => IntType()
            case ParsableStringType() => StringType()
            case nonPrim@ParsableNonPrimitiveType(mods, ids) =>
                val tCanonified = canonifyParsableType(table, context, nonPrim)
                val result = resolveNonPrimitiveTypeContext(table, lexicallyInsideOf, tCanonified,
                                                            new TreeSet(), context)
                result match {
                    case Left(_) => BottomType()
                    case Right((tr, declTable)) =>
                        addModifier(tr, declTable, nonPrim.mods)
                }
        }
    }

    type TraverseResult = Either[Error, (RawType, DeclarationTable)]

    private def appendToPath(
            f: String,
            result: TraverseResult): TraverseResult = {
        result match {
            case Left(_) => result
            case Right((PathType(path, ts), table)) =>
                Right((PathType(f +: path, ts), table))
            case Right((NoPathType(ts), table)) =>
                Right((PathType(f::Nil, ts), table))
        }
    }

    private def canonifyParsableType(
            table: SymbolTable,
            context: Context,
            t: ParsableNonPrimitiveType): ParsableNonPrimitiveType = {
        if (t.identifiers.length == 1) {
            val cName = t.identifiers.head
            table.contract(cName) match {
                case Some(ct) => t
                case None => ParsableNonPrimitiveType(t.mods, "this" +: t.identifiers)
            }
        }
        if (t.identifiers.length == 2) {
            if (context contains t.identifiers.head) return t

            val cNamePossible = t.identifiers.head
            val sNamePossible = t.identifiers.tail.head

            // see if this interpretation of the type works
            table.contract(cNamePossible) match {
                case Some(ct) =>
                    ct.state(sNamePossible) match {
                        case Some(st) => return t
                        case None => ()
                    }
                case None => ()
            }

            ParsableNonPrimitiveType(t.mods, "this" +: t.identifiers)

        } else {
            if (context contains t.identifiers.head) t
            else ParsableNonPrimitiveType(t.mods, "this" +: t.identifiers)
        }
    }

    /* [resolveRawType] returns either an error that was reached while checking
     * (if [tr] could not be traversed), or the declaration table of the type,
     * as well as new raw type: this return value is only different from [tr]
     * if [tr] starts with an implicit "this" (in this case, "this" is added) */

    private def resolveNonPrimitiveTypeContext(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            t: ParsableNonPrimitiveType,
            visitedLocalVars: Set[String],
            context: Context): TraverseResult = {

        if (t.identifiers.length == 1) {
            val cName = t.identifiers.head
            lexicallyInsideOf.lookupContract(cName) match {
                case Some(ct) =>
                    val tRaw = NoPathType(JustContractType(cName))
                    Right((tRaw, ct))
                case None => Left(ContractUndefinedError(cName))
            }
        }
        else {
            val pathHead = t.identifiers.head
            val pathRest = t.identifiers.tail

            if (pathHead == "this") {
                val tNew = ParsableNonPrimitiveType(t.mods, pathRest)
                val result =
                    resolveNonPrimitiveTypeNoContext(table, lexicallyInsideOf, tNew, new TreeSet())
                appendToPath("this", result)
            }

            if (t.identifiers.length == 2) {
                val cNamePossible = pathHead
                val sNamePossible = pathRest.head

                // see if this interpretation of the type works
                lexicallyInsideOf.lookupContract(cNamePossible) match {
                    case Some(ct) =>
                        ct.state(sNamePossible) match {
                            case Some(st) =>
                                val tr = NoPathType(StateType(cNamePossible, sNamePossible))
                                return Right((tr, st))
                            case None => ()
                        }
                    case None => ()
                }
            }

            /* the head must be a variable */

            if (visitedLocalVars contains pathHead) {
                Left(RecursiveVariableTypeError(pathHead))
            }
            val newVisited = visitedLocalVars + pathHead

            val pathHeadType = context(pathHead) match {
                case trNext: ParsableNonPrimitiveType => trNext
                case prim =>
                    val primType = transformType(table, lexicallyInsideOf, context, prim)
                    return Left(DereferenceError(primType))
            }

            val newInsideOf =
                resolveNonPrimitiveTypeContext(table, lexicallyInsideOf, pathHeadType,
                                               newVisited, context) match {
                    case l@Left(_) => return l
                    case Right(travData) => travData._2
                }

            val tNew = ParsableNonPrimitiveType(t.mods, pathRest)

            val result = resolveNonPrimitiveTypeNoContext(table, newInsideOf, tNew, new TreeSet())

            appendToPath(pathHead, result)
        }
    }

    private def resolveNonPrimitiveTypeNoContext(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            t: ParsableNonPrimitiveType,
            visitedFields: Set[(DeclarationTable, String)]): TraverseResult = {

        if (t.identifiers.length == 1) {
            val cName = t.identifiers.head
            lexicallyInsideOf.lookupContract(cName) match {
                case Some(ct) =>
                    val tRaw = NoPathType(JustContractType(cName))
                    Right((tRaw, ct))
                case None => Left(ContractUndefinedError(cName))
            }
        } else {
            val pathHead = t.identifiers.head
            val pathRest = t.identifiers.tail

            if (pathHead == "parent") {
                if (lexicallyInsideOf.contract.hasParent) {
                    val tNew = t.copy(identifiers = pathRest)
                    val newInsideOf = lexicallyInsideOf.contract.parent.get
                    val result = resolveNonPrimitiveTypeNoContext(table, newInsideOf, tNew, visitedFields)
                    appendToPath("parent", result)
                } else {
                    Left(NoParentError(lexicallyInsideOf.contract.name))
                }
            }

            if (t.identifiers.length == 2) {
                val cNamePossible = pathHead
                val sNamePossible = pathRest.head

                // see if this interpretation of the type works
                lexicallyInsideOf.lookupContract(cNamePossible) match {
                    case Some(ct) =>
                        ct.state(sNamePossible) match {
                            case Some(st) =>
                                val tr = NoPathType(StateType(cNamePossible, sNamePossible))
                                return Right((tr, st))
                            case None => ()
                        }
                    case None => ()
                }
            }

            /* the head must be a field */

            val fieldLookup = lexicallyInsideOf.lookupFieldRaw(pathHead)
            if (fieldLookup.isEmpty) {
                return Left(FieldUndefinedError(lexicallyInsideOf.simpleType, pathHead))
            }

            val field = fieldLookup.get

            // paths must consist entirely of [const] fields
            if (!field.isConst) {
                return Left(FieldNotConstError(lexicallyInsideOf.name, pathHead))
            }

            if (visitedFields contains (lexicallyInsideOf, pathHead)) {
                return Left(RecursiveFieldTypeError(lexicallyInsideOf.name, pathHead))
            }

            val nonPrim = field.typ match {
                case tNonPrim: ParsableNonPrimitiveType => tNonPrim
                case prim =>
                    val tRes = transformType(table, lexicallyInsideOf, new TreeMap(), prim)
                    return Left(DereferenceError(tRes))
            }

            val newVisited = visitedFields.+((lexicallyInsideOf, pathHead))

            val traverseField =
                resolveNonPrimitiveTypeNoContext(table, lexicallyInsideOf, nonPrim, newVisited)

            val newInsideOf = traverseField match {
                case Left(err) => return Left(err)
                case Right(res) => res._2
            }

            val tNew = t.copy(identifiers = pathRest)

            val result = resolveNonPrimitiveTypeNoContext(table, lexicallyInsideOf, tNew, newVisited)
            appendToPath(pathHead, result)
        }
    }

}
