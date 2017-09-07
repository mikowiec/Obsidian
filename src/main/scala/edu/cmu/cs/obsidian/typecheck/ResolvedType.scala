package edu.cmu.cs.obsidian.typecheck
import edu.cmu.cs.obsidian.parser._

/* [RawType] is a contract type that doesn't have a permission associated with it,
 * but potentially has a path. */
sealed trait RawType {
    val extractSimpleType: SimpleType
}

case class NoPathType(ts: SimpleType) extends RawType {
    override def toString: String = ts.toString
    override val extractSimpleType: SimpleType = ts
}

/* a path starts with either a local variable or "this", but "this" can sometimes be omitted */
case class PathType(path: Seq[String], ts: SimpleType) extends RawType {
    private def pathAsString = path.foldLeft("")(
        (prev: String, pathNode: String) => prev + pathNode + "."
    )
    override def toString: String = pathAsString + ts.toString
    override val extractSimpleType: SimpleType = ts
}

/* This is different from the representation of types in the AST in that the permission
 * associated with the reference is always explicit.
 * Invariant: any path that occurs in the type makes "this" explicit */
sealed trait ResolvedType {
    // for tests
    val isBottom: Boolean
    val tableOpt: Option[DeclarationTable]

    /* the permission system doesn't allow arbitrary aliasing of a reference
     * typed as [t]: aliasing forces one of the resulting types to be
     * [residualType(t)] instead */
    val residualType: ResolvedType

    val extractSimpleType: Option[SimpleType]
    val extractRawType: Option[RawType]
}

/* int, bool, or string */
sealed trait PrimitiveType extends ResolvedType {
    val isBottom: Boolean = false
    val tableOpt: Option[DeclarationTable] = None
    override val residualType: ResolvedType = this
    override val extractSimpleType: Option[SimpleType] = None
    override val extractRawType: Option[RawType] = None
}

sealed trait NonPrimitiveType extends ResolvedType {
    val isBottom: Boolean = false
    def table: DeclarationTable
    val tableOpt: Option[DeclarationTable] = Some(table)
}

/* all permissioned types are associated with their corresponding symbol table */
case class ReadOnlyRef(tableOf: DeclarationTable, t: RawType) extends NonPrimitiveType {
    override def table: DeclarationTable = tableOf
    override def toString: String = "readonly " + t.toString
    override def equals(other: Any): Boolean = {
        other match {
            case ReadOnlyRef(_, tr) => tr == t
            case _ => false
        }
    }
    override def hashCode(): Int = t.hashCode()
    override val residualType: ResolvedType = this
    override val extractSimpleType: Option[SimpleType] = Some(t.extractSimpleType)
    override val extractRawType: Option[RawType] = Some(t)
}
case class SharedRef(tableOf: DeclarationTable, t: RawType) extends NonPrimitiveType {
    override def table: DeclarationTable = tableOf
    override def toString: String = "shared " + t.toString
    override def equals(other: Any): Boolean = {
        other match {
            case SharedRef(_, tr) => tr == t
            case _ => false
        }
    }
    override def hashCode(): Int = t.hashCode()
    override val residualType: ResolvedType = this
    override val extractSimpleType: Option[SimpleType] = Some(t.extractSimpleType)
    override val extractRawType: Option[RawType] = Some(t)
}
case class OwnedRef(tableOf: DeclarationTable, t: RawType) extends NonPrimitiveType {
    override def table: DeclarationTable = tableOf
    override def toString: String = "owned " + t.toString
    override def equals(other: Any): Boolean = {
        other match {
            case OwnedRef(_, tr) => tr == t
            case _ => false
        }
    }
    override def hashCode(): Int = t.hashCode()
    override val residualType: ResolvedType = ReadOnlyRef(tableOf, t)
    override val extractSimpleType: Option[SimpleType] = Some(t.extractSimpleType)
    override val extractRawType: Option[RawType] = Some(t)
}
case class IntType() extends PrimitiveType {
    override def toString: String = "int"
}
case class BoolType() extends PrimitiveType {
    override def toString: String = "bool"
}
case class StringType() extends PrimitiveType {
    override def toString: String = "string"
}
/* Used to indicate an error in the type checker when a reasonable type cannot
 * otherwise be inferred */
case class BottomType() extends ResolvedType {
    val isBottom: Boolean = true
    val tableOpt: Option[DeclarationTable] = None
    override val residualType: ResolvedType = this
    override val extractSimpleType: Option[SimpleType] = None
    override val extractRawType: Option[RawType] = None
}
