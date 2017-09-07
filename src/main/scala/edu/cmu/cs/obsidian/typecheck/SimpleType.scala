package edu.cmu.cs.obsidian.typecheck

/* [SimpleType] simply indicates a contract, and possibly a state or set of states: there
 * is neither a permission nor a path associated with the type */
sealed trait SimpleType { val contractName: String }

case class JustContractType(contractName: String) extends SimpleType {
    override def toString: String = contractName
}
/* Invariant: [stateNames] is missing at least one of the states of the
 * contract (i.e. it is more specific than [JustContractType(contractName)],
 * but has at least 2 distinct states */
case class StateUnionType(contractName: String, stateNames: Set[String]) extends SimpleType {
    private def orOfStates: String = stateNames.toSeq.tail.foldLeft(stateNames.head)(
        (prev: String, sName: String) => prev + " | " + sName
    )
    override def toString: String = contractName + "." + "(" + orOfStates + ")"
}

case class StateType(contractName: String, stateName: String) extends SimpleType {
    override def toString: String = contractName + "." + stateName
}
