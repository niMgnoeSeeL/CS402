trait Operator {
    val op: String
}
case object And extends Operator {
    val op: String = "&"
}
case object Or extends Operator {
    val op: String = "|"
}
case object Not extends Operator {
    val op: String = "-"
}
case object Implic extends Operator {
    val op: String = ">"
}
case object RevImp extends Operator {
    val op: String = "<"
}
case object Equiv extends Operator {
    val op: String = "="
}

trait Formula
case class Terminal(name: String) extends Formula
case object Nothing extends Formula
case class NonTerminal(var left: Formula, op: Operator, var right: Formula) extends Formula {
    def insert(f: Formula) = {
        op match {
            case Not => {
                if (right != null) System.err.println("node is already full")
                right = f
            }
            case _ => {
                if (left != null && right != null) System.err.println("node is already full")
                else if (left != null) right = f
                else left = f
            }
        }
    }
}

object FormulaUtil {
    def insert(f: Formula, n: NonTerminal) = {
        n.op match {
            case Not => {
                if (n.right != null) System.err.println("node is already full")
                n.right = f
            }
            case _ => {
                if (n.left != null && n.right != null) System.err.println("node is already full")
                else if (n.left != null) n.right = f
                else n.left = f
            }
        }
    }

    def prefix(f: Formula): String = {
        f match {
            case Terminal(s) => s
            case NonTerminal(_, Not, r) => "- " + prefix(r)
            case NonTerminal(l, op, r) => "(" + op.op + " " + prefix(l) + " " + prefix(r) + ")"

        }
    }

    def infix(f: Formula): String = {
        f match {
            case Terminal(s) => s
            case NonTerminal(_, Not, r) => "- " + infix(r)
            case NonTerminal(l, op, r) => "(" + infix(l) + " " + op.op + " " + infix(r) + ")"
        }
    }
}

class NonTerminalList {
    var nextList: List[NonTerminal] = List()

    def apply(i: Int) = nextList(i)

    def append(f: NonTerminal) = {
        nextList = f :: nextList
    }

    def pop: NonTerminal = {
        while(nextList.head.left != null && nextList.head.right != null) nextList = nextList.tail
        nextList.head
    }
}

