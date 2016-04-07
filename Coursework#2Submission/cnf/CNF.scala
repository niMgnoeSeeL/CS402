object CNF {

    def main(args: Array[String]) {
        val formula: Formula = toFormula(args.toList)
        val cNF: Formula = formToCNF(formula)
        println(FormulaUtil.prefix(cNF))
        println(FormulaUtil.infix(cNF))
        if(CNFmini.checkValid(cNF)) println("Valid") else println("Not Valid")
    }

    def formToCNF(formula: Formula): Formula = nNFToCNF(imFeToNNF(formToImFe(formula)))

    def toFormula(argval: List[String]): Formula = {
        var args: List[String] = argval
        if(args.isEmpty) System.err.println("no list argument")
        var root: Formula = null

        args.head match {
            case "&" => root = NonTerminal(null, And, null)
            case "|" => root = NonTerminal(null, Or, null)
            case ">" => root = NonTerminal(null, Implic, null)
            case "<" => root = NonTerminal(null, RevImp, null)
            case "=" => root = NonTerminal(null, Equiv, null)
            case "-" => root = NonTerminal(Nothing, Not, null)
            case s: String => {
                root = Terminal(s)
                CNFmini.addNewId(s)
                return root
            }
        }
        args = args.tail

        var nextNode: NonTerminalList = new NonTerminalList
        nextNode.append(root.asInstanceOf[NonTerminal])

        while(!args.isEmpty){
            args.head match {
                case "&" => {
                    val newNode = NonTerminal(null, And, null)
                    nextNode.pop.insert(newNode)
                    nextNode.append(newNode)
                }
                case "|" => {
                    val newNode = NonTerminal(null, Or, null)
                    nextNode.pop.insert(newNode)
                    nextNode.append(newNode)
                }
                case ">" => {
                    val newNode = NonTerminal(null, Implic, null)
                    nextNode.pop.insert(newNode)
                    nextNode.append(newNode)
                }
                case "<" => {
                    val newNode = NonTerminal(null, RevImp, null)
                    nextNode.pop.insert(newNode)
                    nextNode.append(newNode)
                }
                case "=" => {
                    val newNode = NonTerminal(null, Equiv, null)
                    nextNode.pop.insert(newNode)
                    nextNode.append(newNode)
                }
                case "-" => {
                    val newNode = NonTerminal(Nothing, Not, null)
                    nextNode.pop.insert(newNode)
                    nextNode.append(newNode)
                }
                case s: String => {
                    val newNode = Terminal(s)
                    CNFmini.addNewId(s)
                    nextNode.pop.insert(newNode)
                }
            }
            args = args.tail
        }

        root
    }

    def formToImFe(formula: Formula): Formula = formula match {
        case Terminal(s) => formula
        case NonTerminal(_, Not, r) => NonTerminal(Nothing, Not, formToImFe(r))
        case NonTerminal(l, Implic, r) => NonTerminal(NonTerminal(Nothing, Not, formToImFe(l)), Or, formToImFe(r))
        case NonTerminal(l, RevImp, r) => formToImFe(NonTerminal(r, Implic, l))
        case NonTerminal(l, Equiv, r) => formToImFe(NonTerminal(NonTerminal(l, Implic, r), And, NonTerminal(r, Implic, l)))
        case NonTerminal(l, op, r) => NonTerminal(formToImFe(l), op, formToImFe(r))
    }

    def imFeToNNF(imFe: Formula): Formula = imFe match {
        case Terminal(s) => imFe
        case NonTerminal(_, Not, r1) => r1 match {
            case NonTerminal(_, Not, r2) => imFeToNNF(r2)
            case NonTerminal(l2, And, r2) => imFeToNNF(NonTerminal(NonTerminal(Nothing, Not, l2), Or, NonTerminal(Nothing, Not, r2)))
            case NonTerminal(l2, Or, r2) => imFeToNNF(NonTerminal(NonTerminal(Nothing, Not, l2), And, NonTerminal(Nothing, Not, r2)))
            case _ => imFe // -p, -q etc.
        }
        case NonTerminal(l, And, r) => NonTerminal(imFeToNNF(l), And, imFeToNNF(r))
        case NonTerminal(l, Or, r) => NonTerminal(imFeToNNF(l), Or, imFeToNNF(r))
    }
    
    def distr(eta1: Formula, eta2: Formula): Formula = {
        (eta1, eta2) match {
            case (NonTerminal(eta11, And, eta12), e2) => NonTerminal(distr(eta11, e2), And, distr(eta12, e2))
            case (e1, NonTerminal(eta21, And, eta22)) => NonTerminal(distr(e1, eta21), And, distr(e1, eta22))
            case (e1, e2) => NonTerminal(e1, Or, e2)
        }
    }


    def nNFToCNF(nNF: Formula): Formula = nNF match {
        case Terminal(s) => nNF
        case NonTerminal(_, Not, r) => nNF // -p, -q etc.
        case NonTerminal(l, And, r) => NonTerminal(nNFToCNF(l), And, nNFToCNF(r))
        case NonTerminal(l, Or, r) => distr(nNFToCNF(l), nNFToCNF(r))
    }

}