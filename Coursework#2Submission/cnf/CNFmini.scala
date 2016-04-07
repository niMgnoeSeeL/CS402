import java.io._
import scala.collection.immutable.ListMap

object CNFmini {
    var idMap: scala.collection.immutable.Map[String, Int] = scala.collection.immutable.Map[String, Int]()
    var miniSAT: List[List[Int]] = List()
    var idCounter = 1

    def addNewId(name: String) {
        if (!idMap.contains(name)) {
            idMap = idMap + (name -> idCounter)
            idCounter = idCounter + 1
        }
    }

    def cNFToMiniSAT(cNF: Formula): List[List[Int]] = cNF match {
        case Terminal(s) => List(List(idMap(s)))
        case NonTerminal(_, Not, r) => r match {
            case Terminal(s) => List(List(-idMap(s)))
        }
        case NonTerminal(l, Or, r) => List(cNFToMiniSAT(l)(0) ::: cNFToMiniSAT(r)(0))
        case NonTerminal(l, And, r) => cNFToMiniSAT(l) ::: cNFToMiniSAT(r)
    }

    def miniSATGen(filename: String, cNF: Formula) {
        val writer = new PrintWriter(new File(filename + ".in"))
        miniSAT = cNFToMiniSAT(cNF)
        var column = ""
        ListMap(idMap.toSeq.sortBy(_._1):_*) foreach {case (key, value) => column = column + "c | " + key + " -> " + value + "\n" }
        writer.write(column)

        writer.write("p cnf " + idMap.size + " " + miniSAT.size + "\n")
        for (lst <- miniSAT) {
            var column = ""
            for (el <- lst) column = column + " " + el
            writer.write(column + " 0\n")
        }
        writer.close()

    }

    def checkValid(cNF: Formula): Boolean = {
        val miniSAT: List[List[Int]] = cNFToMiniSAT(cNF)
        val varNum: Int = idMap.size
        var varTrace: Array[Array[Boolean]] = Array()
        var validity: Boolean = true
        
        for (lst <- miniSAT) {
            var tempValidity: Boolean = false
            varTrace = Array()
            for (i <- 0 to varNum - 1) varTrace = varTrace :+ Array(false, false)
            for (el <- lst) {
                if (el > 0) varTrace(el - 1)(0) = true
                else varTrace(-el - 1)(1) = true
            }
            for (i <- 0 to varNum - 1) if(varTrace(i)(0) && varTrace(i)(1)) tempValidity = true
            validity = validity && tempValidity
        }

        validity
    }
}