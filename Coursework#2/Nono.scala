import scala.io.Source
import scala.sys.process._
import java.io._

object Nono {
    def main(args: Array[String] = Array("input.txt")) { 
        val inputList = Source.fromFile(args(0)).toList.map(_.toInt - 48)
        val sizeX = inputList(0)
        val sizeY = inputList(2)

        var rowList: List[List[Int]] = Nil
        var colList: List[List[Int]] = Nil
        val rowsAndColumns = inputList.tail.tail.tail.tail
        var rowcount = 0
        var lineList: List[Int] = List()

        for(i <- rowsAndColumns) i match {
            case -38 => {
                if (rowcount < sizeX) rowList = rowList :+ lineList
                else colList = colList :+ lineList
                rowcount = rowcount + 1
                lineList = List()
            }
            case -16 => {}
            case n => lineList = lineList :+ n
        }

        //debug
        println(sizeX)
        println(sizeY)
        println(rowList)
        println(colList)

        //val cNF = createCNF(sizeX, sizeY, rowList, colList)
        //CNFmini.miniSATGen("Nono", cnf)
        //"minisat Nono.in Nono.out".!

    }



    def createPivotIndex(size: Int, cnum: Int): List[List[Int]] = {
        var ret: List[List[Int]] = List()
        if (size < cnum*2-1) System.err.println("UNSATISFIABLE")
        var pivotIndex: List[Int] = (1 to cnum).toList map (_ * 2 - 1)
        var i = cnum-1
        while(pivotIndex(0) <= size - 2*(cnum-1)){
            if (pivotIndex(cnum-1) <= size){
                ret = pivotIndex :: ret
                i = cnum-1
            }
            pivotIndex = pivotIndex.updated(i, pivotIndex(i) + 1)
            for(j <- (i+1) to cnum-1) {
                pivotIndex = pivotIndex.updated(j, pivotIndex(j-1) + 2)
            }
            i = i-1
        }
        ret
    }


    def wrapOr(formulaList: List[Formula]): Formula = {
        formulaList.length match {
            case 1 => formulaList(0)
            case _ => {
                val splitList = formulaList splitAt formulaList.length/2
                NonTerminal(wrapOr(splitList._1), Or, wrapOr(splitList._2))
            }
        }
    }


    def wrapAnd(formulaList: List[Formula]): Formula = {
        formulaList.length match {
            case 1 => formulaList(0)
            case _ => {
                val splitList = formulaList splitAt formulaList.length/2
                NonTerminal(wrapAnd(splitList._1), And, wrapAnd(splitList._2))
            }
        }
    }
}

