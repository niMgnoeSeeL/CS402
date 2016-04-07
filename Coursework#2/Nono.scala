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

        for(i <- 1 to sizeX){
            for(j <- 1 to sizeY) CNFmini.addNewId("m"+i+j)
        }
        for(i <- 1 to sizeX){
            for(j <- 1 to rowList(i-1).length){
                for(k <- 1 to sizeY) CNFmini.addNewId("pr"+j+i+k)
            } 
        }
        for(i <- 1 to sizeY){
            for(j <- 1 to colList(i-1).length){
                for(k <- 1 to sizeX) CNFmini.addNewId("pc"+j+k+i)
            } 
        }

        val formula: Formula = createFormula(sizeX, sizeY, rowList, colList)
        val cnf: Formula = CNF.formToCNF(formula)
        CNFmini.miniSATGen("Nono", cnf)

        "minisat Nono.in Nono.out".!

        val outputList = Source.fromFile("Nono.out").toList.tail.tail.tail.tail
        visualize(outputList, sizeX, sizeY)
    }

    def visualize(out: List[Char], sizeX: Int, sizeY: Int) {
        val t: String = "0 "
        val f: String = ". "
        val writer = new PrintWriter(new File("output.txt"))
        var counter: Int = 0
        var curr: Boolean = true
        var line: String = ""
        val iter = out.iterator
        while(counter < sizeX*sizeY){
            iter.next match {
                case ' ' => {
                    line = line + (if(curr) t else f)
                    counter = counter + 1
                    curr = true
                    if (counter % sizeY == 0) line = line+"\n"
                }
                case '-' => curr = false
                case _ => ()
            }
        }
        writer.write(line)
        writer.close()
    }

    def createFormula(sizeX: Int, sizeY: Int, rowList: List[List[Int]], colList: List[List[Int]]): Formula = {
        var wholeFormulaList: List[Formula] = List()
        var rowNumber = 1
        var colNumber = 1

        //add one pivot for one block
        var block3: List[Formula] = List()
        for(i <- 1 to sizeX){
            if (rowList(i-1)(0) != 0){
                var block2: List[Formula] = List()
                for(j <- 1 to rowList(i-1).length){
                    
                    var block1: List[Formula] = List()
                    for(k <- 1 to sizeY) block1 = NonTerminal(Nothing, Not, Terminal("pr"+j+i+k)) :: block1
                    for(k <- 1 to sizeY) block2 = wrapAnd(block1.updated(k-1, block1(k-1).asInstanceOf[NonTerminal].right)) :: block2
                } 
                block3 = wrapOr(block2) :: block3
            }
        }
        val rowblocks: Formula = wrapAnd(block3)

        block3 = List()
        for(i <- 1 to sizeY){
            if (colList(i-1)(0) != 0){
                var block2: List[Formula] = List()
                for(j <- 1 to colList(i-1).length){
                    
                    var block1: List[Formula] = List()
                    for(k <- 1 to sizeX) block1 = NonTerminal(Nothing, Not, Terminal("pc"+j+k+i)) :: block1
                    for(k <- 1 to sizeX) block2 = wrapAnd(block1.updated(k-1, block1(k-1).asInstanceOf[NonTerminal].right)) :: block2
                }
                block3 = wrapOr(block2) :: block3
            }
        }
        val colblocks: Formula = wrapAnd(block3)



        for (rL <- rowList) {
            if(rL(0) != 0) wholeFormulaList = createRowFormula(rowNumber, sizeY, rL) :: wholeFormulaList
            rowNumber = rowNumber + 1
        }
        for (cL <- colList) {
            if(cL(0) != 0) wholeFormulaList = createColFormula(colNumber, sizeX, cL) :: wholeFormulaList
            colNumber = colNumber + 1
        }
        wrapAnd(List(rowblocks, colblocks, wrapAnd(wholeFormulaList)))
    }
    

    def createRowFormula(rowNumber: Int, rowSize: Int, condList: List[Int]): Formula = {
        val pivotCondList = (wrapOr(createPivotIndex(rowSize, condList.length) map(_.zipWithIndex 
            map({ case (a, b) => Terminal("pr" + (b + 1) + rowNumber + a)})) map(wrapAnd(_))))
        val blockList0 = condList map ((1 to rowSize).toList.sliding(_).toList map (_ map ("m" + rowNumber + _)))
        val blockList1 = blockList0.zipWithIndex map ({case (block, index) => block map ({case (m) => 
            val withoutNeg = "pr" + (index + 1) + m(0).tail :: m
            var withNeg: List[String] = withoutNeg
            if (withoutNeg.tail.head.last.toInt-48 != 1) withNeg = ("-" + withoutNeg.tail.head.substring(0, 2) + (withoutNeg.tail.head.last.toInt-48 - 1)) :: withNeg
            if (withoutNeg.last.last.toInt-48 != rowSize) withNeg = ("-" + withoutNeg.last.substring(0, 2) + (withoutNeg.last.last.toInt-48 + 1)) :: withNeg
            withNeg})})
        val blockList2 = blockList1 map (_ map (_ map ({case (s) => 
            if(s.head != '-') Terminal(s)
            else NonTerminal(Nothing, Not, Terminal(s.tail))})))
        val blockList3 = blockList2 map (_ map (wrapAnd(_)))
        val blockList4 = blockList3 map (wrapOr(_))
        val blockCondList = wrapAnd(blockList4)
        wrapAnd(List(blockCondList, pivotCondList))
    }

    def createColFormula(colNumber: Int, colSize: Int, condList: List[Int]): Formula = {
        val pivotCondList = (wrapOr(createPivotIndex(colSize, condList.length) map(_.zipWithIndex 
            map({ case (a, b) => Terminal("pc" + (b + 1) + a + colNumber)})) map(wrapAnd(_))))
        val blockList0 = condList map ((1 to colSize).toList.sliding(_).toList map (_ map ("m" + _ + colNumber)))
        val blockList1 = blockList0.zipWithIndex map ({case (block, index) => block map ({case (m) => 
            val withoutNeg = "pc" + (index + 1) + m(0).tail :: m
            var withNeg: List[String] = withoutNeg
            if (withoutNeg.tail.head(1).toInt-48 != 1) withNeg = ("-" + withoutNeg.tail.head(0).toString + (withoutNeg.tail.head(1).toInt-48 - 1) + withoutNeg.tail.head(2)) :: withNeg
            if (withoutNeg.last(1).toInt-48 != colSize) withNeg = ("-" + withoutNeg.last(0).toString + (withoutNeg.last(1).toInt-48 + 1) + withoutNeg.last(2)) :: withNeg
            withNeg})})
        val blockList2 = blockList1 map (_ map (_ map ({case (s) => 
            if(s.head != '-') Terminal(s)
            else NonTerminal(Nothing, Not, Terminal(s.tail))})))
        val blockList3 = blockList2 map (_ map (wrapAnd(_)))
        val blockList4 = blockList3 map (wrapOr(_))
        val blockCondList = wrapAnd(blockList4)
        wrapAnd(List(blockCondList, pivotCondList))
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

