package scalagp
import org.ejml.simple._

object Util {
    
    def readData(str: String): Array[Array[Double]] = {
        val lines = scala.io.Source.fromFile("./src/" + str).mkString  
        val data = lines.split("\n").map(x => x.split(",").map(x => x.toDouble))
        return data
    }
    
    def identityMatrix(n: Int): Matrix = {
        val M2 = new Matrix(n, n, null, true, SimpleMatrix.identity(n))
        return M2
    }
}