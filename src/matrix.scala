package scalagp

import org.ejml.simple._
import org.ejml.alg.dense._;
import org.ejml.data.DenseMatrix64F;
import org.ejml.alg.dense.decomposition.chol;
import org.ejml.factory._

//case class Matrix(val i: Int, val j: Int, val d : Array[Array[Double]]) extends SimpleMatrix(d) {
class Matrix(val i: Int, val j: Int, val d : Array[Array[Double]], val o: Boolean, val m: SimpleMatrix) {
    val M = if (o) m else new SimpleMatrix(d);
    
    val nrow = i;
    val ncol = j;
  
    def this(i: Int, j: Int) = this(i, j, Array.ofDim[Double](i,j), false, null)
    
    def this(d: Array[Array[Double]]) = this(d.length, d(0).length, d, false, null)  
  
    def +(that: Matrix): Matrix = {
        val M2 = new Matrix(this.nrow, this.ncol, null, true, this.M.plus(that.M))
        return M2
    }
    
    def +(that: Double): Matrix = {
        val M2 = new Matrix(this.nrow, this.ncol, null, true, this.M.plus(that))
        return M2
    }
    
    def -(that: Matrix): Matrix = {
        val M2 = new Matrix(this.nrow, this.ncol, null, true, this.M.minus(that.M))
        return M2
    }
    
    
    def *(that: Matrix): Matrix = {
        val M2 = new Matrix(this.nrow, this.ncol, null, true, this.M.mult(that.M))
        return M2
    }
    
    def *(that: Double): Matrix = {
        var M2 = new Matrix(this.nrow, this.ncol, null, true, this.M.scale(that))
        return M2
    }
    
    def \(that: Matrix): Matrix = {
        val M2 = new Matrix(this.nrow, this.ncol, null, true, this.M.invert.mult(that.M))
        return M2
      //= this.M.invert.mult(that.M)
    }
    
    def transpose(): Matrix = {
        val M2 = new Matrix(this.nrow, this.ncol, null, true, this.M.transpose())
        return M2
    }
    
    def set(index: Int, value : Double) = this.M.set(index, value) 
    
    def get(index: Int) = this.M.get(index)
    
    def chol(): Matrix = {
        val chol = DecompositionFactory.chol(this.M.numRows(),true);
        
        val M2 = this.M;
        if( !chol.decompose(M2.getMatrix()))
           throw new RuntimeException("Cholesky failed!");
         
        val L = SimpleMatrix.wrap(chol.getT(null));
        return new Matrix(this.nrow, this.ncol, null, true, L);
    }
    
    def diag(): Matrix = {
        val D = this.M.extractDiag()
        return new Matrix(this.nrow, this.ncol, null, true, D);
    }    
}