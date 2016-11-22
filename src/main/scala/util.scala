package scalagp
import org.ejml.simple._
import java.lang.Math._

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

    def lineSearch(en: Array[Double] => Double, g0 : Array[Double], p0 : Array[Double], s0: Double): Double = {
        val n = sqrt(g0.map(x=>pow(x,2)).sum)
        var s = s0*n
        var prevs = s

        var p = p0
        var prevp = p
        var e = en(p0)
        var preve = e+1
        var i = 0
        var increasing = true
        // Try to increase the step first
        while (e < preve) {
            i = i + 1
            prevs = s
            preve = e
            prevp = p
            s = 1.5*s
            p = Array(p(0) + s*g0(0), p(1) + s*g0(1), p(2) + s*g0(2))
            e = en(p)

            //println("i = " + i + ", e = " + e + ", preve = " + preve + ", s = " + s + ", increasing s")
        }
        if (i == 1 && e > preve && s0 > 1e-2)
            prevs = lineSearch(en, g0, p0, s0*0.1)
        /*if (i>1) {
            return prevs
        }

        s = n
        prevs = s
        p = p0
        prevp = p
        e = en(p0)+1
        preve = en(p0)
        i = 0
        while (e > preve) {
            i = i + 1
            preve = e
            prevp = p
            s = 0.9*s
            p = Array(p(0) + s*g0(0), p(1) + s*g0(1), p(2) + s*g0(2))
            e = en(p)

            //println("i = " + i + ", e = " + e + ", preve = " + preve + ", s = " + s + ", decreasing s")

            if (s < 1e-8) {
                println("Failed to find acceptable step length")
                s = 0
                return s
            }
        }*/
        return prevs
    }

    def bfgs(energy: Array[Double] => Double, gradient: Array[Double] => Array[Double], param0: Array[Double]): Array[Double] = {
        var g = new Matrix(gradient(param0))
        var e = energy(param0)
        var preve = e + 1

        var B = Util.identityMatrix(3)

        val tolFun = 1e-6

        var i = 0
        var param = param0
        var p = new Matrix(Array(0.0,0.0,0.0))
        var a = 0.0
        var s = new Matrix(Array(0.0,0.0,0.0))
        var y = new Matrix(Array(0.0,0.0,0.0))
        while (e < preve - tolFun) {
            preve = e
            i = i + 1
            p = conjugateGradient(g*(-1), B)

            a = lineSearch(energy, Array(p.get(0),p.get(1),p.get(2)), Array(param(0), param(1), param(2)), 1)
            val tmp = p*a
            param = Array(param(0) + tmp.get(0), param(1) + tmp.get(1), param(2) + tmp.get(2))
            e = energy(param)
            s = p*a
            y = new Matrix(gradient(param)) - g
            g = new Matrix(gradient(param))
            B = B + (y*y.transpose())*(1.0/(y.transpose()*s).get(0)) - (B*s*s.transpose()*B)*(1.0/(s.transpose()*B*s).get(0))
            println("Iter = " + i + ", Energy = " + e + ", noiseSigma2 = " + param(0) + ", lengthScale = " + param(1) + ", magnSigma2 = " + param(2));
        }

        return Array(param(0), param(1), param(2))
    }

    def conjugateGradient(b: Matrix, A: Matrix): Matrix = {
        var x = new Matrix(Array(0.0,0.0,0.0));
        var r = b - A*x
        var p = r
        var rsold = r.transpose() * r
        var alpha = 0.0;
        var Ap = A*p
        var rsnew = r
        for (i <- 0 until b.ncol) {
            Ap = A*p
            alpha = ((p.transpose()*A*p).transpose()\rsold.transpose()).transpose().get(0)
            x = x + p*alpha
            r = r - Ap*alpha
            rsnew = r.transpose() * r
            if (sqrt(rsnew.get(0)) < 1e-10) {
                return x
            }
            p = r + p*(rsnew.get(0)/rsold.get(0))
            rsold = rsnew
        }
        return x
    }

}