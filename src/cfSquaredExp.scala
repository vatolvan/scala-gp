package scalagp

class cfSquaredExp(val x: Array[Double], l: Double = 0.2, m: Double = 0.1) {  
    var lengthScale = l
    var magnSigma2 = m
    val K = computeCovariance(x, x)  
    
    def computeCovariance(x1 : Array[Double], x2 : Array[Double]): Matrix = {
        var tmp = new Matrix(x1.length, x2.length)
        var d1 = 0;
        var d2 = 0;
        var iter = 0;
        for (d1 <- x1; d2 <- x2) {            
            val d = magnSigma2 * Math.exp(-Math.pow(d1-d2,2)/(2*Math.pow(lengthScale,2)));
            tmp.set(iter.toInt, d)
            iter = iter + 1
        }
        return tmp
    }
}