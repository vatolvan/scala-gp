package scalagp

import Math._

class gp(val x: Array[Double], val ya : Array[Double]) {
  var cf = new cfSquaredExp(x);

  val y = new Matrix(ya.map(yy => Array(yy)))

  var noiseSigma2 = 0.1;

  def predict(xt : Array[Double]): Array[Array[Double]] = {
        val n = y.nrow;
        val N = Util.identityMatrix(n)*noiseSigma2;
        val Kstar = cf.computeCovariance(xt, x)
        val Kstarstar = cf.computeCovariance(xt,xt)
        val L = (cf.K + N).chol();
        val a = L\y;

        // t = K(x,xt)*inv(K(x,x) + noise)*y
        val t = Kstar * (L.transpose()\a);

        // C = K(xt,xt) - K(xt,x)*inv(K(x,x))*K(x,xt)
        val tmp2 = L\Kstar.transpose()
        val C = (Kstarstar - tmp2.transpose()*tmp2).diag();
        println(C.get(0))

        val id = (0 to xt.length-1).toArray
        val tt = id.map(p => Array(t.get(p), C.get(p)))
        println(tt(0)(0) + ", " + tt(0)(1))
        return tt
    }

    def energy(param: Array[Double]): Double = {
        val prevp = Array(noiseSigma2, cf.lengthScale, cf.magnSigma2)
        this.setParameters(param(0), param(1), param(2))
        var en = 0.0;
        try {
          val n = y.nrow;
          val N = Util.identityMatrix(n)*noiseSigma2;
          val L = (cf.K + N).chol();
          val id = (0 until L.ncol).toArray;
          val logDet = id.map(x => log(L.diag().get(x))).sum;
          val a = L\y;
          val tmp = a.transpose() * a;
          //println("log determinant = " + logDet + ", y'*inv(K)*y = " + tmp.get(0));
          en = 0.5*tmp.get(0)/n + 0.5 * logDet/n + 0.5*log(2*PI)
        } catch {
          case e:RuntimeException => {
            en = 1e10
          }
        }
        this.setParameters(prevp(0), prevp(1), prevp(2))
        return en
    }

    def setCfParameters(l: Double, m: Double) = {
        this.cf = new cfSquaredExp(x, l, m);
    }

    def setParameters(n: Double, l: Double, m: Double) {
        this.noiseSigma2 = n;
        this.setCfParameters(l,m);
    }

    def energy_gradient(param: Array[Double]): Array[Double] = {
      val eps = 1e-6;
      val n = param(0);
      val l = param(1);
      val m = param(2);

      val d1 = this.energy(Array(n + eps, l, m))
      val d2 = this.energy(Array(n - eps, l, m))
      val ng = 0;
      //val ng = (d1-d2)/(2*eps);


      val dl1 = this.energy(Array(n, l + eps, m))
      val dl2 = this.energy(Array(n, l - eps, m))
      val lg = (dl1-dl2)/(2*eps)

      val dm1 = this.energy(Array(n, l, m + eps))
      val dm2 = this.energy(Array(n, l, m - eps))
      val mg = (dm1-dm2)/(2*eps)

      return Array(ng,lg,mg);
    }

    def optimizeParameters2() = {
      var n = this.noiseSigma2
      var l = this.cf.lengthScale
      var m = this.cf.magnSigma2


      var e = this.energy(Array(n,l,m));
      var g = this.energy_gradient(Array(n,l,m));
      var preve = e + 1;
      val diff = 1e-6;

      println("Energy = " + e + ", noiseSigma2 = " + n + ", lengthScale = " + l + ", magnSigma2 = " + m);
      var s = 0.0
      var i = 0;
      while (e < preve - diff) {
        i = i + 1
        preve = e
        s = Util.lineSearch(this.energy, Array(-g(0), -g(1), -g(2)), Array(n,l,m), 1)
        n = n - s*g(0)
        l = l - s*g(1)
        m = m - s*g(2)
        e = this.energy(Array(n,l,m))
        g = this.energy_gradient(Array(n,l,m))
        println("Iter = " + i + ", Energy = " + f"$e%1.5f" + ", noiseSigma2 = " + f"$n%1.5f" + ", lengthScale = " + f"$l%1.5f" + ", magnSigma2 = " + f"$m%1.5f");
      }
    }
}