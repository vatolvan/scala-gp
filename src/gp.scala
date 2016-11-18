package scalagp

import Math._

class gp(val x: Array[Double], val ya : Array[Double]) {
  var cf = new cfSquaredExp(x);
  
  val y = new Matrix(ya.map(yy => Array(yy))) 
  
  var noiseSigma2 = 0.5;
  
  def predict(xt : Array[Double]): Array[Double] = {
        val n = y.nrow;
        val N = Util.identityMatrix(n)*noiseSigma2;
        val Kstar = cf.computeCovariance(xt, x)
        val Kstarstar = cf.computeCovariance(xt,xt)
        val L = (cf.K + N).chol();
        val a = L\y;
        
        // t = K(x,xt)*inv(K(x,x) + noise)*y
        val t = Kstar * (L.transpose()\a);
        
        val id = (0 to xt.length-1).toArray
        val tt = id.map(p => t.get(p))
        return tt
    }
   
    def energy(): Double = {
        try {
          val n = y.nrow;
          val N = Util.identityMatrix(n)*noiseSigma2;
          val L = (cf.K + N).chol();
          val id = (0 to L.ncol-1).toArray;
          val logDet = id.map(x => log(L.diag().get(x))).reduce(_ + _);
          val a = L\y;
          val tmp = a.transpose() * a;
          //println("log determinant = " + logDet + ", y'*inv(K)*y = " + tmp.get(0));
          return 0.5*tmp.get(0)/n + 0.5 * logDet/n + 0.5*log(2*PI)
        } catch {
          case e:RuntimeException => return 1e10
        }
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
     
      this.noiseSigma2 = n + eps;
      val d1 = this.energy();      
      this.noiseSigma2 = n - eps;
      val d2 = this.energy();      
      val ng = (d1-d2)/(2*eps);
      
      this.setCfParameters(l+eps, m);
      val dl1 = this.energy();      
      this.setCfParameters(l-eps, m);
      val dl2 = this.energy();      
      val lg = (dl1-dl2)/(2*eps);
      
      this.setCfParameters(l, m+eps);
      val dm1 = this.energy();      
      this.setCfParameters(l, m-eps);
      val dm2 = this.energy();      
      val mg = (dm1-dm2)/(2*eps);
            
      return Array(ng,lg,mg);
    }
    
    def optimizeParameters2(step: Double = 0.1) = {
      var n = this.noiseSigma2;
      var l = this.cf.lengthScale;
      var m = this.cf.magnSigma2;
      
      println("Energy = " + this.energy() + ", noiseSigma2 = " + n + ", lengthScale = " + l + ", magnSigma2 = " + m); 
      var g = this.energy_gradient(Array(n,l,m));
      var prevg = g;
      
      var e = this.energy();
      var preve = e + 1;
      val diff = 1e-8;
      
      while (e < preve - diff) {
        n = n - step*g(0);
        l = l - step*g(1);
        m = m - step*g(2);
        this.setParameters(n,l,m);
        println("Energy = " + this.energy() + ", noiseSigma2 = " + n + ", lengthScale = " + l + ", magnSigma2 = " + m); 
        preve = e;
        e = this.energy();
        g = this.energy_gradient(Array(n,l,m));
        g = Array(0.2*prevg(0)+g(0), 0.2*prevg(1)+g(1), 0.2*prevg(2)+g(2)) 
      }
      
    }
}