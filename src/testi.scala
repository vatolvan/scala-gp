import scalagp.gp
import scalagp.Util

object testi {
  def main(args: Array[String]): Unit = {    
    // Load test data
    val data = Util.readData("testi.txt");
     
    val gp = new gp(data.map(x=>x(0)), data.map(x=>x(1)));
    
    // Optimize parameters
    gp.optimizeParameters2(0.005);
    
    val t = gp.predict((-5.0 to 5.0 by 0.1).toArray);
    println(t.mkString(","))

  }  
}
