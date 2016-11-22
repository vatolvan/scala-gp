import scalagp.gp
import scalagp.Util
import org.ejml.simple.SimpleMatrix
import java.io._

object testi {
    def main(args: Array[String]): Unit = {
        // Load test data
        val data = Util.readData("main/resources/testi.txt")

        val M = new SimpleMatrix(data)
        println(M.toString)
        val gp = new gp(data.map(x=>x(0)), data.map(x=>x(1)))

        // Optimize parameters
        gp.setParameters(0.1,1,1)
        val t0 = System.nanoTime()
        gp.optimizeParameters2()
        val t1 = System.nanoTime()
        Util.bfgs(gp.energy, gp.energy_gradient, Array(0.1,1,1))
        val t2 = System.nanoTime()

        println("Gradient descent: " + (t1-t0)*1e-9 + " seconds, bfgs: " + (t2-t1)*1e-9 + " seconds")


        val pred = gp.predict((-2.0 to 2.0 by 0.1).toArray)

        val file = "output.txt"
        val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))

        for (x <- pred) {
          writer.write(x(0) + "," + x(1) + "\n")  // however you want to format it
        }
        writer.close()
        println("Finished!")
  }
}
