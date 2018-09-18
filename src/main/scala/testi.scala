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
        gp.setParameters(1,1,1)
        val t0 = System.nanoTime()
        val param1 = gp.optimizeParameters()
        val t1 = System.nanoTime()
        val param2 = Util.bfgs(gp.energy, gp.energy_gradient, Array(1,1,1))
        val t2 = System.nanoTime()

        println("Gradient descent: " + (t1-t0)*1e-9 + " seconds, bfgs: " + (t2-t1)*1e-9 + " seconds")
        println("Gradient descent params = " + param1(0) + ", " + param1(1) + ", " + param1(2))
        println("BFGS params = " + param2(0) + ", " + param2(1) + ", " + param2(2))

        gp.setParameters(param1(0), param1(1), param1(2))

        var pred = gp.predict((-2.0 to 2.0 by 0.1).toArray)

        var file = "output.txt"
        var writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))

        for (x <- pred) {
          writer.write(x(0) + "," + x(1) + "\n")  // however you want to format it
        }
        writer.close()

        gp.setParameters(param2(0), param2(1), param2(2))

        pred = gp.predict((-2.0 to 2.0 by 0.1).toArray)

        file = "output2.txt"
        writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))

        for (x <- pred) {
            writer.write(x(0) + "," + x(1) + "\n")  // however you want to format it
        }
        writer.close()

        println("Finished!")
  }
}
