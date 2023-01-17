import java.io.*

object App {
  @main def main(args: String*): Unit = {
    protorun()
  }

  private def protorun(): Unit = {
    val output: StringBuilder = new StringBuilder()

    val nx: Int = 200
    val ny: Int = 100

    output append s"P3\n$nx $ny\n255\n"

    val lowerLeftCorner: Vec3 = Vec3(-2.0, -1.0, -1.0)
    val horizontal: Vec3 = Vec3(4.0, 0.0, 0.0)
    val vertical: Vec3 = Vec3(0.0, 2.0, 0.0)
    val origin: Vec3 = Vec3(0.0, 0.0, 0.0)

    for
      j <- (0 to ny - 1).reverse
      i <- 0 to nx - 1
    do
      val u: Double = i.toDouble / nx.toDouble
      val v: Double = j.toDouble / ny.toDouble

      val r: Ray = 
        Ray(origin, lowerLeftCorner + horizontal * u + vertical * v)

      val col: Vec3 = Usefuls.color(r)

      val ir: Int = (255.99 * col(0)).toInt
      val ig: Int = (255.99 * col(1)).toInt
      val ib: Int = (255.99 * col(2)).toInt

      output append s"$ir $ig $ib\n"

    simpleWrite(output.toString, "test.ppm")
  }

  private def simpleWrite(content: String, filename: String): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(content)
    bw.close()
  }
}
