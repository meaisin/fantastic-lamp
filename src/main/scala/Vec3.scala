class Vec3(
  val e0: Double,
  val e1: Double,
  val e2: Double) {

    override def toString: String =
      s"[$e0;$e1;$e2]"

    val (x, r) = (e0, e0)
    val (y, g) = (e1, e1)
    val (z, b) = (e2, e2)

    def negative: Vec3 = {
      Vec3(this.e0 * -1, this.e1 * -1, this.e2 * -1)
    }

    def +(that: Vec3): Vec3 = {
      Vec3(this.e0 + that.e0, this.e1 + that.e1, this.e2 + that.e2)
    }

    def -(that: Vec3): Vec3 = {
      Vec3(this.e0 - that.e0, this.e1 - that.e1, this.e2 - that.e2)
    }

    def *(that: Vec3): Vec3 = {
      Vec3(this.e0 * that.e0, this.e1 * that.e1, this.e2 * that.e2)
    }

    def *(that: Double): Vec3 = {
      Vec3(this.e0 * that, this.e1 * that, this.e2 * that)
    }

    def /(that: Vec3): Vec3 = {
      Vec3(this.e0 / that.e0, this.e1 / that.e1, this.e2 / that.e2)
    }

    def /(that: Double): Vec3 = {
      Vec3(this.e0 / that, this.e1 / that, this.e2 / that)
    }

    def dot(that: Vec3): Double = {
      (this.e0 * that.e0) + (this.e1 * that.e1) + (this.e2 * that.e2)
    }

    def cross(that: Vec3): Vec3 = {
      Vec3(
        (this.e1 * that.e2) - (this.e2 * that.e1),
        (-1 * (this.e0 * that.e2) - (this.e2 * that.e0)),
        (this.e0 * that.e1) - (this.e1 * that.e1))
    }

    def unitVector: Vec3 = {
      this / this.length
    }

    val squaredLength: Double = {
      (e0 * e0) + (e1 * e1) + (e2 * e2)
    }

    val length: Double = {
      Math.sqrt(squaredLength)
    }

    def apply(idx: Int): Double = {
      idx match
        case 0 => e0
        case 1 => e1
        case 2 => e2
        case _ => throw new RuntimeException("Invalid index into Vec3 object.")
    }

  }
