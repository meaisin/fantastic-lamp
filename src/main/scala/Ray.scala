class Ray(val a: Vec3, val b: Vec3) {
  def origin: Vec3 = a
  def direction: Vec3 = b

  def pointAtDirection(t: Double): Vec3 = {
    a + b*t
  }
}
