object Usefuls {
  def hitSphere(center: Vec3, radius: Double, r: Ray): Boolean = {
    val oc: Vec3 = r.origin - center
    val a: Double = r.direction.dot(r.direction)
    val b: Double = oc.dot(r.direction) * 2.0
    val c: Double = oc.dot(oc) - (radius * radius)
    val discriminant: Double = (b * b) - (4 * a * c)
    discriminant > 0
  }

  def color(r: Ray): Vec3 = {
    if hitSphere(Vec3(0, 0, -1), 0.5, r) then
      Vec3(1, 0, 0)
    else
      val unitDirection: Vec3 = r.direction.unitVector
      val t: Double = 0.5 * (unitDirection.y + 1.0)
      Vec3(1.0, 1.0, 1.0) * (1.0 - t) + Vec3(0.5, 0.7, 1.0) * t
  }
}
