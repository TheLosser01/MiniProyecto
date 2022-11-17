def simpsom(a: Int, b: Int, f: Double => Double): Double = {
    (b - a) * ((f(a) + 4*f((a+b)/2) + f(b)) / 6)
}

val f1 = (x : Double)  => (-(Math.pow(x, 2)) + (8 * x) - 12)
val s1 = simpsom(3, 5, f1)

val f2 = (x : Double) => (3 * (Math.pow(x, 2)))
val s2 = simpsom (0, 2, f2)

val f3 = (x : Double) => (x + 2 * (Math.pow(x, 2) - (Math.pow(x, 3)) + 5 * (Math.pow(x, 4))))
val s3 = simpsom(-1, 1, f3)

val f4 = (x : Double) => ((2 * x + 1)/(math.pow(x, 2) + x))
val s4 = simpsom(1, 2, f4)

val f5 = (x : Double) => (Math.pow(Math.E, x))
val s5 = simpsom(0, 1, f5)

val f6 = (x : Double) => (1 / (math.sqrt(x - 1)))
val s6 = simpsom(2, 3, f6)

val f7 = (x : Double) => (1 / (math.sqrt(1 + (Math.pow(x, 2)))))
val s7 = simpsom(0, 1, f7)



def simpsomCompuesta(a: Int, b: Int, n : Int, f: Double => Double): Double = {
    val h = (b - a) / n
    val xj = (j : Double) => a + (j * h)
    val ecua = (j : Double) => f (xj(2 * j - 2)) + 4 * f(xj (2 * j - 1)) + f(xj(2 * j))
    (1 to 2).map(ecua(_))(h / 3)  
}

val sC1 = simpsomCompuesta(3, 5, 2, f1)
val sC2 = simpsomCompuesta(0, 2, 2, f2)
