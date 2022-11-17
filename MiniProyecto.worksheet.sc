val f1 = (x : Double)  => (-(Math.pow(x, 2)) + (8 * x) - 12)
val f2 = (x : Double) => (3 * Math.pow(x, 2))
val f3 = (x : Double) => (x + 2 * Math.pow(x, 2) - Math.pow(x, 3) + 5 * Math.pow(x, 4))
val f4 = (x : Double) => ((2 * x + 1)/(math.pow(x, 2) + x))
val f5 = (x : Double) => (Math.pow(Math.E, x))
val f6 = (x : Double) => (1 / (math.sqrt(x - 1)))
val f7 = (x : Double) => (1 / (math.sqrt(1 + Math.pow(x, 2))))


def simpsom(a: Double, b: Double, f: Double => Double): Double = {
    (b - a) * ((f(a) + 4*f((a+b)/2) + f(b)) / 6)
}


val s1 = simpsom(3, 5, f1)
val s2 = simpsom (0, 2, f2)
val s3 = simpsom(-1, 1, f3)
val s4 = simpsom(1, 2, f4)
val s5 = simpsom(0, 1, f5)
val s6 = simpsom(2, 3, f6)
val s7 = simpsom(0, 1, f7)


def simpsomCompuesta(a: Double, b: Double, f: Double => Double): Double = {
    val n = 2
    val h = ((b - a) / n).toDouble
    val x = ( 1 to n /2 ).map (j => (f(a) + 2 * j * h - 2) + 4 * f(a + 2 * j * h - 1) + f(a + 2 * j * h)).sum
    (h * x) / 3
}
val sC1 = simpsomCompuesta(3, 5, f1)
val sC2 = simpsomCompuesta(0, 2, f2)
val sC3 = simpsomCompuesta(-1, 1, f3)
val sC4 = simpsomCompuesta(1, 2, f4)
val sC5 = simpsomCompuesta(0, 1, f5)
val sC6 = simpsomCompuesta(2, 3, f6)
val sC7 = simpsomCompuesta(0, 1, f7)


def simpsomExtendida(a : Double, b: Double, f: Double => Double) : Double = {
    val n = (2 * (b - a))
    val h = ((b -a)/ n)
    val par = (2 * (2 to (n - 2).toInt by 2).map (j => f(a + j * h)).sum + f(b))
    val impar = (f(a) + 4 * (1 to (n - 1).toInt by 2).map (i => f(a + i * h)).sum)
    (h / 3) * (impar + par)
}

val sE1 = simpsomExtendida (3, 5, f1)
val sE2 = simpsomExtendida(0, 2, f2)
val sE3 = simpsomExtendida(-1, 1, f3)
val sE4 = simpsomExtendida(1, 2, f4)
val sE5 = simpsomExtendida(0, 1, f5)
val sE6 = simpsomExtendida(2, 3, f6)
val sE7 = simpsomExtendida(0, 1, f7)

def errorCalculo(a: Double, b: Double): Double = math.abs(a - b)

val eS1 = errorCalculo(7.33, s1)
val eS2 = errorCalculo(8, s2)
val eS3 = errorCalculo(3.333, s3)
val eS4 = errorCalculo(1.09861, s4)
val eS5 = errorCalculo(1.71828, s5)
val eS6 = errorCalculo(0.828427, s6)
val es7 = errorCalculo(0.785398, s7)

val eSC1 = errorCalculo(7.33, sC1)
val eSC2 = errorCalculo(8, sC2)
val eSC3 = errorCalculo(3.333, sC3)
val eSC4 = errorCalculo(1.09861, sC4)
val eSC5 = errorCalculo(1.71828, sC5)
val eSC6 = errorCalculo(0.828427, sC6)
val esC7 = errorCalculo(0.785398, sC7)

val eSE1 = errorCalculo(7.33, sE1)
val eSE2 = errorCalculo(8, sE2)
val eSE3 = errorCalculo(3.333, sE3)
val eSE4 = errorCalculo(1.09861, sE4)
val eSE5 = errorCalculo(1.71828, sE5)
val eSE6 = errorCalculo(0.828427, sE6)
val esE7 = errorCalculo(0.785398, sE7)
