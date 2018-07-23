package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng1) = rng.nextInt
    val (i2, rng2) = rng1.nextInt
    ((i1, i2), rng2)
  }

  def main(args: Array[String]): Unit = {
    println("simple: " + simple(10L).nextInt)
    println("simple: " + simple(20L).nextInt)
    println("simple: " + simple(30L).nextInt)
    println("simple: " + simple(40L).nextInt)
    println("simple: " + simple(50L).nextInt)
    println("randomPair: " + randomPair(simple(50L)))

  }
}