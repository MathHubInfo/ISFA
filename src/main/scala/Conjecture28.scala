object Conjecture28 {

  def isPrime(n: Long): Boolean = {
    if(n == 1 || n == 2) true
    else if (n%2 == 0) false
    else !((3L until (n - 1L)) exists (n % _ == 0))
  }

  def main(args: Array[String]):Unit = {
    var found = false
    for(n <- 3 to 65)
      if((1 to n-1).forall(j => (1 to j-1).forall(i => isPrime(Math.pow(2,n).toLong + Math.pow(2,i).toLong + Math.pow(2, j).toLong + 1))))
        println(n)

  }
}
