object Card {

  def validate(cardNum: Long): Boolean = {
    val code = sumDigits(doubleOddPositions(num2digits(cardNum)))
    code % 10 == 0
  }

  def num2digits(num: Long): List[Int] = {
    def _num2digits(num: Long): List[Int] = {
      if (num < 10) List(num.toInt)
      else {
        val d = num / 10
        val r = (num % 10).toInt
        r :: _num2digits(d)
      }
    }
    _num2digits(num).reverse
  }

  def doubleOddPositions(nums: List[Int]): List[Int] = {
    def _even(n: List[Int]): List[Int] = n match {
      case Nil => Nil
      case h :: t => (h * 2) :: _odd(t)
    }
    def _odd(n: List[Int]): List[Int] = n match {
      case Nil => Nil
      case h :: t => h :: _even(t)
    }
    _even(nums)
  }

  def sumDigits(digits: List[Int]): Int = {
    digits.foldLeft(0) {
      (acc, d) => {
        if(d < 10) d
        else sumDigits(num2digits(d))
      } + acc
    }
  }
}
