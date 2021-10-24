import Chapter2.fib
import org.scalatest.flatspec.AnyFlatSpec

class Chapter2Test extends AnyFlatSpec {

  "Fib(1)" should "equal 1" in {
    assert(fib(1) == 1)
  }

  "Fib(0)" should "equal 0" in {
    assert(fib(0) == 0)
  }

  "Fib(6)" should "equal 8" in {
    assert(fib(6) == 8)
  }
}
