module Lib.TestSuite where
import Lib.Decimal
import Lib.Divisors
import Lib.Lizt
import Lib.Numz
import Lib.Primes

testSuite = testDecimal && testDivisors && testLizt && testNumz && testPrimes 