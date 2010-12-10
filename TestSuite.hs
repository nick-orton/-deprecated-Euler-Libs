module Euler.TestSuite where
import Euler.Decimal
import Euler.Divisors
import Euler.Lizt
import Euler.Numz
import Euler.Primes

testSuite = testDecimal && testDivisors && testLizt && testNumz && testPrimes 
