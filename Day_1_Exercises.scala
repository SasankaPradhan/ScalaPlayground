package com.techsophy

import scala.annotation.tailrec

object Day_1_Exercise {

  def main(args: Array[String]): Unit = {

//    println(isPrime(100))
//    println(isPrime(13))
//    println(isPrime(1))
//    println(isPrime(0))

//    println(factorialUsingWhileLoop(10))
//    println(factorialUsingWhileLoop(30))

//    println(fibonacci(40))
//    println(fibonacciWithTailRecursion(40, 0, 1))

//    println(isPrimeWithTail(10, 3))
//    println(isPrimeWithTail(13, 3))

  }

  //2. Write a function that takes two boolean inputs and returns XOR of it.
  def xor(a: Boolean, b: Boolean): Boolean = a ^ b

  //3. Write a function that takes an int and prints whether the number is even or odd.
  def evenOrOdd(n: Int): Unit = {
    if ((n % 10) / 2 == 0) {
      println("Number is even.")
    } else {
      println("Number is odd.")
    }
  }

  //4. Write a function that takes an int value and returns true if it’s a prime number, otherwise false.
  def isPrime(n: Int): Boolean = {
    if (n == 0 | n == 1) return false
    for (i <- 2 to math.sqrt(n).toInt + 1) {
      if (n % i == 0) return false
    }
    true
  }

  //5. Write a function that takes an int value and returns factorial of it using while loop.
  def factorialUsingWhileLoop(n: Int): BigInt = {
    var p = n
    var fact: BigInt = 1
    while (true) {
      if (p == 0 | p == 1) return fact
      else {
        fact = fact * p
        p = p - 1
      }
    }
    fact
  }

  //3. Write a function that takes an int and prints whether the number is even or odd.
  //6. Perform (3) using tail recursion.
//  def evenOrOddUsingTailRecursion(n: Int, result: Int): Boolean = {
//    if (n % 2 == 0) true
//    else {
//      evenOrOddUsingTailRecursion((n/2).toInt, )
//    }
//    true
//  }

  //7. Write a function that takes a number N and returns the Nth value of the fibonacci number.
  def fibonacci(n: Int): Int = {
    if (n == 1 | n == 2) 1
    else {
      fibonacci(n-1) + fibonacci(n-2)
    }
  }

  @tailrec
  def fibonacciWithTailRecursion(n: Int, result1: BigInt, result2: BigInt): BigInt = {
    if (n == 0 | n == 1) result2
    else fibonacciWithTailRecursion(n-1, result2, result1 + result2)
  }

  //4. Write a function that takes an int value and returns true if it’s a prime number, otherwise false.
  //8. Perform (4) using tail recursion.
  @tailrec
  def isPrimeWithTail(n: Int, acc: Int): Boolean = {
    if (acc == 1) true
    else n % acc != 0 && isPrimeWithTail(n, acc - 1)
  }


}
