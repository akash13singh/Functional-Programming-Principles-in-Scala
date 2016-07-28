//sum. takes 2 integers: a & b and a function f. treturns f(a)+f(a+1)+....+f(b)
def sum(f: Int => Int,a: Int,b: Int): Int=
  if(a>b) 0
  else  f(a) + sum(f,a+1,b)

def sumSquares(a: Int, b: Int) = sum(x=>x*x,a,b)
def sumInts(a:Int,b:Int) = sum(x=>x,a,b)

sumSquares(1,4)
sumInts(1,4)

//product take two int (a,b)and a function f. Returns f(a) + f(a+1) .. * f(b)
def product(f: Int=>Int) (a:Int,b:Int): Int=
  if(a > b)  1
  else f(a) * product(f)(a+1,b)

product(x=>x*x)(3,4)

//factorial using product
def factorial(a:Int):Int =
 product(x=>x)(1,a)

factorial(1)

//a function which generlaizes the product and sum based on the cmbine operator and zero value
def mapReduce(f:Int=>Int,combine:(Int,Int)=>Int,zero:Int)(a:Int,b:Int): Int=
  if(a>b) zero
  else combine(f(a),mapReduce(f,combine,zero)(a+1,b))

mapReduce(x=>x,(x,y)=>x*y,1)(3,5)

// define product using map reduce
def product(a:Int,b:Int) = mapReduce(x=>x,(x,y)=>x*y,1)(a,b)

product(3,6)

//absolute function
def abs(a:Double)=
 if(a>=0) a
 else -a

//
val tolerance = 0.0001

def isCloseEnough(x: Double, y: Double) =
 abs((x - y) / x) / x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
 def iterate(guess: Double): Double = {
  val next = f(guess)
  if (isCloseEnough(guess, next)) next
  else iterate(next)
 }
 iterate(firstGuess)
}

def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(1.0)

def averageDamp(f:Double=>Double)(x:Double) = (x+f(x))/2

def sqrt2(x: Double) = fixedPoint(y => (y + x / y) / 2)(1.0)



