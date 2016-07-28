import demo.Hello

var hello = new Hello
print(hello.sayHello("scala"))

def msort[T](xs: List[T])(lt:(T,T)=>Boolean): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
    val (fst, snd) = xs splitAt n
    merge(msort(fst)(lt), msort(snd)(lt))
  }
}

val l = List(11,7,6,2,4)
msort(l)((x,y)=> x<y)
msort(List("deepta","akash","sam","jack"))((s1:String,s2:String)=>s1.compareTo(s2)<0)
msort(List("this","is","the","life"))((s1,s2)=>s1.compareTo(s2)<0)


val nums = List(-1,2,4,-10,9,-14)
val (l1,l2) = nums partition(x=>x>0)
print(l1)
print(l2)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (first,rest) = xs span (s=> s==x)
    first::pack(rest)
  }
}

pack(List("a", "a", "a", "b", "c", "c", "a"))

def encode[T](xs:List[T]): List[(T,Int)] = pack(xs) map (ys=>(ys(0),ys.length))

encode(List("a", "a", "a", "b", "c", "c", "a"))

//code for generating pairs of numbers (i,j) such that i>j
((1 until 10) map (i=> (1 until i)
  map(j=>(i,j)))).flatten

//code for pairs of numbers with sum = prime
((1 until 10) map (i=> (1 until i)
          map(j=>(i,j)))).flatten filter
(pair=> 2 until pair._1+pair._2 forall (k=>(pair._1+pair._2) % k !=0 ))

//code for pair of primes
def isPrime(n:Int):Boolean = (2 until n) forall (k=> n%k != 0)

for {
  i <-  1 until 10
  j <- 1 until i
  if isPrime(i+j)
} yield(i,j)

case class boy(name:String,age:Int)
val b1 = boy("akash",16)
val b2 = boy("2",26)
val b3 = boy("3",36)
val b4 = boy("deepta",16)
val boys = List(b1,b2,b3,b4)
//for expression
for (b <- boys if b.age < 20) yield b.name
boys filter (b=> b.age < 20) map (b=>b)
//using filter
boys filter (b=> b.age < 20) map (b=>b.name)

//vector sum
val xs = Vector(3,4,2)
val ys = Vector(4,2,1)
xs zip ys
(for( (x,y) <- xs zip ys) yield (x*y) ).sum




