import com.sun.xml.internal.org.jvnet.staxex.NamespaceContextEx.Binding

object polynomials{

  class Poly(val terms: Map[Int,Double]){//a terms field

    /* this definition supermiposes the second polynomial onto the first one.
    def + (other: Poly) = new Poly(terms ++ other.terms)
    */
    /*here we add each exponent term for others and then superimpose. so exponets common or
     present only in the second poly are present in the seconf poly. then the second poly is
     imposed on the first poly giving the result.
     map calls adjust on each key->value pair in the other
    */
    def + (other: Poly) = new Poly(terms ++ ( other.terms map adjust))

    def adjust(term:(Int,Double)): (Int,Double) = {
      val (exp,coeff) = term
      terms get exp match {
        case Some(coeff1) => exp -> (coeff + coeff1) //return a mapping : a map element
        case  None => exp-> coeff
      }
    }

    override def toString =
      (for ((exp,coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
  }

  val p1 =  new Poly(Map(1->2.0,3->4,5->6.2))
  p1.terms(3)
  //p1.terms(2) gives exception
  val p2 =  new Poly(Map(0->3,3->7))
  p1+p2




  class Poly2(terms0: Map[Int,Double]){ //a terms0 parameter
    /*
     uses map with default values and custom constructor with varying number of parameters.
     Scala: Repeated Params
     */
    def this(bindings:(Int,Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0

    def + (other: Poly2) = new Poly2(terms ++ ( other.terms map adjust))

    def adjust(term:(Int,Double)): (Int,Double) = {
      val (exp,coeff) = term
      exp -> (coeff+ terms(exp))
    }

    override def toString =
      (for ((exp,coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
  }

  val p3 =  new Poly2(1->2.0,3->4.0,5->6.2)
  p3.terms(3)
  p3.terms(2) // des not give exception
  val p4 =  new Poly2(0->3.0,3->6.0)
  p3 + p4



  class Poly3(terms0: Map[Int,Double]){

  // using foldLeft

  def this(bindings:(Int,Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0

    def + (other: Poly3) =
      new Poly3((other.terms foldLeft terms)(addTerm))

    def addTerm(terms: Map[Int,Double],term:(Int,Double)):Map[Int,Double]= {
      val (exp, coeff) = term
      terms + (exp-> (coeff + terms(exp)))
    }

    override def toString =
      (for ((exp,coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
  }

  val p5 =  new Poly3(1->2.0,3->4.0,5->6.2)
  val p6 =  new Poly3(0->3.0,3->6.0,5->20.8)
  p5+p6
}


