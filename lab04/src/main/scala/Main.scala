def sumuj(l: List[Option[Double]]): Option[Double] = {
  @annotation.tailrec
  def ogonowaSumuj(l:List[Option[Double]], akum: Option[Double] = None): Option[Double] = l match {
    case Nil => akum
    case Some(glowa) :: ogon if (glowa > 0) => akum match {
      case None => ogonowaSumuj(ogon, Some(glowa))
      case Some(x) => ogonowaSumuj(ogon, Some(glowa+x))
    }
    case _ :: ogon => ogonowaSumuj(ogon, akum) 
  }
  ogonowaSumuj(l)
}

def maksimum(l1: List[Double], l2: List[Double]): List[Double] = {
  @annotation.tailrec
  def ogonoweMaksimum(l1: List[Double], l2: List[Double], akum: List[Double] = List()): List[Double] = (l1,l2) match {
    case (Nil, Nil) => akum
    case (g1 :: o1, g2 :: o2) if (g1 >= g2) => ogonoweMaksimum(o1,o2,akum:+g1)
    case (g1 :: o1, g2 :: o2) if (g1 < g2) => ogonoweMaksimum(o1,o2,akum:+g2)
    case (l, Nil) => akum:::l
    case (Nil, l) => akum:::l
    case _ => List()
  }
  ogonoweMaksimum(l1,l2)
}

def usun[A](l: List[A], el: A): List[A] = {
    @annotation.tailrec
    def ogonowyUsun[A](l: List[A], el: A, akum: List[A] = List()): List[A] = l match {
      case Nil => akum
      case g :: o if (g!=el) => ogonowyUsun(o, el, akum:+g)
      case g :: o if (g==el) => ogonowyUsun(o, el, akum)
      case _ => List()
    }
    ogonowyUsun(l,el)
}

def divide[A](l: List[A]): (List[A], List[A]) = {
    @annotation.tailrec
    def tailDivide[A](l: List[A], akum: (List[A], List[A]) = (List(),List())): (List[A], List[A]) = l match {
      case Nil => akum
      case g :: o if (akum(0).length==akum(1).length) => tailDivide(o,(akum(0):+g,akum(1)))
      case g :: o if (akum(0).length!=akum(1).length) => tailDivide(o,(akum(0),akum(1):+g))
      case _ => (List(),List())
    }
    tailDivide(l)
}

type Pred[A] = A => Boolean

def and[A](p: Pred[A], q: Pred[A]): Pred[A] = {
    a => p(a) && q(a)
}

def or[A](p: Pred[A], q: Pred[A]): Pred[A] = {
    a => p(a) || q(a)
}

def not[A](p: Pred[A]): Pred[A] = {
    a => !p(a)
}

def imp[A](p: Pred[A], q: Pred[A]): Pred[A] = {
    a => !p(a) || q(a)
}

@main
def zadanie_10: Unit = {
  val lista = List(Some(4.0), Some(-3.0), None, Some(1.0), Some(0.0))
  val wynik = sumuj(lista) // ==> Some(5.0)
  println(wynik)
}

@main
def zadanie_11: Unit = {
    val lista1 = List(2.0, -1.6, 3.2, 5.4, -8.4)
    val lista2 = List(3.3, -3.1, 3.2, -4.1, -0.4, 5.5)
    val wynik = maksimum(lista1, lista2) // ==> List(3.3, -1.6, 3.2, 5.4, -0.4, 5.5)
    println(wynik)
}

@main
def zadanie_12: Unit = {
    val lista = List(2, 1, 4, 1, 3, 3, 1, 2)
    val wynik = usun(lista, 1) // ==> List(2, 4, 3, 3, 2).
    println(wynik)
}

@main
def zadanie_13: Unit = {
    val lista = List(1, 3, 5, 6, 7)
    println(divide(lista)) // ==>  ( List(1, 5, 7), List(3, 6) ).
}

@main
def zadanie_14: Unit = {
    // przykładowe predykaty
    val mniejszaNiż9 = (n: Int) => n < 9
    val większaNiż5 = (n: Int) => n > 5

    val a = and(mniejszaNiż9, większaNiż5)
    val o = or(mniejszaNiż9, większaNiż5)
    val n = not(mniejszaNiż9)
    val i = imp(mniejszaNiż9, większaNiż5)

    println(a(4))
    println(o(4))
    println(n(4))
    println(i(4))
}