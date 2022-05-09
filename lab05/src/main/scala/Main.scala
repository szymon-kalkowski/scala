def oczyść[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def ogonoweOczysc[A](l: List[A], akum: List[A] = List()): List[A] = l match {
      case Nil => akum
      case g1::o1 => o1 match { 
        case Nil => akum:+g1
        case g2::o2 if (g1==g2) => ogonoweOczysc(o1, akum)
        case g2::o2 if (g1!=g2) => ogonoweOczysc(o1, akum:+g1)
      }
    }
    ogonoweOczysc(l)
}

def skompresuj[A](l: List[A]): List[(A, Int)] = {
    @annotation.tailrec
    def ogonoweSkompresuj[A](l: List[A], counter: Int = 1, akum: List[(A, Int)] = List()): List[(A, Int)] = l match {
      case Nil => akum
      case g1::o1 => o1 match { 
        case Nil => akum:+(g1, counter)
        case g2::o2 if (g1==g2) => ogonoweSkompresuj(o1, counter+1, akum)
        case g2::o2 if (g1!=g2) => ogonoweSkompresuj(o1, 1, akum:+(g1, counter))
      } 
    }
    ogonoweSkompresuj(l)
}

def isOrdered[A](leq: (A, A) => Boolean)(l: List[A]): Boolean = {
    @annotation.tailrec
    def tailIsOrdered[A](leq: (A, A) => Boolean)(l: List[A]): Boolean = l match {
      case Nil => true
      case g1::o1 => o1 match {
        case Nil => true
        case g2::o2 if (leq(g1,g2)) == false => false
        case g2::o2 if (leq(g1,g2)) == true => tailIsOrdered(leq)(o1)
      } 
    }
    tailIsOrdered(leq)(l)
}

def applyForAll[A, B](l: List[A])(f: A => B): List[B] = {
    @annotation.tailrec
    def tailApplyForAll[A,B](l: List[A], akum: List[B] = Nil)(f: A => B): List[B] = l match {
      case Nil => akum
      case g::o => tailApplyForAll(o, akum:+f(g))(f)
    }
    tailApplyForAll(l)(f)
}

@main
def zadanie_15: Unit = {
    val lista = List(1, 1, 2, 4, 4, 4, 1, 3)
    println(oczyść(lista)) // ==> List(1, 2, 4, 1, 3)
}

@main
def zadanie_16: Unit = {
    val lista = List('a', 'a', 'b', 'c', 'c', 'c', 'a', 'a', 'b', 'd')
    println(skompresuj(lista)) // ==> List(('a', 2), ('b', 1), ('c', 3), ('a', 2), ('b', 1), ('d', 1))
}

@main
def zadanie_17: Unit = {
    val lt = (m: Int, n: Int) => m < n
    val lte = (m: Int, n: Int) => m <= n
    val lista = List(1, 2, 2, 5)
    println(isOrdered(lt)(lista)) // ==> false
    println(isOrdered(lte)(lista)) // ==> true
}

@main
def zadanie_18: Unit = {
    val lista = List(1, 3, 5)
    val f = (n: Int) => n + 3
    println(applyForAll(lista)(f)) // ==> List(4, 6, 8)
}