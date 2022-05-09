def sumOpts(l: List[Option[Double]]): Option[Double] = {
    val result = l.foldLeft (None: Option[Double]) ((akum, x) => (akum,x) match {
      case (None, _) => x
      case (_, None) => akum
      case (Some(a),Some(b)) => Some(a+b)
    })
    result
}

def position[A](l: List[A], el: A): Option[Int] = {
    l.zipWithIndex.find(x => x._1==el).map((a,b) => b)
    //Some(l.indexOf(el))
}

def indices[A](l: List[A], el: A): Set[Int] = {
    l.zipWithIndex.filter(x => x._1==el).map((a,b) => b).toSet
}

def swap[A](l: List[A]): List[A] = {
    Nil//do zrobienia
}

def freq[A](l: List[A]): List[(A, Int)] = {
    l.groupMapReduce(x => x)(_ => 1)(_+_).toList
}

@main
def zadanie_24: Unit = {
  val lista = List(Some(5.4), Some(-2.0), Some(1.0), None, Some(2.6))
  assert( sumOpts(lista) == Some(7.0) )       // ==> OK
  assert( sumOpts(List()) == None)            // ==> OK
  assert( sumOpts(List(None, None)) == None)  // ==> OK
}

@main
def zadanie_25: Unit = {
  val lista = List(2, 1, 1, 5)
  position(lista, 1) // ==> Some(1)
  position(lista, 3) // ==> None
}

@main
def zadanie_26: Unit = {
  val lista = List(1, 2, 1, 1, 5)
  println(indices(lista, 1)) // ==> Set(0, 2, 3).
  println(indices(lista, 7)) // ==> Set()
}

@main
def zadanie_27: Unit = {
  val lista = List(1, 2, 3, 4, 5)
  println(swap(lista)) // ==> List(2, 1, 4, 3, 5) 
}

@main
def zadanie_28: Unit = {
  val strefy: List[String] = java.util.TimeZone.getAvailableIDs.toList
  //println(strefy)
  val europe = strefy.filter(x => x.takeWhile(x => x!='/')=="Europe").map(x => x.stripPrefix("Europe/"))
  //println(europe)
  val sorted_europe = europe.sorted.sortBy(_.length) 
  println(sorted_europe)
}

@main
def zadanie_29: Unit = {
  val lista = List('a','b','a','c','c','a')
  println(freq(lista)) // ==> List(('a', 3),('b', 1),('c', 2))
}