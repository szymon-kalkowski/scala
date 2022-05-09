def subseq[A](list: List[A], begIdx: Int, endIdx: Int): List[A] = {
    list.take(endIdx+1).drop(begIdx)
}

def pairPosNeg(list: List[Double]): (List[Double], List[Double]) = {
    // (list.filter((n: Double) => n<0.0), list.filter((n:Double) => n>0.0))
    (list.filter((n: Double) => n!=0.0).partition((n:Double) => n<0.0))
}

def deStutter[A](list: List[A]): List[A] = {
    val x = list.foldLeft (List[A]()) ( (acc,el) => acc match {
      case Nil => el::acc
      case g::o if g==el => acc
      case _ => el::acc
    } )
    x.reverse
}

def remElems[A](list: List[A], k: Int): List[A] = {
    list.zipWithIndex.filter((x, y) => x!=k).map{
        (x,y) => x
    }
}

def freqMax[A](list: List[A]): (Set[A],Int) = {
    Nil
}

@main
def zadanie_19: Unit = {
    val lista = List(1, 2, 3, 4, 5, 6, 7, 8)
    assert( subseq(lista, 2, 3) == List(3, 4) ) // ==> OK
    assert( subseq(lista, 2, 1) == Nil )        // ==> OK
    assert( subseq(lista, -1, 10) == lista )    // ==> OK
    //println(subseq(lista,2,3))
}

@main
def zadanie_20: Unit = {
    val lista: List[Double] = List(1, -2, 0, 4, 5, 0, -7, 8)
    assert( pairPosNeg(lista) == ( List(-2, -7), List(1, 4, 5, 8) ) ) // ==> OK
}

@main
def zadanie_21: Unit = {
    val l = List(1, 1, 2, 4, 4, 4, 1, 3)
    assert( deStutter(l) == List(1, 2, 4, 1, 3) ) // ==> OK
}

@main
def zadanie_22: Unit = {
    val l = List(1, 1, 2, 4, 4, 1, 3)
    println( remElems(l, 2) == List(1, 1, 4, 4, 1, 3) ) // ==> true
    println( remElems(l, -1) == l ) // ==> true
    println( remElems(l, 15) == l ) // ==> true
}

@main
def zadanie_23: Unit = {
    val l = List(1, 1, 2, 4, 4, 3, 4, 1, 3)
    assert( freqMax(l) == (Set(1,4), 3) ) // ==> OK
}