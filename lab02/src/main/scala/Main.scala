def parzysta(n: Int): Boolean = {
  n%2==0
  /* if (n%2==0){
    "tak"
  } else {
    "nie"
  } */
}

def nwd(a: Int, b: Int): Int = {
  if (b!=0){
    nwd(b, a%b)
  }else{
    if (a<0){
      -a
    }else{
      a
    }
  }
}

def pierwsza(p: Int): Boolean = {
  var pierwsza = true
  for (i <- 1 to p){
    if (i!=0 && i!=1 && i!=p && p%i==0) pierwsza = false
  }
  pierwsza
}

def hipoteza(n: Int): Unit = {
  for (i <- 1 to n){
    for (j <- 1 to n){
      if (pierwsza(i) && pierwsza(j) && i+j==n) println(s"${n} == ${i} + ${j}")
    }
  }
}

@main
def zadanie_02: Unit = {
  println(s"parzysta(6) == ${parzysta(6)}")
  println(s"parzysta(5) == ${parzysta(5)}")
}

@main
def zadanie_03: Unit = {
  println(s"nwd(12,8) = ${nwd(12,8)}")
}

@main
def zadanie_04(liczba: Int): Unit = {
  println(pierwsza(liczba))
}

@main
def zadanie_05(n: Int): Unit = {
  if (parzysta(n) && n>2){
    hipoteza(n)
  }else{
    println("Nie udało się znaleźć takich liczb.")
  }
}