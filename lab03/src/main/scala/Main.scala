def reverse(str: String): String = {
  val list = str.toList
  @annotation.tailrec
  def tailReverse(l: List[Char], akum: List[Char]): List[Char] = {
    if (l==Nil) akum
    else tailReverse(l.tail, l.head :: akum)
  }
  tailReverse(list,List()).mkString
}

/* def pierwsza(n: Int): Boolean = {
  @annotation.tailrec
  def ogonowaPierwsza(n:Int, akum: Int): Boolean = {
    if (akum==1) true
    else
      if (n%akum==0) false
        else ogonowaPierwsza(n,akum-1)
  }
  if (n>1) ogonowaPierwsza(n,n-1)
  else false
} */

def pierwsza(n: Int): Boolean = {
  @annotation.tailrec
  def ogonowaPierwsza(n:Int, akum: Int): Boolean = {
    if (akum*akum>n) true
    else
      if (n%akum==0) false
        else ogonowaPierwsza(n,akum+1)
  }
  if (n>1) ogonowaPierwsza(n,2)
  else false
}

def ciąg(n: Int): Int = {
  @annotation.tailrec
  def ogonowyCiąg(n: Int, a: Int, b: Int):Int = n match {
    case 0 => a
    case _ => ogonowyCiąg(n-1, b, a+b)
  }
  ogonowyCiąg(n,2,1)
}

@main
def zadanie_06(arg: String): Unit = {
  println(reverse(arg))
}

@main
def zadanie_07(arg: Int): Unit = {
  println(pierwsza(arg))
}

@main
def zadanie_08(arg: Int): Unit = {
  println(ciąg(arg))
}