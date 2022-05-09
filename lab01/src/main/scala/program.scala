val napis = """Ala
ma
kota i psa"""

@main
def ćwiczenie_01: Unit = {
    /*var licznik = 1

    for (i <-1 to 10) {
        println(i)
    }

    while (licznik<=10){
        println(licznik)
        licznik+=1
    }

    val napis2 ="* " + napis + " *"
    println(napis2)
    println(s"dlugość napis2 to ${napis2.length}")
    val dlugosc = napis.length
    println("*"*12)
    println(napis)
    println("*"*12)

    val wynik = if (napis2.length > 4) "aqq" else "bueee" 
    */
    var arr = napis.split("\n")
    var i = arr.length
    var max = arr(0).length
    for (n <- 0 to (i-1)){
        if (arr(n).length>max) max=arr(n).length
    }
    println("*"*(max+4))
    for (n <- 0 to (i-1)){
        var l=arr(n).length
        println("* "+arr(n)+" "*(max-l)+" *")
    }
    println("*"*(max+4))
    
}