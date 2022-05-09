def rek(n: Int): Int = {
    @annotation.tailrec
    def ogon(n: Int, akumulator: Int): Int ={
        if (n<=0) akumulator
        else ogon(n-1, n * akumulator)
    }
    ogon(n,1)
}

@main
def rekurencja: Unit = {
    println(rek(10))
}
