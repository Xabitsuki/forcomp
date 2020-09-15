type Occurrences = List[(Char, Int)]

def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  def aux(cur: Map[Char, Int], remaining: Map[Char, Int]): Map[Char, Int] = {
    if (remaining.isEmpty) cur
    else {
      val (k, v) = remaining.head
      aux(cur.updated(k, cur(k) - v), remaining.tail)
    }
  }
  aux(x.toMap, y.toMap).toList.filterNot{case (c,n) => (n==0)}.sortBy{case (c,n) => c}
}

val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
val y = List(('r', 1))

subtract(x,y)

