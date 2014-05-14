package bowling

object `package` {
  val digits = Map(
    '0' -> Dijoid(Seq(".-.",
                     "| |",
                     ". .",
                     "| |",
                     ".-.")),
    '1' -> Dijoid(Seq(".  ",
                     "|  ",
                     ".  ",
                     "|  ",
                     ".  ")),
    '8' -> Dijoid(Seq(".-.",
                     "| |",
                     ".-.",
                     "| |",
                     "._.")),
    '5' -> Dijoid(Seq(".-.",
                     "|  ",
                     ".-.",
                     "  |",
                     "._."))
  )

}

object Calculator {
  def format(n: Int): String =
    Monoid.foldMap(n.toString)(digits).lines mkString "\n"
}

case class Dijoid(lines: Seq[String])

object Dijoid {
  implicit val digitMonoid: Monoid[Dijoid] = new Monoid[Dijoid] {
    val empty = Dijoid(Seq.fill(5)(""))
    def append(a: Dijoid, b: Dijoid) =
      if (a == empty)
        b
      else if (b == empty)
        a
      else
        Dijoid(0 to 4 map { line =>
          s"${a.lines(line)} ${b.lines(line)}"
        })
  }
}

trait Monoid[T] {
  def empty: T
  def append(a: T, b: T): T
}

object Monoid {
  def empty[T](implicit m: Monoid[T]) = m.empty

  def append[T](a: T, b: T)(implicit m: Monoid[T]) = m.append(a, b)

  def foldMap[A, B: Monoid](as: Seq[A])(f: A => B): B =
    (Monoid.empty[B] /: (as map f))(Monoid.append[B])

}