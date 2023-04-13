package u05lab.ex1

import u05lab.ex1.List

// Ex 1. implement the missing methods both with recursion or with using fold, map, flatMap, and filters
// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)
    case _ => None

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def append(list: List[A]): List[A] = this match
    case h :: t => h :: t.append(list)
    case _ => list

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filter(predicate)
    case _ :: t => t.filter(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.map(fun)
    case _ => Nil()

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight[List[B]](Nil())(f(_) append _)

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(z, h))(op)
    case Nil() => z

  def foldRight[B](z: B)(f: (A, B) => B): B = this match
    case h :: t => f(h, t.foldRight(z)(f))
    case _ => z

  def length: Int = foldLeft(0)((l, _) => l + 1)

  def isEmpty: Boolean = this match
    case Nil() => true
    case _ => false

  def reverse(): List[A] = foldLeft[List[A]](Nil())((l, e) => e :: l)

  /** EXERCISES */
  def zipRight: List[(A, Int)] =
    /*  Il problema è che t.length produce i valori desiderati ma al contrario
        al posto di 0 1 2 3
        giustamente produce 3 2 1 0
        e non so come evitarlo
    this match
      case h :: t => (h, t.length) :: t.zipRight
      case _ => Nil()
    */
    var res: List[(A, Int)] = Nil()
    var i = 0
    for el <- this do
      res = res.append(List((el, i)))
      i = i+1
    res

  def partition(pred: A => Boolean): (List[A], List[A]) =
    var trueList: List[A] = Nil()
    var falseList: List[A] = Nil()
    for el <- this do
      if(pred(el))
        trueList = trueList.append(List(el))
      else
        falseList = falseList.append(List(el))
    (trueList, falseList)

  def span(pred: A => Boolean): (List[A], List[A]) =
    var beforeList: List[A] = Nil()
    var afterList: List[A] = Nil()
    var splitFound = false
    for el <- this do
      if(splitFound)
        afterList = afterList.append(List(el))
      else
        if (pred(el))
          beforeList = beforeList.append(List(el))
        else
          splitFound = true
          afterList = afterList.append(List(el))
    (beforeList, afterList)

  /** @throws UnsupportedOperationException if the list is empty */
  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw new UnsupportedOperationException
    case h :: Nil() => h
    case h :: t => op(h, t.reduce(op))

  def takeRight(n: Int): List[A] =
    val reversed = this.reverse()
    var result: List[A] = Nil()
    var idx = 0
    for el <- reversed do
      if (idx < n)
        result = result.append(List(reversed.get(idx).get))
        idx += 1
    result.reverse()

// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

@main def checkBehaviour(): Unit =
  val reference = List(1, 2, 3, 4)
  println(reference.zipRight) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.reduce(_ + _)) // 10
  try Nil.reduce[Int](_ + _)
  catch case ex: Exception => println(ex) // prints exception
  println(List(10).reduce(_ + _)) // 10
  println(reference.takeRight(3)) // List(2, 3, 4)

