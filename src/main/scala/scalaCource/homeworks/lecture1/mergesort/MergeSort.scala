package scalaCource.homeworks.lecture1.mergesort

import scala.annotation.tailrec
import scala.util.Random

object MergeSort extends App{


  val test = Vector.fill(Random.between(0,20))(Random.between(-20, 20))

  println(mergeSort(test).mkString(" "))

  def mergeSort(source:Vector[Int]):Vector[Int]={
    if(source.isEmpty || (source.length == 1))
      source
      else{

      @tailrec
      def go(left:Vector[Int], right:Vector[Int], acc:Vector[Int]=Vector.empty):Vector[Int]={
        (left, right) match {
          case (l, r) if r.isEmpty => acc :++ l
          case (l, r) if l.isEmpty => acc :++ r
          case (l, r)=> {
            val leftHead = l.head
            val leftTail = l.tail
            val rightHead = r.head
            val rightTail = r.tail
            if(leftHead < rightHead) go(leftTail, right, acc :+ leftHead)
            else go(left, rightTail, acc :+ rightHead)
          }
        }
      }
      val (left, right) = source.splitAt(source.length/2)
      go (mergeSort(left), mergeSort(right))
    }
  }


}
