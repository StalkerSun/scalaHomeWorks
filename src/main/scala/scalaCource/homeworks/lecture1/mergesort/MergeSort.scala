package scalaCource.homeworks.lecture1.mergesort

import scala.annotation.tailrec
import scala.util.Random

object MergeSort extends App{


  //val test = Vector.fill(Random.between(0,1_000_000))(Random.between(-20, 20))
  val test = Vector.fill(1_000_000)(Random.between(-20, 20))

  println(mergeSort(test).mkString(" "))

  def mergeSort(source:Vector[Int]):Vector[Int]={
    if(source.isEmpty || (source.length == 1)) source
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
      @tailrec
      def merge(left: Vector[Int], right: Vector[Int], acc:Vector[Int] = Vector.empty):Vector[Int]=
        left match {
          case a if a.isEmpty => acc.reverse ++ right
          case l+:leftTail=>right match {
            case b if b.isEmpty => acc.reverse ++ left
            case r +: rightTail=>
              if(l<r) merge(leftTail, right, l +: acc)
              else merge(left, rightTail, r +: acc)
          }
        }

      @tailrec
      def mergeNotEqual(left: Vector[Int], right: Vector[Int], acc:Vector[Int] = Vector.empty):Vector[Int]=
        left match {
          case a if a.isEmpty => if(acc.isEmpty || acc.head!=right.head) acc.reverse ++ right else acc.reverse
          case l+:leftTail=>right match {
            case b if b.isEmpty => if(acc.isEmpty || acc.head!=left.head)  acc.reverse ++ left else acc.reverse
            case r +: rightTail=>
              if(l<r) mergeNotEqual(leftTail, right, if( acc.isEmpty || l != acc.head ) l +: acc else acc)
              else mergeNotEqual(left, rightTail, if( acc.isEmpty || r!=acc.head ) r +: acc else acc)
          }
        }

      val (left, right) = source.splitAt(source.length/2)

      mergeNotEqual(mergeSort(left), mergeSort(right))
    }
  }




}
