package scalaCource.homeworks.lecture1.topn

import scala.annotation.tailrec
import scala.util.Random

object TopNSort extends App{

  val n = 10

  val list=List.fill(1_000_000)(Random.between(0, 100_000))
  //println(list.mkString(","))

  val stSort = System.currentTimeMillis()
  val resSort = list.sorted.take(n).mkString(",")
  print(resSort, System.currentTimeMillis() - stSort)

  val stTopN = System.currentTimeMillis()
  val res = topn(list, n).mkString(",")

  print(res, System.currentTimeMillis() - stTopN)

  def print(str:String, time:Long):Unit=
    println(s"res - $str \nTime = $time")



  @tailrec
  def topn(s:Seq[Int], n: Int, acc:Seq[Int]= Seq.empty):Seq[Int]=
    s match {
      case a if a.isEmpty => acc
      case head +:tail => acc match {
        case ac if ac.isEmpty || ac.length<n =>
          topn(tail,n, (head+:ac).sorted)
        case ac  if head <= ac.last =>
        topn(tail, n , (head+:ac.init).sorted)
        case _ =>topn(tail, n , acc)
      }
    }
}


