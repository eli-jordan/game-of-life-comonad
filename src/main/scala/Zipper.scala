import cats._
import cats.implicits._

case class StreamZipper[A](left: Stream[A], focus: A, right: Stream[A]) {

    def moveLeft: StreamZipper[A] =
        if (left.isEmpty) this
        else new StreamZipper[A](left.tail, left.head, focus #:: right)

    def moveRight: StreamZipper[A] =
        if (right.isEmpty) this
        else new StreamZipper[A](focus #:: left, right.head, right.tail)

    private lazy val lefts: Stream[StreamZipper[A]] =
        if (left.isEmpty) Stream.empty
        else Stream.iterate(moveLeft)(_.moveLeft).zip(left.tail).map(_._1)

    private lazy val rights: Stream[StreamZipper[A]] =
        if (right.isEmpty) Stream.empty
        else Stream.iterate(moveRight)(_.moveRight).zip(right.tail).map(_._1)

    def map[B](f: A => B): StreamZipper[B] =
        new StreamZipper[B](left.map(f), f(focus), right.map(f))

    lazy val duplicate: StreamZipper[StreamZipper[A]] =
        new StreamZipper[StreamZipper[A]](lefts, this, rights)

    def extend[B](f: StreamZipper[A] => B): StreamZipper[B] =
        duplicate.map(f)

    def force: StreamZipper[A] =
        new StreamZipper(left.force, focus, right.force)

    def toList: List[A] = left.toList.reverse ++ List(focus) ++ right.toList

    override def toString: String = {
        s"|${left.reverse} >$focus< ${right.force}|"
    }
}

object StreamZipper {
    def apply[A](left: List[A], f: A, right: List[A]): StreamZipper[A] =
        new StreamZipper[A](left.reverse.toStream, f, right.toStream)

    def apply[A](as: List[A]): StreamZipper[A] =
        new StreamZipper[A](Stream.empty, as.head, as.tail.toStream)

    implicit val ListZipperComonad: Comonad[StreamZipper] = new Comonad[StreamZipper] {

        override def extract[A](fa: StreamZipper[A]): A =
            fa.focus

        override def coflatMap[A, B](fa: StreamZipper[A])(f: StreamZipper[A] => B): StreamZipper[B] =
            fa.extend(f)

        override def coflatten[A](fa: StreamZipper[A]): StreamZipper[StreamZipper[A]] =
            fa.duplicate

        override def map[A, B](fa: StreamZipper[A])(f: A => B): StreamZipper[B] =
            fa.map(f)
    }
}

object Zipper extends App {
    def slidingAverage(z: StreamZipper[Int]): StreamZipper[Double] = {
        def avg(a: StreamZipper[Int]): Double = {
            val left = a.moveLeft.focus
            val current = a.focus
            val right = a.moveRight.focus
            (left + current + right) / 3d
        }

        z.coflatMap(avg)
    }

    println(slidingAverage(StreamZipper(List(1, 2, 3), 4, List(5, 6, 7))).toList)
}
