import cats._
import cats.implicits._

import scala.collection.mutable

object RepresentableFunctor extends App {

    trait Indexable[F[_], X] {
        implicit def F: Functor[F]

        def index[A](f: F[A]): X => A
    }

    trait Representable[F[_], X] extends Indexable[F, X] {
        def tabulate[A](f: X => A): F[A]
    }

    object Representable {
        def apply[F[_], X](implicit e: Representable[F, X]): Representable[F, X] = e
    }

    /**
     * The reader functor is the identity representable. i.e. it represents itself
     */
    implicit def readerRepresentable[E]: Representable[E => ?, E] = new Representable[E => ?, E] {
        override implicit def F: Functor[E => ?] = Monad[E => ?]

        override def tabulate[A](f: E => A): E => A = f

        override def index[A](f: E => A): E => A = f
    }

    implicit val streamRepresentable: Representable[Stream, Int] = new Representable[Stream, Int] {
        override implicit def F: Functor[Stream] = Functor[Stream]

        private val inc: Int => Int = _ + 1

        override def tabulate[A](f: Int => A): Stream[A] =
            f(0) #:: tabulate(inc.andThen(f))

        override def index[A](f: Stream[A]): Int => A = idx => {
            f.drop(idx).head
        }
    }

    class Matrix2D[A](value: Vector[Vector[A]], rows: Int, cols: Int)

    def fixedLengthVectorRepresentable(length: Int): Representable[Vector, Int] = new Representable[Vector, Int] {
        override implicit def F: Functor[Vector] = Monad[Vector]

        override def tabulate[A](f: Int => A): Vector[A] = (0 to length).map(f).toVector

        override def index[A](f: Vector[A]): Int => A = f(_)
    }

    case class RepresentableStore[F[_], S, A](fa: F[A])(index: S)(implicit R: Representable[F, S]) {

        def peek(s: S): A =
            R.index(fa)(s)

        lazy val counit: A =
            peek(index)

        lazy val cojoin: RepresentableStore[F, S, RepresentableStore[F, S, A]] =
            RepresentableStore(R.tabulate(RepresentableStore(fa)))(index)

        def map[B](f: A => B): RepresentableStore[F, S, B] =
            RepresentableStore(R.F.map(fa)(f))(index)

        def experiment[G[_]: Functor](fn: S => G[S]): G[A] = {
            fn(index).map(peek)
        }
    }

    def FnStore[S, A](fn: S => A, index: S): RepresentableStore[S => ?, S, A] =
        RepresentableStore[S => ?, S, A](fn)(index)

    implicit def StoreComonadInstance[F[_], S](implicit R: Representable[F, S]): Comonad[RepresentableStore[F, S, ?]] = new Comonad[RepresentableStore[F, S, ?]] {
        override def extract[A](fa: RepresentableStore[F, S, A]): A =
            fa.counit

        override def coflatMap[A, B](fa: RepresentableStore[F, S, A])(f: RepresentableStore[F, S, A] => B): RepresentableStore[F, S, B] =
            map(fa.cojoin)(f)

        override def map[A, B](fa: RepresentableStore[F, S, A])(f: A => B): RepresentableStore[F, S, B] =
            fa.map(f)
    }

    def string(i: Int): String = s" --$i-- "

    val stringNumbers: Stream[String] = Representable[Stream, Int].tabulate(string)

    //    println(stringNumbers.take(10).toList)

    //    val indexed = Representable[Stream, Int].index(stringNumbers)
    //    println(indexed(0))
    //    println(indexed(1))
    //    println(indexed(2))

    def mapFunctor[E]: Functor[Lambda[X => mutable.Map[E, X]]] = new Functor[Lambda[X => mutable.Map[E, X]]] {
        override def map[A, B](fa: mutable.Map[E, A])(f: A => B): mutable.Map[E, B] =
            fa.map { case (k, v) => (k, f(v)) }
    }

    implicit def mapRepresentable[E]: Representable[mutable.Map[E, ?], E] = new Representable[mutable.Map[E, ?], E] {
        override implicit def F: Functor[mutable.Map[E, ?]] = mapFunctor

        override def tabulate[A](f: E => A): mutable.Map[E, A] = new mutable.HashMap[E, A]() {
            override def apply(key: E): A = getOrElseUpdate(key, f(key))
        }

        override def index[A](f: mutable.Map[E, A]): E => A = e => f(e)
    }

    val stringMap = Representable[mutable.Map[Int, ?], Int].tabulate(string)
    println(stringMap(1))
}


object RepresentableLifeStore extends App {
    import RepresentableFunctor._

    type Coord = (Int, Int)
    type Grid[A] = RepresentableStore[Coord => ?, Coord, A]

    def neighbourCoords(x: Int, y: Int): List[Coord] = List(
        (x + 1, y),
        (x - 1, y),
        (x, y + 1),
        (x, y - 1),
        (x + 1, y + 1),
        (x + 1, y - 1),
        (x - 1, y + 1),
        (x - 1, y - 1)
    )

    def conway(grid: Grid[Boolean]): Boolean = {
        val neighbours = grid.experiment[List] { case (x, y) => neighbourCoords(x, y) }
        val liveCount = neighbours.count(identity)

        grid.counit match {
            case true if liveCount < 2 => false
            case true if liveCount == 2 || liveCount == 3 => true
            case true if liveCount > 3 => false
            case false if liveCount == 3 => true
            case x => x
        }
    }

    def step(grid: Grid[Boolean]): Grid[Boolean] =
        grid.coflatMap(conway)

    def render(plane: Grid[Boolean]): String = {
        val extent = 20

        val coords: List[Coord] = (for {
            x <- 0 until extent
            y <- 0 until extent
        } yield (x, y)).toList

        def cellString(value: Boolean): String = if (value) " X " else " . "

        val cells = plane.experiment[List] { _ => coords } map cellString
        cells.grouped(extent).map(_.mkString).mkString("\n")
    }

    val glider = Map(
        (1, 0) -> true,
        (2, 1) -> true,
        (0, 2) -> true,
        (1, 2) -> true,
        (2, 2) -> true,
    )

    val blinker = Map(
        (0, 0) -> true,
        (1, 0) -> true,
        (2, 0) -> true
    )

    val beacon = Map(
        (0, 0) -> true,
        (1, 0) -> true,
        (0, 1) -> true,
        (3, 2) -> true,
        (2, 3) -> true,
        (3, 3) -> true
    )

    implicit class InitOps(pairs: Map[Coord, Boolean]) {
        def at(coord: Coord): Map[Coord, Boolean] = pairs.map {
            case ((x, y), v) => ((x + coord._1, y + coord._2), v)
        }
    }

    val initialState = (glider at(0, 0)) ++ (beacon at(15, 5)) ++ (blinker at(16, 4))

    def gameLoop(): Unit = {
        var current = RepresentableStore[Coord => ?, Coord, Boolean](coord => initialState.getOrElse(coord, false))((0, 0))
        while (true) {
            current = step(current)
            val rendered = render(current)
            println("\033\143") // Clear the terminal
            println(rendered)
            Thread.sleep(300)
        }
    }

    gameLoop()
}

