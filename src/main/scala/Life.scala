
import cats._
import cats.implicits._

import scala.collection.mutable

case class Store[S, A](lookup: S => A)(val index: S) {

    def peek(s: S): A = lookup(s)

    lazy val counit: A = lookup(index)

    lazy val cojoin: Store[S, Store[S, A]] =
        Store(Store(lookup))(index)

    def map[B](f: A => B): Store[S, B] =
        Store(Store.memoize(lookup.andThen(f)))(index)

    def experiment[F[_] : Functor](fn: S => F[S]): F[A] = {
        fn(index).map(lookup)
    }

    override def toString: String = {
        s"lookup: $lookup index: $index"
    }
}

object Store {

    def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
        override def apply(key: I): O = getOrElseUpdate(key, f(key))
    }

    implicit def StoreComonadInstance[S]: Comonad[Store[S, ?]] = new Comonad[Store[S, ?]] {
        override def extract[A](fa: Store[S, A]): A =
            fa.counit

        override def coflatMap[A, B](fa: Store[S, A])(f: Store[S, A] => B): Store[S, B] =
            map(fa.cojoin)(f)

        override def map[A, B](fa: Store[S, A])(f: A => B): Store[S, B] =
            fa.map(f)
    }
}

object LifeStore extends App {
    type Coord = (Int, Int)
    type Grid[A] = Store[Coord, A]

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
        var current = Store[Coord, Boolean](coord => initialState.getOrElse(coord, false))((0, 0))
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
