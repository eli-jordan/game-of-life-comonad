import LifeStore.Coord
import cats._
import cats.implicits._

object LifeZipper extends App {

    case class Grid[A](value: StreamZipper[StreamZipper[A]]) {

        def map[B](f: A => B): Grid[B] =
            Grid(value.map(_.map(f)))

        def moveUp: Grid[A] =
            Grid(value.moveLeft)

        def moveDown: Grid[A] =
            Grid(value.moveRight)

        def moveLeft: Grid[A] =
            Grid(value.map(_.moveLeft))

        def moveRight: Grid[A] =
            Grid(value.map(_.moveRight))

        def extract: A =
            value.extract.extract

        def duplicate: Grid[Grid[A]] = {
            // Takes W[W[A]] and adds one additional layer, creating W[W[W[A]]]
            // where W = StreamZipper.
            //
            // Notice that the implementation is very similar to what we had in our original zipper
            // except that we need to make use the 'map' function in the iteration.
            def layer[X](u: StreamZipper[StreamZipper[X]]): StreamZipper[StreamZipper[StreamZipper[X]]] = {
                val lefts = Stream.iterate(u)(ssx => ssx.map(_.moveLeft)).tail.zip(u.left).map(_._1)
                val rights = Stream.iterate(u)(ssx => ssx.map(_.moveRight)).tail.zip(u.right).map(_._1)
                StreamZipper(lefts, u, rights)
            }

            // To make a Grid[Grid[A]] we need
            //    StreamZipperZ[StreamZipper[    StreamZipper[StreamZipper[A]]      ]]
            //                                   |       | inner Grid |      |
            //    |       <---            outer Grid                            -->  |
            // So we apply 'layer' twice to add two layers
            val layers: StreamZipper[StreamZipper[StreamZipper[StreamZipper[A]]]] = layer(layer(value))
            Grid(layers).map(Grid.apply)
        }

        def toLists: List[List[A]] = {
            value.map(_.toList).toList
        }
    }

    implicit val GridComonad: Comonad[Grid] = new Comonad[Grid] {
        override def extract[A](fa: Grid[A]): A =
            fa.extract

        override def coflatMap[A, B](fa: Grid[A])(f: Grid[A] => B): Grid[B] =
            map(fa.duplicate)(f)

        override def map[A, B](fa: Grid[A])(f: A => B): Grid[B] =
            fa.map(f)
    }

    def neighbours[A](grid: Grid[A]): List[A] = List(
        grid.moveUp,
        grid.moveDown,
        grid.moveLeft,
        grid.moveRight,
        grid.moveUp.moveLeft,
        grid.moveUp.moveRight,
        grid.moveDown.moveLeft,
        grid.moveDown.moveRight
    ).map(_.extract)

    def conway(grid: Grid[Boolean]): Boolean = {
        val liveCount = neighbours(grid).count(identity)

        grid.extract match {
            case true if liveCount < 2 => false
            case true if liveCount == 2 || liveCount == 3 => true
            case true if liveCount > 3 => false
            case false if liveCount == 3 => true
            case x => x
        }
    }

    def step(grid: Grid[Boolean]): Grid[Boolean] =
        grid.coflatMap(conway)

    def newGrid[Boolean](values: List[List[Boolean]]): Grid[Boolean] = {
        Grid(fromList(values.map(fromList)))
    }

    def tabulate(fn: ((Int, Int)) => Boolean): Grid[Boolean] = {
        val extent = 20

        val coords: List[(Int, Int)] = (for {
            x <- 0 until extent
            y <- 0 until extent
        } yield (x, y)).toList

        val coordinates = coords.grouped(extent).toList

        newGrid(coordinates.map(_.map(e => fn(e))))
    }

    def fromList[A](items: List[A]): StreamZipper[A] = {
        val left = items.take(items.size)
        val right = items.drop(items.size)
        StreamZipper(left.tail, left.head, right)
    }


    def render(grid: Grid[Boolean]): String = {
        def cellString(value: Boolean): String = if (value) " X " else " . "
        grid.map(cellString).toLists.map(_.mkString).mkString("\n")
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

    val initialState = (glider at(1, 1)) ++ (beacon at(16, 6)) ++ (blinker at(17, 5))

    def initialFn(coord: (Int, Int)): Boolean =
        initialState.getOrElse(coord, false)

    def gameLoop(): Unit = {
        var current = tabulate(initialFn)
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