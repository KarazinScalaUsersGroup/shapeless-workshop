package karazinscalausersgroup.workshop

import shapeless._

object ShapelessWorkshop extends App {

  case class Virus()

  trait Injector[A] {
    def injectInto(obj: A): List[Virus]
  }

  object Injector {
    def apply[A](func: A => List[Virus]): Injector[A] =
      new Injector[A] {
        def injectInto(obj: A): List[Virus] = func(obj)
      }
  }

  trait Human
  trait Mouse

  trait DNA[T]
  case class Cell[A](dna: DNA[A])
  case class Body[A](cells: List[Cell[A]])

  implicit val humanDNAInjector: Injector[DNA[Human]] =
    Injector[DNA[Human]](_ => Virus() :: Nil)

  implicit val mouseDNAInjector: Injector[DNA[Mouse]] =
    Injector[DNA[Mouse]](_ => Virus() :: Nil)

  // We don't need human or mouse cell specific injector.
  // It's enough to have human or mouse dna specific injector
  implicit def cellInjector[A](implicit injector: Injector[DNA[A]]): Injector[Cell[A]] =
    Injector[Cell[A]](cell => injector.injectInto(cell.dna))

  // We don't need human or mouse body specific injector.
  // It's enough to have human or mouse dna specific injector
  implicit def bodyInjector[A](implicit injector: Injector[Cell[A]]): Injector[Body[A]] =
    Injector[Body[A]](body => body.cells.flatMap(cell => injector.injectInto(cell)))

  def injectInto[A](obj: A)(implicit injector: Injector[A]) =
    injector.injectInto(obj)

  val humanDNA = new DNA[Human] {}
  val humanCell = new Cell[Human](humanDNA)
  val humanBody = new Body[Human](humanCell :: humanCell :: Nil) {}

  val mouseDNA = new DNA[Mouse] {}
  val mouseCell = new Cell[Mouse](mouseDNA)
  val mouseBody = new Body[Mouse](mouseCell :: mouseCell :: Nil) {}

  println(injectInto(humanDNA))
  println(injectInto(humanCell))
  println(injectInto(humanBody))

  println(injectInto(mouseDNA))
  println(injectInto(mouseCell))
  println(injectInto(mouseBody))

}
