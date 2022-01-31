package module2

import scala.language.implicitConversions

object homework_hkt_impllicts {

    /**
      * 
      * Доработать сигнатуру tupleF и реализовать его
      * По итогу должны быть возможны подобные вызовы
      *   val r1 = println(tupleF(optA, optB))
      *   val r2 = println(tupleF(list1, list2))
      * 
      */
    def tupleF[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)] = {
        for {
            a <- fa
            b <- fb
        } yield {
            (a, b)
        }
    }


    trait Bindable[F[_], A] {
        def map[B](f: A => B): F[B]
        def flatMap[B](f: A => F[B]): F[B]
    }

    object Bindable {

        implicit def optBindable[A](opt: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
            override def map[B](f: A => B): Option[B] = opt.map(f)

            override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
        }

        implicit def listBindable[A](list: List[A]): Bindable[List, A] = new Bindable[List, A] {
            override def map[B](f: A => B): List[B] = list.map(f)

            override def flatMap[B](f: A => List[B]): List[B] = list.flatMap(f)
        }

    }


    def main(args: Array[String]): Unit = {
        val optA: Option[Int] = Some(1)
        val optB: Option[Int] = Some(2)

        val list1 = List(1, 2, 3)
        val list2 = List(4, 5, 6)

        val r1 = println(tupleF(optA, optB))
        val r2 = println(tupleF(list1, list2))
    }
}