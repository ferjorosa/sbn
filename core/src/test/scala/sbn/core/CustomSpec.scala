package sbn.core

import org.scalatest.{BeforeAndAfterAll, FlatSpec, GivenWhenThen, Matchers}

import scala.util.{Failure, Success, Try}

trait CustomSpec extends FlatSpec with Matchers with BeforeAndAfterAll with GivenWhenThen{

  def shouldFail[T](function: Try[T]): Throwable = {
    function match {
      case Success(result) => throw new AssertionError("should return a Failure")
      case Failure(exception) => exception
    }
  }
}