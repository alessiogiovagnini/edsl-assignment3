package ch.usi.si.msde.edsl.assignment_03

import scala.concurrent.{Future}
import scala.util.{Try, Success, Failure}
import scala.util.Success
import model.HttpRequestModel._
import model.AssertionModel._
import model.AssertionExecutor
import scala.concurrent.Await
import ch.usi.si.msde.edsl.assignment_03.model.JsonModel.JsonObject
import ch.usi.si.msde.edsl.assignment_03.model.JsonModel.JsonValue

trait RequestAssertionDSL extends AssertionExecutor:

  // Do not touch this import
  import model.AsyncContext._

  // Implement the DSL here.


end RequestAssertionDSL

object Exercise2_1 extends RequestAssertionDSL with App:

  // Do not touch this
  import JsonDSL.{ given, * }
  import HttpRequestDSL.{ given, * }
  import model.AsyncContext.{ given, * }

  // Exercise 2.1: Basic respond/fail assertions

  // tests += test("a get on user 2 will respond with 200 OK") calling {
  //   GET { https("reqres.in") / "api" / "user" / "3" }
  // } respondsWith statusCode == 200 and contentType == "application/json"

  // tests += test("a get on a non-existing user will respond with 404") calling {
  //   GET { https("reqres.in") / "api" / "user" / "-3721" }
  // } respondsWith statusCode == 404

  // tests += test("a login without password will respond with a json body containing an error") calling {
  //      POST { 
  //     https("reqres.in") / "api" / "login"
  //   } withEntity json(
  //     "email" `:` "morpheus@nebuchadnezzar"
  //   )
  // } respondsWith contentType == "application/json" and statusCode == 400 and entity == json(
  //   "error" `:` "Missing password"
  // )
 
  // do ** NOT ** remove this
  run()
end Exercise2_1

object Exercise2_2 extends App with RequestAssertionDSL:

  // Do not touch this
  import JsonDSL._
  import HttpRequestDSL._
  import model.AsyncContext._
 
  // tests += test("a get on a user will respond with 200 or 404") calling {
  //   GET { https("reqres.in") / "api" / "user" / "3" }
  // } respondsWith statusCode == 200 or statusCode == 404

  // tests += test("a get on a user will respond with 404 or 200 and contentType application/json") calling {
  //   GET { https("reqres.in") / "api" / "user" / "3" }
  // } respondsWith statusCode == 404 or statusCode == 200 and contentType == "application/json"

  // do ** NOT ** remove this
  run()
end Exercise2_2