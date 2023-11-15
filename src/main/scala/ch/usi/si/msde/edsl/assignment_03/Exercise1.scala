package ch.usi.si.msde.edsl.assignment_03

import model.*
import model.HttpRequestModel.*
import model.JsonModel.*
import model.AsyncContext
import scala.concurrent.Future
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

object JsonDSL:
  // Implement the DSL for exercise 1.1 here.


  def json(elements: Map[String, JsonValue]*): JsonObject =  {
    val jsonMap: Map[String, JsonValue] = Map.empty ++ elements.flatten

    JsonObject(values = jsonMap)
  }

  given Conversion[String, JsonString] = (str: String) => JsonString(str)
  given Conversion[Long, JsonNumber] = (num: Long) => JsonNumber(num.toDouble)
  given Conversion[Int, JsonNumber] = (num: Int) => JsonNumber(num.toDouble)
  given Conversion[Float, JsonNumber] = (num: Float) => JsonNumber(num.toDouble)
  given Conversion[Double, JsonNumber] = (num: Double) => JsonNumber(num)
  given Conversion[Short, JsonNumber] = (num: Short) => JsonNumber(num.toDouble)

  given Conversion[Boolean, JsonBoolean] = (bool: Boolean) => if (bool) then
    JsonTrue
    else
    JsonFalse

  given Conversion[Seq[JsonValue], JsonArray] = (seq: Seq[JsonValue]) => JsonArray(seq.toList)

  extension (str: String){
    infix def `:`(value: JsonValue): Map[String, JsonValue] = {
      Map.empty + (str -> value)
    }
  }



end JsonDSL

object HttpRequestDSL:

  // ** Do not touch this **
  import JsonDSL._
  import AsyncContext.{ given, * }

  // ** Implement the DSL here **

end HttpRequestDSL

@main def exercise1_1() = 
  // DO NOT touch this
  import JsonDSL.{ given, * }

  // Uncomment these lines for working examples.

  // Empty object 
   val jsonFragment1 = json()

   println(jsonFragment1)
  
   val jsonFragment2 = json(
     "title" `:` "The Matrix",
     "sequel" `:` false,
     "duration"`:` 126
   )

   println(jsonFragment2)

   val jsonFragment3 = json(
     "title" `:` "The Matrix",
     "director"`:` json(
       "firstName"`:` "Lana",
       "lastName"`:` "Wachowski"
     )
   )
  
   println(jsonFragment3)

   val jsonFragment4 = json(
     "name" `:` "Morpheus",
     "job" `:` "leader",
     "directors" `:` Seq(json(
       "firstName" `:` "Lana",
       "lastName" `:` "Wachowski"
     ),
     json(
       "firstName" `:` "Lilly",
       "lastName" `:` "Wachowski"
     ))
   )

   println(jsonFragment4)

end exercise1_1

@main def exercise1_2() =
  // DO NOT touch this
  import JsonDSL.{ given, * }
  import HttpRequestDSL.{ given, * }
  import AsyncContext.{ given, * }

  // Exercise 1.2
  // Uncomment these lines for working examples.
  
  // val getRequest1: Future[Response] = GET { https("www.google.com") / "search" }
  // val getRequest2 = GET { https("www.usi.ch") / "en" / "university" }
  // val getRequest3 = GET { http("usi.cch") }

  // val postRequest1 = POST { 
  //   https("reqres.in") / "api" / "users"
  // } withEntity json (
  //   "name" `:` "morpheus",
  //   "job" `:` "leader"
  // )

  // val postRequest2 = POST { 
  //   https("reqres.in") / "api" / "register"
  // } withEntity json {
  //   "email" `:` "agent.smith@reqres.in"
  //   "password" `:` "OguhGnivaew"
  // }

  // val postRequest3 = POST { 
  //   https("reqres.in") / "api" / "login"
  // } withEntity json {
  //   "email" `:` "morpheus@nebuchadnezzar"
  // }

  // Do not touch this, just uncomment it.
  // executeInSequence(getRequest1,getRequest2,getRequest3,
  //   postRequest1, postRequest2, postRequest3)
end exercise1_2

// Bonus Exercise (hard)
@main def exercise1_3() =
  // DO NOT touch this
  import JsonDSL.{ given, * }
  import HttpRequestDSL.{ given, * }
  import AsyncContext.{ given, * }

  // val getRequest4 = GET { https("reqres.in") / "api" / "users" ? "page" = "1" & "per_page" = "4" }

  // Do not touch this, just uncomment it.
  // executeInSequence(getRequest4)
end exercise1_3