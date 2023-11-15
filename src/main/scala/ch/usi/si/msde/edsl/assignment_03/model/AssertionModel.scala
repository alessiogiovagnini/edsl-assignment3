package ch.usi.si.msde.edsl.assignment_03.model

import scala.concurrent.Future
import ch.usi.si.msde.edsl.assignment_03.model.HttpRequestModel.Response
import scala.util.Success
import scala.util.Failure
import AsyncContext.{ given, * }
import ch.usi.si.msde.edsl.assignment_03.model.JsonModel.*
import spray.json.JsObject.apply

/**
  * A model for assertions on http requests.
  */
object AssertionModel:

  /**
    * An assertion with a description.
    *
    * @param description a description for the assertion.
    * @param assertion the assertion itself.
    */
  case class AssertionWithDescription(description: String, assertion: ExecutableAssertion):
    def run() = assertion.run(description)
  end AssertionWithDescription

  /**
    * An executable assertion.
    */
  trait ExecutableAssertion:
    def run(description: String): Future[AssertionResult]
  end ExecutableAssertion



  /**
    * An assertion result, with a description of what happened during its check.
    */
  sealed trait AssertionResult:
    val message: String
  end AssertionResult

  /**
    * A successful assertion.
    */
  case class AssertionSuccess(message: String) extends AssertionResult

  /**
    * A failed assertion.
    */
  case class AssertionFailure(message: String) extends AssertionResult

  /**
    * An assertion on the failure of a request.
    *
    * @param f a function returning a future of a response.
    */
  class RequestWillFailAssertion(f: () => Future[Response]) extends ExecutableAssertion:
    def run(description: String): Future[AssertionResult] = f().transform: tryResult =>
      Success(tryResult match
        case Success(value) => AssertionFailure(s"${description}: request did not fail (responded with status code ${value.statusCode})")
        case Failure(f) => AssertionSuccess(s"${description}: correctly failed with error ${f.getMessage()}")
      )
  end RequestWillFailAssertion

  /**
   * A predicate on a http response, composed of a function and a description.
   */ 
  trait ResponsePredicate:
    
    val fun: Response => Boolean
    val description: String

    /** 
     * Composes two predicates with a logical and.
     */ 
    def &&(other: ResponsePredicate): ResponsePredicate = AndPredicate(this,other)
    /**
     * Composes two predicates with a logical or.
     */ 
    def ||(other: ResponsePredicate): ResponsePredicate = OrPredicate(this,other)

    
    private case class AndPredicate(a: ResponsePredicate, b: ResponsePredicate) extends ResponsePredicate:
      val fun = (r: Response) => a.fun(r) && b.fun(r)
      val description = s"${a.description} and ${b.description}"
    end AndPredicate

    private case class OrPredicate(a: ResponsePredicate, b: ResponsePredicate) extends ResponsePredicate:
      val fun = (r: Response) => a.fun(r) || b.fun(r)
      val description = s"${a.description} or ${b.description}"
    end OrPredicate
  end ResponsePredicate

  /**
   * A predicate which matches a given statusCode on a response.
   */ 
  case class ResponseHasStatusCodeEqualsToPredicate(statusCode: Int) extends ResponsePredicate:
    val fun = (r: Response) => r.statusCode == statusCode
    val description = s"statusCode == ${statusCode}"
  end ResponseHasStatusCodeEqualsToPredicate

  /**
   * A predicate whicch matches a given content type on a response.
   */ 
  case class ResponseHasContentTypeEqualsToPredicate(contentType: String) extends ResponsePredicate:
    val fun = (r: Response) => r.contentType == contentType
    val description = s"contentType is ${contentType}"
  end ResponseHasContentTypeEqualsToPredicate

  /**
    * An assertion using a predicate on a response.
    *
    * @param f a function returning a future of a response.
    * @param expectedStatusCodePredicate the predicate that the response must satisfy.
    */
  case class RequestSucceedsWithResponsePredicateAssertion(f: () => Future[Response], responsePredicate: ResponsePredicate) extends ExecutableAssertion:
    def run(description: String): Future[AssertionResult] = 
      f().transform: tryResult =>
        Success(tryResult match
          case Success(response) => if(responsePredicate.fun(response)) AssertionSuccess(s"${description}: response matched ${responsePredicate.description}") else AssertionFailure(s"${description}: response did not match ${responsePredicate.description}")
          case Failure(f) => AssertionFailure(s"${description}: ${f}")
        )
    end run
  end RequestSucceedsWithResponsePredicateAssertion  


  extension (sprayJsonObject: spray.json.JsValue)
    def contains(dslJsonObject: JsonObject): Boolean = 
      dslJsonObject.values.forall { (key, value) =>
        sprayJsonObject.isInstanceOf[spray.json.JsObject] &&
        sprayJsonObject.asJsObject.fields.contains(key) &&
          sprayJsonObject.asJsObject.fields(key) === value
      }
    
    def ===(dslJsonValue: JsonValue): Boolean = 
      (sprayJsonObject, dslJsonValue) match
        case (n1: spray.json.JsNull.type, b2: JsonNull.type) => true
        case (b1: spray.json.JsBoolean, b2: JsonBoolean) => b1.value == b2.value
        case (s1: spray.json.JsString, s2: JsonString) => s1.value == s2.value
        case (n1: spray.json.JsNumber, n2: JsonNumber) => n1.value == n2.value
        case (o1: spray.json.JsObject, o2: JsonObject) =>
          o1.fields.keySet == o2.values.keySet &&
          o1.fields.forall { (key, value) => value === o2.values(key) }
        case (o1: spray.json.JsArray, o2: JsonArray) =>
          o1.elements.size == o2.values.size &&
          o1.elements.forall(element => o2.values.exists(element === _))
        case _ => false

      
  /**
   * A predicate which matches the body of response.
   */ 
  case class ResponseContainsJson(referenceJsonObject: JsonObject) extends ResponsePredicate:
    val fun = (r: Response) => {
      r.jsonBody.map { responseJson =>
        responseJson.contains(referenceJsonObject)
      }.getOrElse(false)
    }
    val description = s"entity.json is ${referenceJsonObject}"
  end ResponseContainsJson
end AssertionModel