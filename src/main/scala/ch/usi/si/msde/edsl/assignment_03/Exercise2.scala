package ch.usi.si.msde.edsl.assignment_03

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import model.HttpRequestModel.*
import model.AssertionModel.{ResponsePredicate, *}
import model.AssertionExecutor

import scala.collection.mutable.Stack
import ch.usi.si.msde.edsl.assignment_03.model.JsonModel.JsonObject
import ch.usi.si.msde.edsl.assignment_03.model.JsonModel.JsonValue

trait RequestAssertionDSL extends AssertionExecutor:

  // Do not touch this import
  import model.AsyncContext._

  // Implement the DSL here.

  // >>>>>>>>>> BONUS
  // this is the generic element for our expression tree element
  sealed abstract class ExpressionPredicateTree

  // a leaf is a value
  case class PredicateLeaf(responsePredicate: ResponsePredicate)
      extends ExpressionPredicateTree

  // a node is an operator
  case class OperatorNode(
      isAnd: Boolean,
      left: Option[ExpressionPredicateTree] = None,
      right: Option[ExpressionPredicateTree] = None
  ) extends ExpressionPredicateTree

  /** Convert the List of infix notation to a list of postfix notation
    *
    * @param infix
    *   The input list of ExpressionPredicateTree elements in infix notation.
    * @return
    *   The output list of ExpressionPredicateTree elements in postfix notation.
    */
  def infixToPostfix(
      infix: List[ExpressionPredicateTree]
  ): List[ExpressionPredicateTree] = {
    val stack: Stack[OperatorNode] = Stack()

    var postfix: List[ExpressionPredicateTree] = List()

    for (currentElement <- infix) {
      currentElement match
        case currentLeaf: PredicateLeaf => {
          postfix = postfix ++ List(currentLeaf)
        }
        case currentOperator: OperatorNode => {
          // Check stack conditions for precedence and associativity
          if (stack.isEmpty || (!stack.top.isAnd && currentOperator.isAnd)) {
            stack.push(currentOperator)
          } else {
            // Pop operators from the stack based on precedence and associativity
            val popped = stack.popWhile(n =>
              (currentOperator.isAnd && n.isAnd) || !currentOperator.isAnd
            )
            stack.push(currentOperator)
            postfix = postfix ++ popped.toList
          }

        }
    }
    val popped = stack.popAll()
    postfix = postfix ++ popped.toList
    postfix
  }

  /** Here we convert the List of postfix notation to our ExpressionTree
    *
    * @param postfix
    *   The input list of ExpressionPredicateTree elements in postfix notation.
    * @return
    *   The constructed ExpressionPredicateTree from the postfix notation.
    */
  def postfixToTree(
      postfix: List[ExpressionPredicateTree]
  ): ExpressionPredicateTree = {
    val nodes: Stack[ExpressionPredicateTree] = Stack()
    for (currentElement <- postfix) {
      currentElement match
        case currentLeaf: PredicateLeaf => {
          nodes.push(currentLeaf)
        }
        case currentOperand: OperatorNode => {
          val right: ExpressionPredicateTree = nodes.pop()
          val left: ExpressionPredicateTree = nodes.pop()
          val newNode: OperatorNode = OperatorNode(
            isAnd = currentOperand.isAnd,
            left = Some(left),
            right = Some(right)
          )
          nodes.push(newNode)
        }
    }
    nodes.pop()
  }

  // if it is an AND operation we go for it otherwise its an OR
  def process(
      isAnd: Boolean,
      left: ResponsePredicate,
      right: ResponsePredicate
  ): ResponsePredicate = {
    if (isAnd) {
      left && right
    } else {
      left || right
    }
  }

  /** Evaluates an ExpressionPredicateTree and returns the corresponding
    * ResponsePredicate.
    *
    * @param root
    *   The root node of the ExpressionPredicateTree to be evaluated.
    * @return
    *   The ResponsePredicate obtained after evaluating the
    *   ExpressionPredicateTree.
    */
  def evaluateTree(root: ExpressionPredicateTree): ResponsePredicate = {

    root match
      case isLeaf: PredicateLeaf => {
        isLeaf.responsePredicate
      }
      case isNode: OperatorNode => {
        val left: ResponsePredicate = evaluateTree(isNode.left.get)
        val right: ResponsePredicate = evaluateTree(isNode.right.get)

        process(isAnd = isNode.isAnd, left = left, right = right)
      }
  }
  // >>>>>>>>>> END BONUS

  val tests = TestHandler()

  case class TestHandler():
    inline def +=(newTest: Test) = {
      val assertionWithDescription: AssertionWithDescription =
        newTest.getAssertionWithDescription
      namedAssertions = namedAssertions ++ List(assertionWithDescription)
    }

  def test(description: String): Test = {
    Test(description = description)
  }

  case class Test(
      description: String,
      response: Option[Future[Response]] = None,
      rePredicate: List[ExpressionPredicateTree] = List()
  ):
    inline def calling(response: Future[Response]): Test = {
      Test(description = description, response = Some(response))
    }

    inline def respondsWith(predicate: ResponsePredicate): Test = {
      Test(
        description = description,
        response = response,
        rePredicate =
          rePredicate ++ List(PredicateLeaf(responsePredicate = predicate))
      )
    }

    inline def and(inputPredicate: ResponsePredicate): Test = {
      val andPredicate: OperatorNode = OperatorNode(isAnd = true)
      Test(
        description = description,
        response = response,
        rePredicate = rePredicate ++ List(
          andPredicate,
          PredicateLeaf(responsePredicate = inputPredicate)
        )
      )
    }

    inline def or(inputPredicate: ResponsePredicate): Test = {
      val orPredicate: OperatorNode = OperatorNode(isAnd = false)
      Test(
        description = description,
        response = response,
        rePredicate = rePredicate ++ List(
          orPredicate,
          PredicateLeaf(responsePredicate = inputPredicate)
        )
      )
    }

    def getAssertionWithDescription: AssertionWithDescription = {

      // here we make and evaluate the tree only if we have more than one element
      val evaluatedPredicate: ResponsePredicate = rePredicate match
        case List(singleElement: ResponsePredicate) => {
          singleElement
        }
        case List(_, _*) => {
          val infix: List[ExpressionPredicateTree] = rePredicate
          val postFix: List[ExpressionPredicateTree] =
            infixToPostfix(infix = infix)
          val predicateRoot: ExpressionPredicateTree =
            postfixToTree(postfix = postFix)
          val evaluatedPredicate: ResponsePredicate =
            evaluateTree(root = predicateRoot)
          evaluatedPredicate
        }
        case _ => {
          throw new RuntimeException("Must have at least one predicate")
        }

      val fun = () => response.get

      val assertion: ExecutableAssertion =
        RequestSucceedsWithResponsePredicateAssertion(
          f = fun,
          responsePredicate = evaluatedPredicate
        )
      AssertionWithDescription(description = description, assertion = assertion)
    }

  sealed trait ResponsePredicateType

  case object statusCode extends ResponsePredicateType:
    def ==(statusCode: Int): ResponsePredicate =
      ResponseHasStatusCodeEqualsToPredicate(statusCode)

  case object contentType extends ResponsePredicateType:
    def ==(contentType: String): ResponsePredicate =
      ResponseHasContentTypeEqualsToPredicate(contentType)

  case object entity extends ResponsePredicateType:
    def ==(entity: JsonObject): ResponsePredicate = ResponseContainsJson(
      entity
    )

end RequestAssertionDSL

object Exercise2_1 extends RequestAssertionDSL with App:

  // Do not touch this
  import JsonDSL.{given, *}
  import HttpRequestDSL.{given, *}
  import model.AsyncContext.{given, *}

  // Exercise 2.1: Basic respond/fail assertions

  tests += test("a get on user 2 will respond with 200 OK") calling {
    GET { https("reqres.in") / "api" / "user" / "3" }
  } respondsWith statusCode == 200 and contentType == "application/json"

  tests += test("a get on a non-existing user will respond with 404") calling {
    GET { https("reqres.in") / "api" / "user" / "-3721" }
  } respondsWith statusCode == 404

  tests += test(
    "a login without password will respond with a json body containing an error"
  ) calling {
    POST {
      https("reqres.in") / "api" / "login"
    } withEntity json(
      "email" `:` "morpheus@nebuchadnezzar"
    )
  } respondsWith contentType == "application/json" and statusCode == 400 and entity == json(
    "error" `:` "Missing password"
  )

  // do ** NOT ** remove this
  run()
end Exercise2_1

object Exercise2_2 extends App with RequestAssertionDSL:

  // Do not touch this
  import JsonDSL._
  import HttpRequestDSL._
  import model.AsyncContext._

  tests += test("a get on a user will respond with 200 or 404") calling {
    GET { https("reqres.in") / "api" / "user" / "3" }
  } respondsWith statusCode == 200 or statusCode == 404

  tests += test(
    "a get on a user will respond with 404 or 200 and contentType application/json"
  ) calling {
    GET { https("reqres.in") / "api" / "user" / "3" }
  } respondsWith statusCode == 404 or statusCode == 200 and contentType == "application/json"

  // do ** NOT ** remove this
  run()
end Exercise2_2
