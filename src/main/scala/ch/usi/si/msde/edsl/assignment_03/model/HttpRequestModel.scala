package ch.usi.si.msde.edsl.assignment_03.model

import scala.concurrent.Future
import scala.util.Success
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.unmarshalling.*
import akka.http.scaladsl.common.EntityStreamingSupport
import akka.http.scaladsl.common.JsonEntityStreamingSupport
import spray.json.JsValue
import spray.json._
import DefaultJsonProtocol._
import scala.util.Try

/** A simple model for Http requests
  */
object HttpRequestModel:

  // an given value that allows async computations on futures.
  import AsyncContext.ec

  /** A trait modeling a request on a URL.
    */
  trait Request:
    val url: URL

  /** A http get request.
    *
    * @param url
    *   a url.
    */
  case class GetRequest(url: URL) extends Request:
    /** Performs the Get requests.
      *
      * The given instance is available if you do not touch the imports.
      *
      * @param system
      * @return
      *   a future of a response.
      */
    def perform()(using
        system: akka.actor.ClassicActorSystemProvider
    ): Future[Response] =
      Http()
        .singleRequest(HttpRequest(uri = url.toString))
        .flatMap(akkaResponse => {
          val futureBodyString = Unmarshal(akkaResponse.entity).to[String]
          futureBodyString.map { bodyString =>
            Response(akkaResponse, bodyString)
          }
        })
  end GetRequest

  case class PostRequest(url: URL, rawJsonEntity: String) extends Request:
    /** Performs a Post request.
      *
      * The given instance is available if you do not touch the imports.
      *
      * @param system
      * @return
      *   a future of a response.
      */
    def perform()(using
        system: akka.actor.ClassicActorSystemProvider
    ): Future[Response] =
      Http()
        .singleRequest(
          HttpRequest(
            method = HttpMethods.POST,
            uri = url.toString,
            entity = HttpEntity(ContentTypes.`application/json`, rawJsonEntity)
          )
        )
        .flatMap(akkaResponse => {
          val futureBodyString = Unmarshal(akkaResponse.entity).to[String]
          futureBodyString.map { bodyString =>
            println(bodyString)
            Response(akkaResponse, bodyString)
          }
        })

  end PostRequest

  /** A view of a http response, including the status code, the content types,
    * and the headers.
    *
    * @param akkaResponse
    *   the complete http response.
    */
  class Response(private val akkaResponse: HttpResponse, stringBody: String):
    lazy val statusCode = akkaResponse.status.intValue()
    lazy val headers: Seq[(String, String)] = akkaResponse.headers.map {
      header => header.name() -> header.value
    }
    lazy val contentType = akkaResponse.entity.contentType.mediaType.toString()
    lazy val jsonBody: Try[JsValue] = Try { stringBody.parseJson }

    override def toString =
      s"Response with code ${statusCode} and content type ${contentType}"
  end Response

  /** Executes some futures in sequence, and then terminates.
    *
    * @param seq
    *   a sequence of futures.
    */
  def executeInSequence[T](seq: Future[T]*): Unit =
    val transformedSeq = seq map { _.transform(t => Success(t)) }
    Future.sequence(transformedSeq) map { list =>
      list.foreach(println)
    } foreach { _ =>
      AsyncContext.system.terminate()
    }
  end executeInSequence

end HttpRequestModel
