package mf.exercises

import cats.implicits._
import mf.http.Http
import mf.models._

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/** Should fail if any call to Http.getBatch fails
  * Should fail if any BatchResponse.response element fails to parse into a ParsedResponse using the
  * ParsedResponse.parser
  */
class Exercise1(implicit ec: ExecutionContext) {
  def requestBatch(
      requestList: List[Request],
      batchSize: Int
  ): Future[Either[ServiceError, List[ParsedResponse]]] = {
    val batches: immutable.List[BatchRequest] =
      requestList.grouped(batchSize).toList.map(BatchRequest.apply)
    val futures: Future[List[Either[ServiceError, List[ParsedResponse]]]] =
      batches.traverse(getAndParseBatch)
    futures.map(r => r.combineAll)
  }

  private def getAndParseBatch(
      batch: BatchRequest
  ): Future[Either[ServiceError, List[ParsedResponse]]] = {
    Http.getBatch(batch).map(r => parseBatchResponse(r, ParsedResponse.parser))
  }

  private def parseBatchResponse(
      batchResponse: BatchResponse,
      parser: String => Try[ParsedResponse]
  ): Either[ServiceError, List[ParsedResponse]] =
    batchResponse.responses.traverse { raw =>
      parser(raw.value).toEither.leftMap(e => ServiceError(e.getMessage))
    }
}
