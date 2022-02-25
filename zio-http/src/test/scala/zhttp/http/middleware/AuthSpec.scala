package zhttp.http.middleware

import zhttp.http._
import zhttp.internal.HttpAppTestExtensions
import zio.UIO
import zio.test.Assertion._
import zio.test._

object AuthSpec extends DefaultRunnableSpec with HttpAppTestExtensions {
  private val basicHS: Headers   = Headers.basicAuthorizationHeader("user", "resu")
  private val basicHF: Headers   = Headers.basicAuthorizationHeader("user", "user")
  private val barerToken: String = "dummyBarerToken"
  private val barerHS: Headers   = Headers.bearerAuthorizationHeader(barerToken)
  private val barerHF: Headers   = Headers.bearerAuthorizationHeader(barerToken + "SomethingElse")

  private val basicAuthM: HttpMiddleware[Any, Nothing]    = Middleware.basicAuth { c =>
    c.uname.reverse == c.upassword
  }
  private val basicAuthZIOM: HttpMiddleware[Any, Nothing] = Middleware.basicAuthZIO { c =>
    UIO(c.uname.reverse == c.upassword)
  }
  private val barerAuthM: HttpMiddleware[Any, Nothing]    = Middleware.barerAuth { c =>
    c == barerToken
  }
  private val barerAuthZIOM: HttpMiddleware[Any, Nothing] = Middleware.barerAuthZIO { c =>
    UIO(c == barerToken)
  }

  def spec = suite("AuthSpec") {
    suite("basicAuth") {
      testM("HttpApp is accepted if the basic authentication succeeds") {
        val app = (Http.ok @@ basicAuthM).status
        assertM(app(Request().addHeaders(basicHS)))(equalTo(Status.OK))
      } +
        testM("Uses forbidden app if the basic authentication fails") {
          val app = (Http.ok @@ basicAuthM).status
          assertM(app(Request().addHeaders(basicHF)))(equalTo(Status.UNAUTHORIZED))
        } +
        testM("Responses should have WWW-Authentication header if Basic Auth failed") {
          val app = Http.ok @@ basicAuthM header "WWW-AUTHENTICATE"
          assertM(app(Request().addHeaders(basicHF)))(isSome)
        }
    } +
      suite("basicAuthZIO") {
        testM("HttpApp is accepted if the basic authentication succeeds") {
          val app = (Http.ok @@ basicAuthZIOM).status
          assertM(app(Request().addHeaders(basicHS)))(equalTo(Status.OK))
        } +
          testM("Uses forbidden app if the basic authentication fails") {
            val app = (Http.ok @@ basicAuthZIOM).status
            assertM(app(Request().addHeaders(basicHF)))(equalTo(Status.UNAUTHORIZED))
          } +
          testM("Responses should have WWW-Authentication header if Basic Auth failed") {
            val app = Http.ok @@ basicAuthZIOM header "WWW-AUTHENTICATE"
            assertM(app(Request().addHeaders(basicHF)))(isSome)
          }
      } +
      suite("barerAuth") {
        testM("HttpApp is accepted if the barer authentication succeeds") {
          val app = (Http.ok @@ barerAuthM).status
          assertM(app(Request().addHeaders(barerHS)))(equalTo(Status.OK))
        } +
          testM("Uses forbidden app if the barer authentication fails") {
            val app = (Http.ok @@ barerAuthM).status
            assertM(app(Request().addHeaders(barerHF)))(equalTo(Status.UNAUTHORIZED))
          } +
          testM("Responses should have WWW-Authentication header if barer Auth failed") {
            val app = Http.ok @@ barerAuthM header "WWW-AUTHENTICATE"
            assertM(app(Request().addHeaders(barerHF)))(isSome)
          }
      } +
      suite("barerAuthZIO") {
        testM("HttpApp is accepted if the barer authentication succeeds") {
          val app = (Http.ok @@ barerAuthZIOM).status
          assertM(app(Request().addHeaders(barerHS)))(equalTo(Status.OK))
        } +
          testM("Uses forbidden app if the barer authentication fails") {
            val app = (Http.ok @@ barerAuthZIOM).status
            assertM(app(Request().addHeaders(barerHF)))(equalTo(Status.UNAUTHORIZED))
          } +
          testM("Responses should have WWW-Authentication header if barer Auth failed") {
            val app = Http.ok @@ barerAuthZIOM header "WWW-AUTHENTICATE"
            assertM(app(Request().addHeaders(barerHF)))(isSome)
          }
      }
  }
}
