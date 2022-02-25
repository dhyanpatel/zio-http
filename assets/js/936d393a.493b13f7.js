"use strict";(self.webpackChunkzio_http_docs=self.webpackChunkzio_http_docs||[]).push([[380],{3905:function(e,t,r){r.d(t,{Zo:function(){return l},kt:function(){return m}});var n=r(7294);function o(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function s(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function a(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?s(Object(r),!0).forEach((function(t){o(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):s(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function i(e,t){if(null==e)return{};var r,n,o=function(e,t){if(null==e)return{};var r,n,o={},s=Object.keys(e);for(n=0;n<s.length;n++)r=s[n],t.indexOf(r)>=0||(o[r]=e[r]);return o}(e,t);if(Object.getOwnPropertySymbols){var s=Object.getOwnPropertySymbols(e);for(n=0;n<s.length;n++)r=s[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(o[r]=e[r])}return o}var p=n.createContext({}),c=function(e){var t=n.useContext(p),r=t;return e&&(r="function"==typeof e?e(t):a(a({},t),e)),r},l=function(e){var t=c(e.components);return n.createElement(p.Provider,{value:t},e.children)},u={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},v=n.forwardRef((function(e,t){var r=e.components,o=e.mdxType,s=e.originalType,p=e.parentName,l=i(e,["components","mdxType","originalType","parentName"]),v=c(r),m=o,f=v["".concat(p,".").concat(m)]||v[m]||u[m]||s;return r?n.createElement(f,a(a({ref:t},l),{},{components:r})):n.createElement(f,a({ref:t},l))}));function m(e,t){var r=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var s=r.length,a=new Array(s);a[0]=v;var i={};for(var p in t)hasOwnProperty.call(t,p)&&(i[p]=t[p]);i.originalType=e,i.mdxType="string"==typeof e?e:o,a[1]=i;for(var c=2;c<s;c++)a[c]=r[c];return n.createElement.apply(null,a)}return n.createElement.apply(null,r)}v.displayName="MDXCreateElement"},7244:function(e,t,r){r.r(t),r.d(t,{frontMatter:function(){return i},contentTitle:function(){return p},metadata:function(){return c},toc:function(){return l},default:function(){return v}});var n=r(7462),o=r(3366),s=(r(7294),r(3905)),a=["components"],i={},p="HTTPS Server",c={unversionedId:"v1.x/examples/zio-http-basic-examples/https_server",id:"v1.x/examples/zio-http-basic-examples/https_server",isDocsHomePage:!1,title:"HTTPS Server",description:"",source:"@site/docs/v1.x/examples/zio-http-basic-examples/https_server.md",sourceDirName:"v1.x/examples/zio-http-basic-examples",slug:"/v1.x/examples/zio-http-basic-examples/https_server",permalink:"/zio-http/docs/v1.x/examples/zio-http-basic-examples/https_server",tags:[],version:"current",frontMatter:{},sidebar:"tutorialSidebar",previous:{title:"HTTPS Cient",permalink:"/zio-http/docs/v1.x/examples/zio-http-basic-examples/https_client"},next:{title:"Websocket Server",permalink:"/zio-http/docs/v1.x/examples/zio-http-basic-examples/web-socket"}},l=[],u={toc:l};function v(e){var t=e.components,r=(0,o.Z)(e,a);return(0,s.kt)("wrapper",(0,n.Z)({},u,r,{components:t,mdxType:"MDXLayout"}),(0,s.kt)("h1",{id:"https-server"},"HTTPS Server"),(0,s.kt)("pre",null,(0,s.kt)("code",{parentName:"pre",className:"language-scala"},'import zhttp.http._\nimport zhttp.service.server.ServerChannelFactory\nimport zhttp.service.server.ServerSSLHandler._\nimport zhttp.service.{EventLoopGroup, Server}\nimport zio._\n\nobject HttpsHelloWorld extends App {\n  // Create HTTP route\n  val app: HttpApp[Any, Nothing] = Http.collect[Request] {\n    case Method.GET -> !! / "text" => Response.text("Hello World!")\n    case Method.GET -> !! / "json" => Response.json("""{"greetings": "Hello World!"}""")\n  }\n\n  /**\n   * sslcontext can be created using SslContexBuilder. In this example an inbuilt API using keystore is used. For\n   * testing this example using curl, setup the certificate named "server.crt" from resources for the OS. Alternatively\n   * you can create the keystore and certificate using the following link\n   * https://medium.com/@maanadev/netty-with-https-tls-9bf699e07f01\n   */\n  val sslctx = ctxFromCert(\n    getClass().getClassLoader().getResourceAsStream("server.crt"),\n    getClass().getClassLoader().getResourceAsStream("server.key"),\n  )\n\n  private val server =\n    Server.port(8090) ++ Server.app(app) ++ Server.ssl(\n      ServerSSLOptions(sslctx, SSLHttpBehaviour.Accept),\n    )\n\n  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {\n    server.make.useForever\n      .provideCustomLayer(ServerChannelFactory.auto ++ EventLoopGroup.auto(0))\n      .exitCode\n  }\n}\n')))}v.isMDXComponent=!0}}]);