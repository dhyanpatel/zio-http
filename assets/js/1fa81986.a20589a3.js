"use strict";(self.webpackChunkzio_http_docs=self.webpackChunkzio_http_docs||[]).push([[760],{3905:function(e,t,r){r.d(t,{Zo:function(){return d},kt:function(){return m}});var n=r(7294);function a(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function o(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function i(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?o(Object(r),!0).forEach((function(t){a(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):o(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function l(e,t){if(null==e)return{};var r,n,a=function(e,t){if(null==e)return{};var r,n,a={},o=Object.keys(e);for(n=0;n<o.length;n++)r=o[n],t.indexOf(r)>=0||(a[r]=e[r]);return a}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(n=0;n<o.length;n++)r=o[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(a[r]=e[r])}return a}var p=n.createContext({}),s=function(e){var t=n.useContext(p),r=t;return e&&(r="function"==typeof e?e(t):i(i({},t),e)),r},d=function(e){var t=s(e.components);return n.createElement(p.Provider,{value:t},e.children)},u={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},c=n.forwardRef((function(e,t){var r=e.components,a=e.mdxType,o=e.originalType,p=e.parentName,d=l(e,["components","mdxType","originalType","parentName"]),c=s(r),m=a,v=c["".concat(p,".").concat(m)]||c[m]||u[m]||o;return r?n.createElement(v,i(i({ref:t},d),{},{components:r})):n.createElement(v,i({ref:t},d))}));function m(e,t){var r=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var o=r.length,i=new Array(o);i[0]=c;var l={};for(var p in t)hasOwnProperty.call(t,p)&&(l[p]=t[p]);l.originalType=e,l.mdxType="string"==typeof e?e:a,i[1]=l;for(var s=2;s<o;s++)i[s]=r[s];return n.createElement.apply(null,i)}return n.createElement.apply(null,r)}c.displayName="MDXCreateElement"},7759:function(e,t,r){r.r(t),r.d(t,{frontMatter:function(){return l},contentTitle:function(){return p},metadata:function(){return s},toc:function(){return d},default:function(){return c}});var n=r(7462),a=r(3366),o=(r(7294),r(3905)),i=["components"],l={sidebar_position:"1"},p="Server",s={unversionedId:"v1.x/dsl/server",id:"v1.x/dsl/server",isDocsHomePage:!1,title:"Server",description:"This section describes, ZIO HTTP Server and different configurations you can provide while creating the Server",source:"@site/docs/v1.x/dsl/server.md",sourceDirName:"v1.x/dsl",slug:"/v1.x/dsl/server",permalink:"/zio-http/docs/v1.x/dsl/server",tags:[],version:"current",sidebarPosition:1,frontMatter:{sidebar_position:1},sidebar:"tutorialSidebar",previous:{title:"Getting Started",permalink:"/zio-http/docs/v1.x/getting-started"},next:{title:"Http",permalink:"/zio-http/docs/v1.x/dsl/http"}},d=[{value:"Start a ZIO HTTP Server with default configurations",id:"start-a-zio-http-server-with-default-configurations",children:[],level:2},{value:"Start a ZIO HTTP Server with custom configurations.",id:"start-a-zio-http-server-with-custom-configurations",children:[{value:"Binding Server to a socket address",id:"binding-server-to-a-socket-address",children:[],level:3}],level:2},{value:"Server Configurations",id:"server-configurations",children:[],level:2}],u={toc:d};function c(e){var t=e.components,r=(0,a.Z)(e,i);return(0,o.kt)("wrapper",(0,n.Z)({},u,r,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"server"},"Server"),(0,o.kt)("p",null,"This section describes, ZIO HTTP Server and different configurations you can provide while creating the Server "),(0,o.kt)("h2",{id:"start-a-zio-http-server-with-default-configurations"},"Start a ZIO HTTP Server with default configurations"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},"  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =\n    Server.start(8090, app.silent).exitCode\n")),(0,o.kt)("h2",{id:"start-a-zio-http-server-with-custom-configurations"},"Start a ZIO HTTP Server with custom configurations."),(0,o.kt)("ol",null,(0,o.kt)("li",{parentName:"ol"},"Imports required by the customised server. ",(0,o.kt)("pre",{parentName:"li"},(0,o.kt)("code",{parentName:"pre",className:"language-scala"},"import zhttp.http._\nimport zhttp.service.server.ServerChannelFactory\nimport zhttp.service.{EventLoopGroup, Server}\nimport zio._\nimport scala.util.Try\n"))),(0,o.kt)("li",{parentName:"ol"},"The Server can be built incrementally with a ",(0,o.kt)("inlineCode",{parentName:"li"},"++")," each returning a new Server overriding any default configuration. (More properties are given in the ",(0,o.kt)("a",{parentName:"li",href:"#server-configurations"},"Server Configurations")," section below.)",(0,o.kt)("pre",{parentName:"li"},(0,o.kt)("code",{parentName:"pre",className:"language-scala"},"private val server =\n  Server.port(PORT) ++              // Setup port\n    Server.maxRequestSize(8 * 1024) ++ // handle max request size of 8 KB (default 4 KB)\n    Server.app(fooBar ++ app)       // Setup the Http app\n"))),(0,o.kt)("li",{parentName:"ol"},"And then use ",(0,o.kt)("inlineCode",{parentName:"li"},"Server.make"),' to get a "managed" instance use it to run a server forever',(0,o.kt)("pre",{parentName:"li"},(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {\n  server.make\n    .use(start =>\n      console.putStrLn(s"Server started on port ${start.port}")\n      *> ZIO.never,\n    ).provideCustomLayer(ServerChannelFactory.auto ++ EventLoopGroup.auto(2))\n    .exitCode\n')),(0,o.kt)("strong",{parentName:"li"},"Tip :")," ",(0,o.kt)("inlineCode",{parentName:"li"},"ServerChannelFactory.auto ++ EventLoopGroup.auto(num Threads)")," is supplied as an external dependency to choose netty transport type. One can leave it as ",(0,o.kt)("inlineCode",{parentName:"li"},"auto")," to let the application handle it for you.\nAlso in ",(0,o.kt)("inlineCode",{parentName:"li"},"EventLoopGroup.auto(numThreads)")," you can choose number of threads based on number of available processors. ")),(0,o.kt)("h3",{id:"binding-server-to-a-socket-address"},"Binding Server to a socket address"),(0,o.kt)("p",null,"One can bind server to Inet address in multiple ways, either by providing a port number or "),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"If no port is provided, the default port is 8080"),(0,o.kt)("li",{parentName:"ul"},"If specified port is 0, it will use a dynamically selected port.")),(0,o.kt)("details",null,(0,o.kt)("summary",null,(0,o.kt)("b",null,"A complete example ")),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"Example below shows how the server can be started in forever mode to serve HTTP requests:")),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'import zhttp.http._\nimport zhttp.service._\nimport zhttp.service.server.ServerChannelFactory\nimport zio._\n\nimport scala.util.Try\n\nobject HelloWorldAdvanced extends App {\n  // Set a port\n  private val PORT = 8090\n\n  private val fooBar: HttpApp[Any, Nothing] = Http.collect[Request] {\n    case Method.GET -> !! / "foo" => Response.text("bar")\n    case Method.GET -> !! / "bar" => Response.text("foo")\n  }\n\n  private val app = Http.collectM[Request] {\n    case Method.GET -> !! / "random" => random.nextString(10).map(Response.text)\n    case Method.GET -> !! / "utc"    => clock.currentDateTime.map(s => Response.text(s.toString))\n  }\n\n  private val server =\n    Server.port(PORT) ++              // Setup port\n            Server.paranoidLeakDetection ++ // Paranoid leak detection (affects performance)\n            Server.app(fooBar +++ app)      // Setup the Http app\n\n  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {\n    // Configure thread count using CLI\n    val nThreads: Int = args.headOption.flatMap(x => Try(x.toInt).toOption).getOrElse(0)\n\n    // Create a new server\n    server.make\n            .use(_ =>\n              // Waiting for the server to start\n              console.putStrLn(s"Server started on port $PORT")\n\n                      // Ensures the server doesn\'t die after printing\n                      *> ZIO.never,\n            )\n            .provideCustomLayer(ServerChannelFactory.auto ++ EventLoopGroup.auto(nThreads))\n            .exitCode\n  }\n}\n'))),(0,o.kt)("h2",{id:"server-configurations"},"Server Configurations"),(0,o.kt)("table",null,(0,o.kt)("thead",{parentName:"table"},(0,o.kt)("tr",{parentName:"thead"},(0,o.kt)("th",{parentName:"tr",align:null},(0,o.kt)("strong",{parentName:"th"},"Configuration")),(0,o.kt)("th",{parentName:"tr",align:null},(0,o.kt)("strong",{parentName:"th"},"Purpose and usage")))),(0,o.kt)("tbody",{parentName:"table"},(0,o.kt)("tr",{parentName:"tbody"},(0,o.kt)("td",{parentName:"tr",align:null},(0,o.kt)("inlineCode",{parentName:"td"},"Server.app(httpApp)")),(0,o.kt)("td",{parentName:"tr",align:null},"Mount routes. Refer to complete example above")),(0,o.kt)("tr",{parentName:"tbody"},(0,o.kt)("td",{parentName:"tr",align:null},(0,o.kt)("inlineCode",{parentName:"td"},"Server.maxRequestSize(8 * 1024)")),(0,o.kt)("td",{parentName:"tr",align:null},"handle max request size of 8 KB (default 4 KB)")),(0,o.kt)("tr",{parentName:"tbody"},(0,o.kt)("td",{parentName:"tr",align:null},(0,o.kt)("inlineCode",{parentName:"td"},"Server.port(portNum)")," or ",(0,o.kt)("inlineCode",{parentName:"td"},"Server.bind(portNum)")),(0,o.kt)("td",{parentName:"tr",align:null},"Bind server to the port, refer to examples above")),(0,o.kt)("tr",{parentName:"tbody"},(0,o.kt)("td",{parentName:"tr",align:null},(0,o.kt)("inlineCode",{parentName:"td"},"Server.ssl(sslOptions)")),(0,o.kt)("td",{parentName:"tr",align:null},"Creates a new server with ssl options. ",(0,o.kt)("a",{parentName:"td",href:"https://github.com/dream11/zio-http/blob/main/example/src/main/scala/example/HttpsHelloWorld.scala"},"HttpsHelloWorld"))),(0,o.kt)("tr",{parentName:"tbody"},(0,o.kt)("td",{parentName:"tr",align:null},(0,o.kt)("inlineCode",{parentName:"td"},"Server.acceptContinue")),(0,o.kt)("td",{parentName:"tr",align:null},"Sends a ",(0,o.kt)("a",{parentName:"td",href:"https://www.w3.org/Protocols/rfc2616/rfc2616-sec8.html#sec8.2.3"},"100 CONTINUE"))),(0,o.kt)("tr",{parentName:"tbody"},(0,o.kt)("td",{parentName:"tr",align:null},(0,o.kt)("inlineCode",{parentName:"td"},"Server.disableFlowControl")),(0,o.kt)("td",{parentName:"tr",align:null},"Refer ",(0,o.kt)("a",{parentName:"td",href:"https://netty.io/4.1/api/io/netty/handler/flow/FlowControlHandler.html"},"Netty FlowControlHandler"))),(0,o.kt)("tr",{parentName:"tbody"},(0,o.kt)("td",{parentName:"tr",align:null},(0,o.kt)("inlineCode",{parentName:"td"},"Server.disableLeakDetection")),(0,o.kt)("td",{parentName:"tr",align:null},"Disable any leak detection Refer netty's ",(0,o.kt)("a",{parentName:"td",href:"https://netty.io/4.0/api/io/netty/util/ResourceLeakDetector.Level.html"},"ResourceLeakDetector"))),(0,o.kt)("tr",{parentName:"tbody"},(0,o.kt)("td",{parentName:"tr",align:null},(0,o.kt)("inlineCode",{parentName:"td"},"Server.simpleLeakDetection")),(0,o.kt)("td",{parentName:"tr",align:null},"Simplistic leak detection comes with small over head. Refer netty's ",(0,o.kt)("a",{parentName:"td",href:"https://netty.io/4.0/api/io/netty/util/ResourceLeakDetector.Level.html"},"ResourceLeakDetector"))),(0,o.kt)("tr",{parentName:"tbody"},(0,o.kt)("td",{parentName:"tr",align:null},(0,o.kt)("inlineCode",{parentName:"td"},"Server.paranoidLeakDetection")),(0,o.kt)("td",{parentName:"tr",align:null},"Comes with highest possible overhead (for testing purposes only). Refer netty's ",(0,o.kt)("a",{parentName:"td",href:"https://netty.io/4.0/api/io/netty/util/ResourceLeakDetector.Level.html"},"ResourceLeakDetector"))),(0,o.kt)("tr",{parentName:"tbody"},(0,o.kt)("td",{parentName:"tr",align:null},(0,o.kt)("inlineCode",{parentName:"td"},"Server.consolidateFlush")),(0,o.kt)("td",{parentName:"tr",align:null},"Flushing content is done in batches. Can potentially improve performance.")))))}c.isMDXComponent=!0}}]);