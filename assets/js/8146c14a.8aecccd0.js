"use strict";(self.webpackChunkzio_http_docs=self.webpackChunkzio_http_docs||[]).push([[698],{3905:function(e,t,n){n.d(t,{Zo:function(){return d},kt:function(){return m}});var a=n(7294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function s(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function o(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?s(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):s(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function p(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},s=Object.keys(e);for(a=0;a<s.length;a++)n=s[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var s=Object.getOwnPropertySymbols(e);for(a=0;a<s.length;a++)n=s[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var l=a.createContext({}),i=function(e){var t=a.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):o(o({},t),e)),n},d=function(e){var t=i(e.components);return a.createElement(l.Provider,{value:t},e.children)},c={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},u=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,s=e.originalType,l=e.parentName,d=p(e,["components","mdxType","originalType","parentName"]),u=i(n),m=r,k=u["".concat(l,".").concat(m)]||u[m]||c[m]||s;return n?a.createElement(k,o(o({ref:t},d),{},{components:n})):a.createElement(k,o({ref:t},d))}));function m(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var s=n.length,o=new Array(s);o[0]=u;var p={};for(var l in t)hasOwnProperty.call(t,l)&&(p[l]=t[l]);p.originalType=e,p.mdxType="string"==typeof e?e:r,o[1]=p;for(var i=2;i<s;i++)o[i]=n[i];return a.createElement.apply(null,o)}return a.createElement.apply(null,n)}u.displayName="MDXCreateElement"},607:function(e,t,n){n.r(t),n.d(t,{frontMatter:function(){return p},contentTitle:function(){return l},metadata:function(){return i},toc:function(){return d},default:function(){return u}});var a=n(7462),r=n(3366),s=(n(7294),n(3905)),o=["components"],p={sidebar_position:"4"},l="Response",i={unversionedId:"v1.x/dsl/response",id:"v1.x/dsl/response",isDocsHomePage:!1,title:"Response",description:"ZIO HTTP Response is designed to encode HTTP Response.",source:"@site/docs/v1.x/dsl/response.md",sourceDirName:"v1.x/dsl",slug:"/v1.x/dsl/response",permalink:"/zio-http/docs/v1.x/dsl/response",tags:[],version:"current",sidebarPosition:4,frontMatter:{sidebar_position:4},sidebar:"tutorialSidebar",previous:{title:"Request",permalink:"/zio-http/docs/v1.x/dsl/request"},next:{title:"HttpData",permalink:"/zio-http/docs/v1.x/dsl/httpdata"}},d=[{value:"Creating a Response",id:"creating-a-response",children:[{value:"Empty Response",id:"empty-response",children:[],level:3},{value:"Specialized Response Constructors",id:"specialized-response-constructors",children:[],level:3},{value:"Specialized Response Operators",id:"specialized-response-operators",children:[],level:3},{value:"Response from HttpError",id:"response-from-httperror",children:[],level:3}],level:2},{value:"Adding Cookie to Response",id:"adding-cookie-to-response",children:[],level:2}],c={toc:d};function u(e){var t=e.components,n=(0,r.Z)(e,o);return(0,s.kt)("wrapper",(0,a.Z)({},c,n,{components:t,mdxType:"MDXLayout"}),(0,s.kt)("h1",{id:"response"},"Response"),(0,s.kt)("p",null,(0,s.kt)("strong",{parentName:"p"},"ZIO HTTP")," ",(0,s.kt)("inlineCode",{parentName:"p"},"Response")," is designed to encode HTTP Response.\nIt supports all HTTP status codes and headers along with custom methods and headers (as defined in ",(0,s.kt)("a",{parentName:"p",href:"https://datatracker.ietf.org/doc/html/rfc2616"},"RFC2616")," )"),(0,s.kt)("h2",{id:"creating-a-response"},"Creating a Response"),(0,s.kt)("p",null,(0,s.kt)("inlineCode",{parentName:"p"},"Response")," can be created with ",(0,s.kt)("inlineCode",{parentName:"p"},"status"),", ",(0,s.kt)("inlineCode",{parentName:"p"},"headers")," and ",(0,s.kt)("inlineCode",{parentName:"p"},"data"),".  "),(0,s.kt)("p",null,"The below snippet creates a response with default params, ",(0,s.kt)("inlineCode",{parentName:"p"},"status")," as ",(0,s.kt)("inlineCode",{parentName:"p"},"Status.OK"),", ",(0,s.kt)("inlineCode",{parentName:"p"},"headers")," as ",(0,s.kt)("inlineCode",{parentName:"p"},"Headers.empty")," and ",(0,s.kt)("inlineCode",{parentName:"p"},"data")," as ",(0,s.kt)("inlineCode",{parentName:"p"},"HttpData.Empty"),"."),(0,s.kt)("pre",null,(0,s.kt)("code",{parentName:"pre",className:"language-scala"}," val res: Response = Response()\n")),(0,s.kt)("h3",{id:"empty-response"},"Empty Response"),(0,s.kt)("ul",null,(0,s.kt)("li",{parentName:"ul"},(0,s.kt)("inlineCode",{parentName:"li"},"ok")," creates an empty response with status code 200")),(0,s.kt)("pre",null,(0,s.kt)("code",{parentName:"pre",className:"language-scala"}," val res: Response = Response.ok\n")),(0,s.kt)("ul",null,(0,s.kt)("li",{parentName:"ul"},(0,s.kt)("inlineCode",{parentName:"li"},"status")," creates an empty response with provided status code.")),(0,s.kt)("pre",null,(0,s.kt)("code",{parentName:"pre",className:"language-scala"}," val res: Response = Response.status(Status.CONTINUE)\n")),(0,s.kt)("h3",{id:"specialized-response-constructors"},"Specialized Response Constructors"),(0,s.kt)("ul",null,(0,s.kt)("li",{parentName:"ul"},(0,s.kt)("inlineCode",{parentName:"li"},"text")," creates a response with data as text, content-type header set to text/plain and status code 200 ")),(0,s.kt)("pre",null,(0,s.kt)("code",{parentName:"pre",className:"language-scala"},' val res: Response = Response.text("hey")\n')),(0,s.kt)("ul",null,(0,s.kt)("li",{parentName:"ul"},(0,s.kt)("inlineCode",{parentName:"li"},"json")," creates a response with data as json, content-type header set to application/json and status code 200 ")),(0,s.kt)("pre",null,(0,s.kt)("code",{parentName:"pre",className:"language-scala"},' val res: Response = Response.json("""{"greetings": "Hello World!"}""")\n')),(0,s.kt)("ul",null,(0,s.kt)("li",{parentName:"ul"},(0,s.kt)("inlineCode",{parentName:"li"},"html")," creates a response with data as html, content-type header set to text/html and status code 200")),(0,s.kt)("pre",null,(0,s.kt)("code",{parentName:"pre",className:"language-scala"},' val res: Response = Response.html(Html.fromString("html text"))\n')),(0,s.kt)("h3",{id:"specialized-response-operators"},"Specialized Response Operators"),(0,s.kt)("ul",null,(0,s.kt)("li",{parentName:"ul"},(0,s.kt)("inlineCode",{parentName:"li"},"setStatus")," to update the ",(0,s.kt)("inlineCode",{parentName:"li"},"status")," of ",(0,s.kt)("inlineCode",{parentName:"li"},"Response"))),(0,s.kt)("pre",null,(0,s.kt)("code",{parentName:"pre",className:"language-scala"},'val res: Response = Response.text("Hello World!").setStatus(Status.NOT_FOUND)\n')),(0,s.kt)("ul",null,(0,s.kt)("li",{parentName:"ul"},(0,s.kt)("inlineCode",{parentName:"li"},"updateHeaders")," to update the ",(0,s.kt)("inlineCode",{parentName:"li"},"headers")," of ",(0,s.kt)("inlineCode",{parentName:"li"},"Response"))),(0,s.kt)("pre",null,(0,s.kt)("code",{parentName:"pre",className:"language-scala"},' val res: Response = Response.ok.updateHeaders(_ => Headers("key", "value"))\n')),(0,s.kt)("h3",{id:"response-from-httperror"},"Response from HttpError"),(0,s.kt)("p",null,(0,s.kt)("inlineCode",{parentName:"p"},"fromHttpError")," creates a response with provided ",(0,s.kt)("inlineCode",{parentName:"p"},"HttpError")),(0,s.kt)("pre",null,(0,s.kt)("code",{parentName:"pre",className:"language-scala"}," val res: Response = Response.fromHttpError(HttpError.BadRequest())\n")),(0,s.kt)("h2",{id:"adding-cookie-to-response"},"Adding Cookie to Response"),(0,s.kt)("p",null,(0,s.kt)("inlineCode",{parentName:"p"},"addCookie")," adds cookies in the headers of the response."),(0,s.kt)("pre",null,(0,s.kt)("code",{parentName:"pre",className:"language-scala"},' val cookie = Cookie("key", "value")\n val res = Response.ok.addCookie(cookie)\n')))}u.isMDXComponent=!0}}]);