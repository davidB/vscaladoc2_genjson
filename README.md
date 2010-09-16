Overview
--------

VScaladoc is a variation of scaladoc : an API generator for project written in Scala. 

VScaladoc2 is the second generation. Second because it **no longer generates static html files**.

VScaladoc2 is currently split in **3** independents (+/-) projects :

* [vscaladoc2_genjson]((http://github.com/davidB/vscaladoc_genjson) :
  * generate apidoc in json format
  * run over Scaladoc2 (scala 2.8.0+)
* [vscaladoc2_demoprj](http://github.com/davidB/vscaladoc_demoprj) :
  a sample project use to test, demo the json generator and the renderers. fork it, add cases, (push back) ...
  source available at 
* [vscaladoc2_www]((http://github.com/davidB/vscaladoc_www) :
  a website to render api : from json format -> more user friendly (html)
  currenlty, it doesn't host api but retreive them from there origin http server
  it provides a better support for api integration/aggregation (eg multi-modules api, cross lib references,...)
  it's a work in progress with lot of Bug to fixe, and TODO
  it'll support "pluggable" renderer (specification to define)
  
Currently, it's not possible to generate/store local html files (see below)

Why splitting ?
---------------
* because it's annoying to rerun scalac (internaly) to acquire data when :
 * you only want to use a different html display
 * when you want to integrate some display extension
* because everybody doesn't like same display for api, vscaladoc2_www will allow to choose a favorite skin (TODO)
  see http://www.scala-lang.org/node/4285
* because dynamic website will allow update (fix/enhance) of skin without requiring every lib provider to re-generate apidoc
* because it will be easier to create a cross library database of TypeName, member, signature (TODO)
* because will allow IDE to request api in a lighter format than current html
* because allow to use display tools/lib without being forced to match the scala-compiler/scala-library version (eg website use lift-json, scalate to renderer api)
* future : it will ease creation and usage of cross lib browser, search,...

What about static html generation ?
------------------------------------

Or why not providing a static json -> html tool ?

Because I want :

* to create a "central" place to browse api,
* to ease aggregration/cross project reference (with dynamic update), it's hard and not evolutive to achieve with static generation
* to allow reader to browse api of severals project with his "preferences" (not available)

A possible future solution could be to provide a standalone viewer (http-webapp) with the abilities :

* to browse a local repository of api-data (json format)
* to sniff/capture remote api

Like this user will be able to browse off-line and to access all the benefit of the central "repository"

One of the target (I forgot to list) is to simplify )

Note : You could create your own tool, (or pay me to make it) ;-)

Call for contribution
---------------------

As it's a full rethink : 

* json format is not stable, nor complete (it's time to request)
* there is lot of missing feature is the website, help me to enhance
* it's an opportunitie for everyone to contribute (provide renderer,...)

License
-------
    
    Copyright (C) 2010 Alchim31 http://alchim31.net/
    
    http://github.com/davidB/vscaladoc2_genjson
    
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at
    
            http://www.apache.org/licenses/LICENSE-2.0
    
    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

