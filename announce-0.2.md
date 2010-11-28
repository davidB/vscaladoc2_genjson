
to:post@implicitly.posterous.com, scala-announce@listes.epfl.ch 
Subject : ${artifactId} ${version} ((tag : vscaladoc, scaladoc, api))

<markdown>
Hi,

I'm pleased to announce the [vscaladoc2_genjson] 0.2 release !

VScaladoc2 is the second generation. Second because it **no longer generates static html files**.
Main advantage : Html rendering could be enhance/fix in lot of case without requiring regeneration of scaladoc by lib's creators.

## Modules

VScaladoc2 is currently split in **3** independents (+/-) projects :

* [vscaladoc2_genjson] :
  * generate apidoc in json format
  * run over Scaladoc2 (scala 2.8.0+)
* [vscaladoc2_demoprj] :
  a sample project use to test, demo the json generator and the renderers. fork it, add cases, (push back) ...
* [vscaladoc2_www] :
  * A central place where API of libraries can be referenced and find (Javadoc2/Scaladoc/Scaladoc2/VScaladoc2)
  * A html displayer for API generated in JSON format with [vscaladoc2_genjson]
    * currenlty, it doesn't host api but retreive them from there origin http server
    * it provides a better support for api integration/aggregation (eg multi-modules api, cross lib references,...)
    * it's a work in progress with lot of Bug to fixe, and TODO

You can browse html output of some lib at [vscaladoc2](http://vscaladoc2.alchim31.net/).
  
## Generate

1. create a configuration file (in json)
2. run main with the configuration as argument
3. see result (in json) under
   `$HOME/.config/vscaladoc2/apis/${artifactId}/${version}`

Sample shell script (used for demoprj, scala-library-2.8.0):

 [genjson.sh](https://github.com/davidB/vscaladoc2_demoprj/blob/master/genjson.sh)

For maven user (without pom.xml modification, used for lift-2.2-M1) :

 `mvn org.scala-tools:maven-scala-plugin:2.15.0:genjson`

## Share

share Api generated with [vscaladoc2_genjson] :

* upload $HOME/.config/vscaladoc2/apis/${artifactId}/${version}-apidoc.jar.gz on an http server (eg github download section)
* sign up to [vscaladoc2] + fill the form to add url to your archive

   [vscaladoc2]: http://vscaladoc.alchim31.net
   [vscaladoc2_genjson]: http://github.com/davidB/vscaladoc2_genjson
   [vscaladoc2_demoprj]: http://github.com/davidB/vscaladoc2_demoprj
   [vscaladoc2_www]: http://github.com/davidB/vscaladoc2_www
   
</markdown>
