examples
========

Extremely random crap, much of which needs revisiting. Buyer beware. Note that there are some example tests in `src/test/scala`.

compiler args
-------------

Suggested compiler args for 2.10 ... for 2.11 need to add `-Xstrict-inference` and maybe more (TBD).

```scala
scalacOptions ++= Seq(
  "-deprecation",           
  "-encoding", "UTF-8",       // yes, this is 2 args
  "-feature",                
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xfatal-warnings",       
  "-Xlint",
  "-Yno-adapted-args",      
  "-Ywarn-all",             
  "-Ywarn-dead-code",        // N.B. doesn't work with the ??? hole
  "-Ywarn-numeric-widen",   
  "-Ywarn-value-discard"   
)
```


random crap
-----------

I know nothing about shell scripting because I want to retain plausible deniability (shell scripting, installers, and crystal reports are the unholy trinity of bullshit you never want to admit you can do). Having said that, here are some very crappy scripts that I just wrote to improve my sanity.

List the packages contained in a jarfile

```
#!/bin/bash
jar tf $1 | grep class$ | perl -pe "s|/[^/]*class$||;s|/|.|g" | sort | uniq
```

Display the manifest

```
#!/bin/bash
unzip -p $1 META-INF/MANIFEST.MF
```

Edit the manifest in-place (note that this creates and destroys `META-INF/MANIFEST.MF` in the local dir ... really need to do this in `/tmp` or something).

```
#!/bin/bash
unzip $1 META-INF/MANIFEST.MF > /dev/null
mate -w META-INF/MANIFEST.MF
jar umf META-INF/MANIFEST.MF $1 2> /dev/null
rm META-INF/MANIFEST.MF
rmdir META-INF
echo "*** UPDATED TO"
unzip -p $1 META-INF/MANIFEST.MF
```



