Demos for Kurohashi-Kawahara lab
==================================================

Right now, the only available one is [Juman++](http://nlp.ist.i.kyoto-u.ac.jp/EN/index.php?JUMAN%2B%2B).
You can see the demo in action [on this page](http://tulip.kuee.kyoto-u.ac.jp/demo/jumanpp_lattice?text=%E5%A4%96%E5%9B%BD%E4%BA%BA%E5%8F%82%E6%94%BF%E6%A8%A9%E3%81%AB%E5%AF%BE%E3%81%99%E3%82%8B%E8%80%83%E3%81%88%E6%96%B9%E3%81%AE%E9%81%95%E3%81%84%E3%80%82).

## Compiling

This is a [Play](https://www.playframework.com/) 2.5 / [Scala.js](https://www.scala-js.org/) application.
You need to have [sbt](http://www.scala-sbt.org/) installed.
After cloning, `cd` into the directory and run `sbt` there.

After sbt has finished its initialization (the first one will download lots of files and can take ~1h)
use `run` in the sbt console to launch the demo locally.

Juman++ is configured by `play/conf/application.conf` file.
It is a [HOCON](https://github.com/typesafehub/config/blob/master/HOCON.md) file.
It's scope is `akane.jumanpp` and it has subitems:

* `executable`: defaults to jumanpp
* `resources`: path to jumanpp resources (model). Empty by default.
* `lattice`: size of output lattice. Defaults to 1: only best variant.
* `args`: other arguments as a list `[]`
