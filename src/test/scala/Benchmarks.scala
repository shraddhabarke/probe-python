import java.io.File

import sygus.Main

object Benchmarks extends App
{
  println(
      "-------------------------------------------\n" +
      "| Probe-Python Synthesizer |\n" +
      "-------------------------------------------\n")
  println("Index Name                 time     Program")
  trace.DebugPrints.setNone()
  val benchmarks = new File("src/test/resources/")
  assert(benchmarks.isDirectory)

  benchmarks.listFiles().filter(_.isDirectory).foreach(
    dir => {
      println("----- -------------------- --------------------------------------")
      dir.listFiles()
        .filter(_.getName.contains("20.examples.json"))
        .filter(!_.getName.contains(".out"))
        .sorted
        .zipWithIndex
        .foreach(benchmark => {
          val file = benchmark._1
          val index = benchmark._2 + 1
          val name: String = file.getName.substring(0,file.getName.indexOf('.'))
          print(f"($index%2d)  [$name%18s] ")

          try {
            Main.pySynthesize(file.getAbsolutePath) match {
              case None => println("Timeout")
              case Some((program: String, time: Int)) => println(f"[${time / 1000.0}%1.3f] $program")
            }
          } catch {
            case e: Throwable => println(e.getMessage)
          }
        })
    })
}
