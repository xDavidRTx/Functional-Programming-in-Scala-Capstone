package observatory

import java.time.LocalDate

import observatory.Extraction._
import org.apache.log4j.{Level, Logger}
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import java.io.File

import com.sksamuel.scrimage.Image
import observatory.Interaction.intervals
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD

import annotation.tailrec
import scala.reflect.ClassTag


object Main extends App {

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  @transient lazy val conf: SparkConf = new SparkConf ().setMaster ("local").setAppName ("observatory")
  @transient lazy val sc: SparkContext = new SparkContext (conf)

  val refTempColor = Seq(
    ( 60: Temperature, Color(0, 0, 0)),
    ( 4: Temperature, Color(255,   0,   0)),
    ( 2: Temperature, Color(255, 255,   0)),
    (  0: Temperature, Color(  255, 255, 255)),
    (-2: Temperature, Color(  0,   255, 255)),
    (-7: Temperature, Color(0,   0, 255)))


  val refDevCOl= Seq(
    ( 60: Temperature, Color(255, 255, 255)),
    ( 32: Temperature, Color(255,   0,   0)),
    ( 12: Temperature, Color(255, 255,   0)),
    (  0: Temperature, Color(  0, 255, 255)),
    (-15: Temperature, Color(  0,   0, 255)),
    (-27: Temperature, Color(255,   0, 255)),
    (-50: Temperature, Color( 33,   0, 107)),
    (-60: Temperature, Color(  0,   0,   0)))

  val start = System.nanoTime()
  val years = (1975 to 2015).par
 // computeWeek3()// Fazer as imagens para utilizar no site
  computeWeek5() // MEDOOOOO!!!!
  val end = System.nanoTime()
  val elapsedSeconds = (end - start) / 1e9d
  val inMin = elapsedSeconds/60
  println("Data Read OK! elapsed time: " + inMin + " Minutes")



 // Visualization.visualize(iterLocTemp, refTempColor).output(new java.io.File("target/year_"+year+".png"))

  //Interaction.tile(iterLocTemp,refTempColor, Tile(0,0,0)).output(new java.io.File(target))
  //Interaction.generateTiles[Iterable[(Location,Temperature)]](data, generateImage)


  def generateImage(year: Year,tile: Tile,data: Iterable[(Location, Temperature)]): Unit ={
    val path = s"target/temperatures/$year/${tile.zoom}/"
    val dir = new File(path).mkdirs()
    val target = path + s"${tile.x}-${tile.y}.png"
    val imagem = Interaction.tile(data, refTempColor, tile)
      imagem.output(new java.io.File(target))
  }

  def computeWeek3(): Unit ={
    for{year <- years
        zoom <- 0 to 3
        x <- 0 until  Interaction.intervals(zoom)
        y <- 0 until  Interaction.intervals(zoom)
    } yield generateImage(year,Tile(x,y,zoom),Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(year, "/stations.csv", "/" + year.toString + ".csv")))

  }

  // Não TESTADOOO!
  def computeWeek5(): Unit ={
    val years = 1975 to 1989
    val normal = years.par.map(year => Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(year, "/stations.csv", "/" + year.toString + ".csv")))
   //val normal =  for{year <- 1975 to 1989} yield Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(year, "/stations.csv", "/" + year.toString + ".csv"))
   println("Done Normals! ")
   val avg = Manipulation.average(normal.toList)
    println("Done AVG! ! ")
    val years2 = 1990 to 2015
    val desviations = years2.map( year => (year,Manipulation.deviation( Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(year, "/stations.csv", "/" + year.toString + ".csv")), avg)))
   //val desviations =  for{year <- 1990 to 2015} yield (year,Manipulation.deviation( Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(year, "/stations.csv", "/" + year.toString + ".csv")), avg))
    println("Done Desviations! ")
    val arguments = for{ dev <- desviations
         zoom <- 0 to 0
         x <- 0 until  Interaction.intervals(zoom)
         y <- 0 until  Interaction.intervals(zoom)}
     yield ((dev._2,refDevCOl,Tile(x,y,zoom)), dev._1,Tile(x,y,zoom))

    println("Done final arguments")

    arguments.foreach(x => generateImageWeek5(Visualization2.visualizeGrid(x._1._1,x._1._2,x._3),x._2,x._3))
  }

  def generateImageWeek5(img: Image, year: Int, tile: Tile): Unit ={
    println("Printing Image")
    val path = s"target/deviations/$year/${tile.zoom}/"
    val dir = new File(path).mkdirs()
    val target = path + s"${tile.x}-${tile.y}.png"
    img.output(new java.io.File(target))
  }

}
