package observatory

import java.time.LocalDate

import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

/**
  * 1st milestone: data extraction
  */
object Extraction {

  @transient lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("capstone")
  @transient lazy val sc: SparkContext = new SparkContext(conf)

  val coursera = true
  /** RDD para correr a MAIN */
  val stationsRDD    =  "src/main/resources/stations.csv"
  val temperatureRDD = "src/main/resources/"



  def dir(year: Int,Name1: String, Name2: String): (String,String) ={
    if(coursera) (this.getClass.getResource(Name1).getPath,this.getClass.getResource(Name2).getPath)
    else (stationsRDD  ,temperatureRDD + year +".csv")
  }


  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    *
    *
    */

  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {

    val stations = readStations(sc.textFile(dir(year,stationsFile,temperaturesFile)._1)).filter(x => x.Latitude.isDefined || x.Longitude.isDefined).map {
      x => ((x.STN,x.WBAN),Location(x.Latitude.get,x.Longitude.get))
    }
    val read_temp_values = readYearTemp(sc.textFile(dir(year,stationsFile,temperaturesFile)._2)).filter(x => x.Temperature.isDefined).map {
      x => ((x.STN,x.WBAN),(LocalDate.of(year,x.Month.getOrElse(0),x.Day.getOrElse(0)), x.Temperature.get))
    }
   stations.join(read_temp_values).map{case(key, lista) => (lista._2._1,lista._1,converter(lista._2._2))}.collect()

  }

  def converter(CEL: Double): Double = {((CEL - 32.0)*5.0)/9.0}

  def getyear(arr: Array[String]): Year_read = {

    Year_read(
      STN =  if(arr(0) == "") None else Some(arr(0).toInt),
      WBAN = if(arr(1) == "") None else Some(arr(1).toInt),
      Month = if(arr(2) == "") None else Some(arr(2).toInt),
      Day = if(arr(3) == "") None else Some(arr(3).toInt),
      Temperature = if(arr(4) == "") None else Some(arr(4).toDouble)
    )

  }


  def readYearTemp(lines: RDD[String]): RDD[Year_read] = {
    lines.map(x =>  x.split(","))
         .filter(x => x.size == 5)
         .map(x =>  getyear(x))
  }

  // case class  station (STN: Int, WBAN: Int, Latitude: Double, Longitude: Double)
  def readStations(lines: RDD[String]): RDD[Station] = {

    lines.map(line => {
      val arr = line.split(",")
      val l = arr.length
      Station(
        STN =if (l< 4 || arr(0) == "") None else Some(arr(0).toInt),
        WBAN =if( l< 4 ||arr(1) == "") None else Some(arr(1).toInt),
        Latitude = if( l< 4 ||arr(2) == "") None else Some(arr(2).toDouble),
        Longitude =if( l< 4 ||arr(3) == "") None else Some(arr(3).toDouble)
      )
    })
  }


  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] =
            sc.parallelize(records.toSeq)
            .map{x => (x._2,x._3)}
            .groupByKey()
            .mapValues(x => x.toList)
            .mapValues(x => x.sum/x.size)
            .collect().toSeq
}
