package observatory

import java.time.LocalDate
import scala.math._

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {


val pi = 3.14159265359
val alpha = 127
val w = 256
val h = 256

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val aux = pow(2,tile.zoom)
    val long = tile.x/aux*360 - 180
    val lat = atan(sinh(pi-(tile.y/aux)*2*pi))*(180/pi)
    Location(lat, long)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {

    val newx = tile.x*256
    val newy = tile.y*256
    val points = for{
      i <- 0 until h
      j <- 0 until w
    } yield (i,j)

    val pix = points
              .par
              .map(x => tileLocation(Tile(x._2 + newx, x._1 + newy ,tile.zoom + 8)))
              .map(x => Visualization.predictTemperature(temperatures,x))
              .map(x => Visualization.interpolateColor(colors,x))
              .map(x => Pixel(x.red,x.green,x.blue, alpha))
              .toArray
    Image(w,h,pix)
  }



  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for{
      zoom <- 0 to 3
      x <- 0 until  intervals(zoom)
      y <- 0 until  intervals(zoom)
      data <- yearlyData
    } yield generateImage(data._1,Tile(x,y,zoom), data._2)
  }

  def intervals(int: Int): Int ={
    Math.pow(2,int).toInt
  }



}
