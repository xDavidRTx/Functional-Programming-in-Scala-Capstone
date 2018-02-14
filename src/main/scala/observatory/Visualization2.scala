package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Interaction.tileLocation

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  val alpha = 127
  val h = 256
  val w = 256

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
        val x = point.x
        val y = point.y
        d00*(1-x)*(1-y) + d10*(1-y)*x + d01*(1-x)*y +d11*x*y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {

          val newx = tile.x*h
          val newy = tile.y*w

          val points = for{
            i <- 0 until h
            j <- 0 until w
          } yield (i,j)




         val pix1 = points.par.map(x =>  tileLocation(Tile(x._2 + newx, x._1 + newy ,tile.zoom + 8)))
                  .map( x => (x.lat.floor.toInt, x.lat.ceil.toInt, x.lon.floor.toInt, x.lon.ceil.toInt, x))


           val pix = pix1.map( x=>
                   bilinearInterpolation(normalize(x._5),     // Convert to CellPoint
                   grid.apply(GridLocation(x._1, x._3)),      // d00 Top-left value
                   grid.apply(GridLocation(x._2,  x._3)),     // d01 Bottom-left value
                   grid.apply(GridLocation(x._1, x._4)),      // d10 Top-right value
                   grid.apply(GridLocation(x._2,  x._4))))    // d11 Bottom-right value
                  .map(x => Visualization.interpolateColor(colors,x))
                  .map(x => Pixel(x.red,x.green,x.blue, alpha))
                  .toArray

          Image(w,h,pix)
  }

  def normalize(location: Location): CellPoint ={

    val lat = location.lat - location.lat.floor
    val lon = location.lon - location.lon.floor

    CellPoint(lon,lat)

  }
}
