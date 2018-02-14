package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {

 //var cache = collection.mutable.Map[GridLocation, Temperature]()

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {


    GridLocation => Visualization.predictTemperature(temperatures, GridLocation.toLocation)

    }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    GridLocation => {
      val calc = (for {year <- temperaturess} yield (1,Visualization.predictTemperature(year,GridLocation.toLocation)))
        .reduceLeft((x,y) => (x._1+y._1,x._2+y._2))
      calc._2/calc._1
    }
  }
  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {


    GridLocation => Visualization.predictTemperature(temperatures, GridLocation.toLocation)-normals.apply(GridLocation)
  }
}

