package observatory

/**
  * 6th (and last) milestone: user interface polishing
  */
object Interaction2 {


  val refDevCOl = Seq(
    ( 4: Temperature, Color(255,   0,   0)),
    ( 2: Temperature, Color(255, 255,   0)),
    (  0: Temperature, Color(  255, 255, 255)),
    (-2: Temperature, Color(  0,   255, 255)),
    (-7: Temperature, Color(0,   0, 255)))


  val refTempColor= Seq(
    ( 60: Temperature, Color(255, 255, 255)),
    ( 32: Temperature, Color(255,   0,   0)),
    ( 12: Temperature, Color(255, 255,   0)),
    (  0: Temperature, Color(  0, 255, 255)),
    (-15: Temperature, Color(  0,   0, 255)),
    (-27: Temperature, Color(255,   0, 255)),
    (-50: Temperature, Color( 33,   0, 107)),
    (-60: Temperature, Color(  0,   0,   0)))


  /**
    * @return The available layers of the application
    */
  def availableLayers: Seq[Layer] = {

      Seq(
        Layer(LayerName.Temperatures, refTempColor, 1975 to 2015),
        Layer(LayerName.Deviations,refDevCOl, 1975 to 2015))
    }


  /**
    * @param selectedLayer A signal carrying the layer selected by the user
    * @return A signal containing the year bounds corresponding to the selected layer
    */
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range] = {
    Signal(selectedLayer.apply().bounds)
  }

  /**
    * @param selectedLayer The selected layer
    * @param sliderValue The value of the year slider
    * @return The value of the selected year, so that it never goes out of the layer bounds.
    *         If the value of `sliderValue` is out of the `selectedLayer` bounds,
    *         this method should return the closest value that is included
    *         in the `selectedLayer` bounds.
    */
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Year]): Signal[Year] = {

    Signal((selectedLayer.apply().bounds.min max sliderValue.apply()) min selectedLayer.apply().bounds.max)
  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The URL pattern to retrieve tiles
    */
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] = {
    Signal(s"target/${selectedLayer.apply().layerName.id}/${selectedYear.apply()}/{z}/{x}-{y}.png")
  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The caption to show
    */
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] = {

    Signal(s"${selectedLayer.apply().layerName.id.capitalize} (${selectedYear.apply()})")
  }

}

sealed abstract class LayerName(val id: String)
object LayerName {
  case object Temperatures extends LayerName("temperatures")
  case object Deviations extends LayerName("deviations")
}

/**
  * @param layerName Name of the layer
  * @param colorScale Color scale used by the layer
  * @param bounds Minimum and maximum year supported by the layer
  */
case class Layer(layerName: LayerName, colorScale: Seq[(Temperature, Color)], bounds: Range)

