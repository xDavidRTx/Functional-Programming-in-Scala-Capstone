package observatory
import com.sksamuel.scrimage.{Image, Pixel}


/**
  * 2nd milestone: basic visualization
  */
object Visualization {


  val R = 6371
  val pi = Math.PI
  val p = 2
  val w = 360
  val h = 180

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {

    val distances = temperatures.map(x => (x._1,calcDist(x._1, location), x._2))
    val near = distances.filter( x => x._2 <= 1)
    if(near.nonEmpty)  near.minBy(_._2)._3
    else{
      val compute = distances.map(x => (getWi(x._2), getWi(x._2)*x._3)).reduceLeft((x,y) => (x._1+y._1,x._2+y._2))
      compute._2/compute._1
    }
  }


  def getWi(distance: Double): Double = {
    1/Math.pow(distance,p)
  }

  def calcDist(l1: Location, l2: Location): Double = {

    def isAntipode(l1: Location, l2: Location): Boolean = {
      l1.lat == - l2.lat && (l1.lon - l2.lon).abs == 180
    }

    if(l1 == l2) 0

    else if(isAntipode(l1,l2)) pi*R
    else{
      R*Math.acos((Math.sin(l1.lat.toRadians) * Math.sin(l2.lat.toRadians)) + Math.cos(l1.lat.toRadians)*Math.cos(l2.lat.toRadians) * Math.cos(Math.abs(l2.lon.toRadians - l1.lon.toRadians)))
    }
  }

  def getColor(max: (Color, Double), min: (Color, Double), temperature: Temperature) : Color = {

    val r = min._1.red   +   (temperature - min._2)*((max._1.red - min._1.red )/(max._2 - min._2))
    val g = min._1.green + (temperature - min._2)*((max._1.green - min._1.green )/(max._2 - min._2))
    val b = min._1.blue  +  (temperature - min._2)*((max._1.blue - min._1.blue )/(max._2 - min._2))

    Color(( 0 max r.round.toInt) min 255, ( 0 max g.round.toInt ) min 255,  (0 max b.round.toInt) min 255)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */

  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    val p = points.toList.sortBy(_._1)            // Diogo
    val i = p.indexWhere(pair => pair._1 > value) // Diogo
    val equals = points.filter(x => x._1 == value)
    if(equals.nonEmpty) equals.head._2
    else if(value > p.reverse.head._1 ) p.reverse.head._2
    else if (value < p.head._1) p.head._2
    else {
          getColor((p(i-1)._2,p(i-1)._1), (p(i)._2,p(i)._1), value)
        }
  }


  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {

    val fim = (for{
    i <- 0 until h
    j <- 0 until w
    } yield  (i,j))
      .par
      .map( x => normalize(x))
      .map(x => predictTemperature(temperatures,x))
      .map(x => interpolateColor(colors, x))
      .map(x => Pixel(x.red, x.green, x.blue, 255)).toArray

    Image(w, h, fim)


    }


  def normalize(pix: (Int,Int)): Location =  {
      val lat  = -(pix._1 - h/2)
      val long =   pix._2 - w/2
      Location(lat,long)
  }

}

