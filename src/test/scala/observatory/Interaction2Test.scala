package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait Interaction2Test extends FunSuite with Checkers {

  test("yearSelection") {
    var res2200 = Interaction2.yearSelection(Signal(Interaction2.availableLayers(0)), Signal(2200))
    assert(res2200() == 2015)
    var res1200 = Interaction2.yearSelection(Signal(Interaction2.availableLayers(0)), Signal(1200))
    assert(res1200() == 1975)
    var res2015 = Interaction2.yearSelection(Signal(Interaction2.availableLayers(0)), Signal(2015))
    assert(res2200() == 2015)
    var res1975 = Interaction2.yearSelection(Signal(Interaction2.availableLayers(0)), Signal(1975))
    assert(res1975() == 1975)
    var res1980 = Interaction2.yearSelection(Signal(Interaction2.availableLayers(0)), Signal(1980))
    assert(res1980() == 1980)
  }

  test("yearSelection2") {
    var res2200 = Interaction2.yearSelection(Signal(Interaction2.availableLayers(1)), Signal(2200))
    assert(res2200() == 2015)
    var res1200 = Interaction2.yearSelection(Signal(Interaction2.availableLayers(1)), Signal(1200))
    assert(res1200() == 1990)
    var res2015 = Interaction2.yearSelection(Signal(Interaction2.availableLayers(1)), Signal(2015))
    assert(res2200() == 2015)
    var res1975 = Interaction2.yearSelection(Signal(Interaction2.availableLayers(1)), Signal(1990))
    assert(res1975() == 1990)
    var res1999 = Interaction2.yearSelection(Signal(Interaction2.availableLayers(1)), Signal(1999))
    assert(res1999() == 1999)
  }

}
