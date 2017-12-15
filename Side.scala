package common

import play.api.libs.json._

object Side extends Enumeration {
  type Side = Value
  val SELL, BUY = Value
  @transient implicit val sideEnumFormat = new Format[Side.Side] {
    def reads(json: JsValue) = JsSuccess(Side.withName(json.as[String].toUpperCase))
    def writes(s: Side.Side) = JsString(s.toString)
  }

  def opposite(s : Side) : Side = s match { case Side.BUY => Side.SELL case Side.SELL => Side.BUY }
  def sign(s : Side) : Int = s match { case Side.BUY => 1 case Side.SELL => -1 }
  def of(x : BigDecimal) : Side = if (x > 0) Side.BUY else Side.SELL
}
