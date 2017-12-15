package common

import scala.collection.immutable

object Orderbook {
  final case class OrderResult(baseChange : BigDecimal, counterChange : BigDecimal)

  val empty : Orderbook = Orderbook(
    immutable.TreeMap[BigDecimal,BigDecimal]()(Ordering.BigDecimal.reverse),
    immutable.TreeMap[BigDecimal,BigDecimal]()
  )

  def newFromSeqs(bids : Seq[(BigDecimal,BigDecimal)], asks : Seq[(BigDecimal,BigDecimal)]) : Orderbook = Orderbook(
    bids = empty.bids ++ bids,
    asks = empty.asks ++ asks
  )
}

case class Orderbook private(bids : immutable.TreeMap[BigDecimal,BigDecimal],
                             asks : immutable.TreeMap[BigDecimal,BigDecimal]) {
  import Orderbook._
  def isCrossed : Boolean = bids.nonEmpty && asks.nonEmpty && bids.head._1 >= asks.head._1

  /**
    * Returns midprice (if possible)
    * @return
    */
  def midPrice : Option[BigDecimal] =
    if (bids.nonEmpty && asks.nonEmpty) Some((asks.head._1 + bids.head._1)/2)
    else None

  /**
    * Return the result of a limit order for given amount,price and side.
    * @param baseAmount Size of limit order
    * @param counterPrice Price of limit order
    * @param side Side of limit order
    * @return
    */
  def orderResult(baseAmount : BigDecimal, counterPrice : BigDecimal, side : Side.Side) : OrderResult = side match {
    case Side.BUY =>
      var baseBought = BigDecimal(0)
      var counterSold = BigDecimal(0)
      for (price <- asks.keys.takeWhile(_ <= counterPrice)) {
        if (baseBought < baseAmount) {
          val amt = (baseAmount - baseBought) min asks(price)
          baseBought += amt
          counterSold += amt * price
        }
      }
      OrderResult(baseBought, -counterSold)
    case Side.SELL =>
      var baseSold = BigDecimal(0)
      var counterBought = BigDecimal(0)
      for (price <- bids.keys.takeWhile(_ >= counterPrice)) {
        if (baseSold < baseAmount) {
          val amt = (baseAmount - baseSold) min bids(price)
          baseSold += amt
          counterBought += amt * price
        }
      }
      OrderResult(-baseSold, counterBought)
  }

  /**
    * Returns how much liquidity is in the book up to a given price.
    * @param counterPrice Price in counter currency
    * @param side Side of the limit order (buy or sell)
    * @return
    */
  def getLiquidity(counterPrice:BigDecimal, side:Side.Side): BigDecimal = {
    //TODO : better way?
    val INFINITY = BigDecimal("1000000000000042")
    orderResult(INFINITY, counterPrice, side).baseChange.abs
  }

  /**
    * If a market order is placed for a given baseAmount, what is the average price of execution
    * @param baseAmount size of the market order (base currency)
    * @param side side of market order (buy or sell)
    * @return
    */
  def avgExecPrice(baseAmount:BigDecimal, side:Side.Side) : Double = {
    //TODO : better way?
    val INFINITY = BigDecimal("1000000000000042")
    val r = orderResult(baseAmount, side match {case Side.SELL => BigDecimal(0.00) case Side.BUY => INFINITY}, side)
    r.counterChange.toDouble.abs / r.baseChange.toDouble.abs
  }

  override def toString = "<orderbook (" + bids.headOption.map(t => s"${t._1} x ${t._2}").getOrElse(".") + " ~ " + asks.headOption.map(t => s"${t._1} x ${t._2}").getOrElse(".")+ ") (" + bids.keys.size + " bids, " + asks.keys.size + " asks) >"
}
