package ch09

import ch09._StateMonad_StocksApp.State

/**
 * Source:
 *    - Desc: http://rcardin.github.io/design/programming/fp/monad/2018/11/22/and-monads-for-all-state-monad.html
 *    - github: https://github.com/rcardin/state-monad-example/blob/master/src/main/scala/org/rcardin/monad/state/StocksApp.scala
 */
object _StateMonad_StocksApp {

  val Prices: Map[String, Double] = Map("AMZN" -> 1631.17, "GOOG" -> 1036.05, "TSLA" -> 346.00)

  // Definition of the State Monad
  type State[S, A] = S => (A, S)

  def unit[S, A](a: A): State[S, A] = state => {
    (a, state)
  }

  def map[S, A, B](sm: State[S, A])(f: A => B): State[S, B] = state => {
    val (value, newState) = sm(state)
    (f(value), newState)
  }

  def flatMap[S, A, B](sm: State[S, A])(f: A => State[S, B]): State[S, B] = state => {
    val (value, newState) = sm(state)
    f(value)(newState)
  }

  // End of definition of the State Monad

  /**
   * Added by me
   * Alternative to the type alias
   */
  object StateAsCaseClass {

    case class State[S, A](run: S => (A, S))

    object State {
      def apply[S, A](a: => A): State[S, A] = State { s => (a, s) }
    }

    def unit[S, A](a: => A): State[S, A] = State { s => (a, s) }

    def map[S, A, B](sm: State[S, A])(f: A => B): State[S, B] = State {
      s0 => {
        val (a, s1) = sm.run(s0)
        (f(a), s1)
      }
    }

    def flatMap[S, A, B](sm: State[S, A])(f: A => State[S, B]): State[S, B] = State {
      s0 => {
        val (a, s1) = sm.run(s0)
        f(a).run(s1)
      }
    }
  } // End of my addition


  /**
   * A stocks portfolio, which associate a stock name to the quantity owned
   */
  type Stocks = Map[String, Double]
  type Transaction[A] = State[Stocks, A]

  /**
   * Returns the quantity of stocks owned for `name`.
   *
   * @param name Name of the stock
   * @return The quantity of stocks owned for `name`.
   */
  def get(name: String): Transaction[Double] = portfolio => {
    (portfolio(name), portfolio)
  }

  /**
   * Buys an amount (dollars) of the stock with given `name`. Returns the number
   * of purchased stocks.
   *
   * @param name   The name of the stocks to buy
   * @param amount The amount in dollars to buy
   * @return The quantity of stocks purchased
   */
  def buy(name: String, amount: Double): Transaction[Double] = portfolio => {
    val purchased = amount / Prices(name)
    val owned = portfolio(name)
    (purchased, portfolio + (name -> (owned + purchased)))
  }

  /**
   * Sells a `quantity` of stocks of the given `name`. Returns the amount of
   * dollars earned by the selling operation.
   *
   * @param name     The name of the stocks to sell
   * @param quantity The quantity of stocks to sell
   * @return The earned amount
   */
  def sell(name: String, quantity: Double): Transaction[Double] = portfolio => {
    val revenue = quantity * Prices(name)
    val owned = portfolio(name)
    (revenue, portfolio + (name -> (owned - quantity)))
  }

  /**
   * Sells all stocks called `from`, and with the revenue buys stocks called `to`.
   * Returns the quantity of stock sold and the quantity of stocks purchased.
   *
   * @param from Stocks to be sold
   * @param to   Stocks to be purchased
   * @return The quantity of stock sold and the quantity of stocks purchased
   */
  def move(from: String, to: String): Transaction[(Double, Double)] = portfolio => {
    val (originallyOwned, _) = get(from)(portfolio)
    val (revenue, newPortfolio) = sell(from, originallyOwned)(portfolio)
    val (purchased, veryNewPortfolio) = buy(to, revenue)(newPortfolio)
    ((originallyOwned, purchased), veryNewPortfolio)
  }

  //  def unit[A](a: A): Transaction[A] = portfolio => {
  //    (a, portfolio)
  //  }
  //
  //  def map[A, B](tx: Transaction[A])(f: A => B): Transaction[B] = portfolio => {
  //    val (value, newPortfolio) = tx(portfolio)
  //    (f(value), newPortfolio)
  //  }
  //
  //  def flatMap[A, B](tx: Transaction[A])(f: A => Transaction[B]): Transaction[B] = portfolio => {
  //    val (value, newPortfolio) = tx(portfolio)
  //    f(value)(newPortfolio)
  //  }

  /**
   * Uses the flatMap and the map functions to implement the same use case of the `move` function,
   * but without passing the updated portfolio explicitly.
   */
  def moveFunc(from: String, to: String): Transaction[(Double, Double)] =
    flatMap(get(from))(
      originallyOwned => flatMap(sell(from, originallyOwned))(
        revenue => map(buy(to, revenue))(
          purchased => (originallyOwned, purchased)
        )
      )
    )

  /**
   * Rewrites the `moveFunc` function, using the `for-yield` syntactic sugar.
   *
   * Error:(115, 31) value flatMap is not a member of ch09._StateMonad_StocksApp.Transaction[Double]
   */
  /*def moveImper(from: String, to: String): Transaction[(Double, Double)] =
    for {
      originallyOwned <- get(from)
      revenue <- sell(from, originallyOwned)
      purchased <- buy(to, revenue)
    } yield {
      (originallyOwned, purchased)
    }*/
}