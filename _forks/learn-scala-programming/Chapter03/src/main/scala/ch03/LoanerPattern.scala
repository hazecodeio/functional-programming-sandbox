package ch03


object LoanerPattern {
  class Loan[-T <: AutoCloseable, +R](app: T => R) extends (T => R) {
    override def apply(t: T): R = try app(t) finally t.close()
  }
  new Loan((_: java.io.BufferedReader).readLine())(Console.in)
  new Loan((_: java.io.BufferedReader).readLine()).apply(Console.in)
  // Alternatively
  new Loan((buf:java.io.BufferedReader) => buf.readLine())(Console.in)
  new Loan((buf:java.io.BufferedReader) => buf.readLine()).apply(Console.in)
}
