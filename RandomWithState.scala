package Project

trait RandomWithState {
  def seed: Long
  def nextInt(): (Int, RandomWithState)
  def nextInt(n: Int): (Int, RandomWithState)
}