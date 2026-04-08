package it.unibo.pps.polyglot.a01b

import it.unibo.pps.polyglot.OptionToOptional
import it.unibo.pps.util.Optionals.Optional as ScalaOptional

trait ScalaLogics:
  def size: Int
  def mines: Int

  def hit(x: Int, y: Int): java.util.Optional[Integer]
  def won: Boolean

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(override val size: Int, override val mines: Int) extends ScalaLogics:
  private var emptyPositionsHit = 0
  private val random = scala.util.Random()
  private val minesPositions: Array[(Int, Int)] = new Array[(Int, Int)](mines)
  for i <- 0 until mines do minesPositions(i) = (random.nextInt(size), random.nextInt(size))

  override def hit(x: Int, y: Int): java.util.Optional[Integer] =
    for index <- 0 until mines do
      if minesPositions(index) == (x, y) then return OptionToOptional(ScalaOptional.Empty())
    emptyPositionsHit += 1
    var result = 0
    for index <- 0 until mines do
      val (mX, mY) = minesPositions(index)
      if math.abs(mX - x) <= 1 && math.abs(mY - y) <= 1 then result += 1
    OptionToOptional(ScalaOptional.Just(result))

  override def won: Boolean = emptyPositionsHit == size * size - mines
