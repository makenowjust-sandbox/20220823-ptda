package codes.quine.labs.ptda

import scala.collection.mutable
import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordering.Implicits.infixOrderingOps

final class PtdaException(message: String) extends Exception(message)

/** TimeValuation is an environment type for clocks and timing parameters.
  *
  * @tparam C clock type
  * @tparam TP timing parameter type
  */
final case class TimeValuation[C, TP](clocks: Map[C, Timestamp], params: Map[TP, NumberConstraint])

/** NumberValuation is an environment type for number variables.
  *
  * @tparam NV number variable type
  */
type NumberValuation[NV] = Map[NV, NumberConstraint]

/** StringValuation is an environent type for string variables.
  *
  * @tparam SV string variable type
  */
type StringValuation[SV] = Map[SV, StringConstraint]

/** TimestampConstraint is a value constraint of timestamp. */
enum TimestampConstraint:
  case Fixed(value: Number)
  case Unfixed(nes: Set[Number], range: Range[Timestamp])

  def -(t: Timestamp): NumberConstraint = this match
    case Fixed(v) => NumberConstraint.Fixed(v - t)
    case Unfixed(nes, r) => NumberConstraint.Unfixed(nes.map(_ - t), r - t)

/** NumberConstraint is a value constraint of number. */
enum NumberConstraint:
  case Fixed(value: Number)
  case Unfixed(nes: Set[Number], range: Range[Number])

  def intersect(that: NumberConstraint): Option[NumberConstraint] = (this, that) match
    case (Fixed(v1), Fixed(v2)) => if v1 == v2 then Some(Fixed(v1)) else None
    case (Fixed(v), Unfixed(nes, r)) =>
      if r.contains(v) && !nes.contains(v) then Some(Fixed(v)) else None
    case (Unfixed(nes, r), Fixed(v)) =>
      if r.contains(v) && !nes.contains(v) then Some(Fixed(v)) else None
    case (Unfixed(nes1, r1), Unfixed(nes2, r2)) =>
      r1.intersect(r2).flatMap { r =>
        val nes = (nes1 ++ nes2).filter(r.contains(_))
        if r.start == r.end then
          if nes.contains(r.start) then None else Some(Fixed(r.start))
        else Some(Unfixed(nes, r))
      }

  def +(t: Timestamp): TimestampConstraint = this match
    case Fixed(v) => TimestampConstraint.Fixed(v + t)
    case Unfixed(nes, r) => TimestampConstraint.Unfixed(nes.map(_ + t), r + t)

object NumberConstraint:
  val All = Unfixed(Set.empty, Range(Double.NegativeInfinity, Double.PositiveInfinity))

  def build(v: Number, op: ComparisonOp): NumberConstraint = op match
    case ComparisonOp.Lt =>
      NumberConstraint.Unfixed(Set(v), Range(Double.NegativeInfinity, v))
    case ComparisonOp.Gt =>
      NumberConstraint.Unfixed(Set(v), Range(v, Double.PositiveInfinity))
    case ComparisonOp.LtEq =>
      NumberConstraint.Unfixed(Set.empty, Range(Double.NegativeInfinity, v))
    case ComparisonOp.GtEq =>
      NumberConstraint.Unfixed(Set.empty, Range(v, Double.PositiveInfinity))
    case ComparisonOp.Eq =>
      NumberConstraint.Fixed(v)
    case ComparisonOp.Ne =>
      NumberConstraint.Unfixed(Set(v), Range(Double.NegativeInfinity, Double.PositiveInfinity))

/** Range is an range of values.
  *
  * @tparam T value type
  */
final case class Range[T](start: T, end: T):
  /** Computes the intersection of two ranges.
    * If two ranges have no intersection, then it returns `None`.
    */
  def intersect(that: Range[T])(using Ordering[T]): Option[Range[T]] =
    val values = Seq(Left(start), Left(end), Right(that.start), Right(that.end))
    values.sortBy(_.merge) match
      case Seq(Left(_), Right(start), Left(end), Right(_)) => Some(Range(start, end))
      case Seq(Right(_), Left(start), Right(end), Left(_)) => Some(Range(start, end))
      case Seq(Left(_), Right(start), Right(end), Left(_)) => Some(Range(start, end))
      case Seq(Right(_), Left(start), Left(end), Right(_)) => Some(Range(start, end))
      case _ => None

  def contains(value: T)(using Ordering[T]): Boolean =
    start <= value && value <= end

  def +(t: T)(using Numeric[T]): Range[T] = Range(start + t, end + t)

  def -(t: T)(using Numeric[T]): Range[T] = Range(start - t, end - t)

/** StringConstraint is a value constraint of string. */
enum StringConstraint:
  case Fixed(value: String)
  case Unfixed(nes: Set[String])

  def intersect(that: StringConstraint): Option[StringConstraint] = (this, that) match
    case (Fixed(v1), Fixed(v2)) =>
      if v1 == v2 then Some(Fixed(v1)) else None
    case (Fixed(v), Unfixed(nes)) =>
      if !nes.contains(v) then Some(Fixed(v)) else None
    case (Unfixed(nes), Fixed(v)) =>
      if !nes.contains(v) then Some(Fixed(v)) else None
    case (Unfixed(nes1), Unfixed(nes2)) =>
      Some(Unfixed(nes1 ++ nes2))

object StringConstraint:
  val All = Unfixed(Set.empty)

  def build(v: String, op: EquivalenceOp): StringConstraint = op match
    case EquivalenceOp.Eq =>
      StringConstraint.Fixed(v)
    case EquivalenceOp.Ne =>
      StringConstraint.Unfixed(Set(v))

/** Configuration is a running state for PTDA. */
final case class Configuration[L, C, TP, NV, SV](
  loc: L,
  timeEnv: TimeValuation[C, TP],
  numberEnv: NumberValuation[NV],
  stringEnv: StringValuation[SV]
)

final class Executor[A, L, C, TP, NV, SV](
  final private[this] val ptda: Ptda[A, L, C, TP, NV, SV]
):
  private[this] type Conf = Configuration[L, C, TP, NV, SV]
  private[this] type Trans = Transition[A, L, C, TP, NV, SV]

  /** The last action's timestamp. */
  final private[this] var timestamp: Timestamp = 0

  /** The list of current running configurations. */
  final private[this] var confs: Set[Conf] = Set(initConf)

  /** Computes the initial configuration from the given PTDA. */
  private[this] def initConf: Conf =
    val clocks = ptda.clockVars.map(clock => clock -> (0: Timestamp)).toMap
    val params = ptda.timeParams.map(param => param -> NumberConstraint.All).toMap
    val timeEnv = TimeValuation(clocks, params)
    val numberEnv = ptda.numberVars.map(x => x -> NumberConstraint.All).toMap
      ++ ptda.initNumberEnv.map((x, v) => x -> NumberConstraint.Fixed(v))
    val stringEnv = ptda.stringVars.map(x => x -> StringConstraint.All).toMap
      ++ ptda.initStringEnv.map((x, v) => x -> StringConstraint.Fixed(v))
    Configuration(ptda.initLoc, timeEnv, numberEnv, stringEnv)

  def execute(action: Action[A, NV, SV]): Set[Conf] =
    val nextTimestamp = action.timestamp
    val numberLocals = action.numbers
    val stringLocals = action.strings
    val epsConfs = executeEps(nextTimestamp)

    timestamp = nextTimestamp
    val constraint = TimestampConstraint.Fixed(timestamp)
    val nextConfs = Set.newBuilder[Conf]
    val results = Set.newBuilder[Conf]
    for conf <- epsConfs do
      if !confs.contains(conf) && ptda.acceptLocs.contains(conf.loc) then
        results.addOne(conf)

      for
        trans <- ptda.transitions.getOrElse(conf.loc, Seq.empty)
        if trans.action.exists(_ == action.id)
        (nextConf, nextConstraint) <- evaluate(conf, constraint, trans, updateClock = true, numberLocals, stringLocals)
      do
        nextConfs.addOne(nextConf)
        if ptda.acceptLocs.contains(nextConf.loc) then
          results.addOne(nextConf)

    confs = nextConfs.result()
    results.result()

  def executeEnd(): Set[Conf] =
    val epsConfs = executeEps(timestamp)
    val constraint = TimestampConstraint.Fixed(timestamp)
    val results = Set.newBuilder[Conf]
    for conf <- epsConfs do
      if !confs.contains(conf) && ptda.acceptLocs.contains(conf.loc) then
        results.addOne(conf)
    results.result()

  def executeEps(nextTimestamp: Timestamp): Set[Conf] =
    val queue = mutable.Queue.empty[(Conf, TimestampConstraint)]
    val results = mutable.Set.empty[Conf]

    val constraint = TimestampConstraint.Unfixed(Set.empty, Range(timestamp, nextTimestamp))
    for conf <- confs do
      queue.addOne((conf, constraint))
      results.add(conf)

    while queue.nonEmpty do
      val (conf, constraint) = queue.dequeue()
      for
        trans <- ptda.transitions.getOrElse(conf.loc, Seq.empty)
        if trans.action.isEmpty
        (nextConf, nextConstraint) <- evaluate(conf, constraint, trans, updateClock = false)
      do
        if results.add(nextConf) then
          queue.addOne((nextConf, nextConstraint))

    results.toSet

  def evaluate(conf: Conf, constraint: TimestampConstraint, trans: Trans, updateClock: Boolean, numberLocals: Map[NV, Number] = Map.empty, stringLocals: Map[SV, String] = Map.empty): Option[(Conf, TimestampConstraint)] =
    for
      numberEnv <- evaluateNumberGuards(conf, trans, numberLocals)
      stringEnv <- evaluateStringGuards(conf, trans, stringLocals)
      (params, nextConstraint) <- evaluateTimeGuards(conf, constraint, trans)
    yield
      val nextConf = updateConf(conf, numberEnv, stringEnv, params, trans, updateClock)
      (nextConf, nextConstraint)

  def evaluateNumberGuards(conf: Conf, trans: Trans, locals: Map[NV, Number] = Map.empty): Option[NumberValuation[NV]] =
    trans.numberGuards.foldLeft(Some(conf.numberEnv): Option[NumberValuation[NV]]) { (optEnv, guard) =>
      optEnv.flatMap(evaluateNumberGuard(guard, locals)(_))
    }

  def evaluateNumberGuard(guard: NumberGuard[NV], locals: Map[NV, Number])(env: NumberValuation[NV]): Option[NumberValuation[NV]] =
    val lhs = resolveNV(env, locals, guard.lhs)
    val rhs = guard.rhs.flatMap(resolveNV(env, locals, _))
    val (op, x, u, v) = (lhs, rhs) match
      case (Left(v1), Left(v2)) => return Option.when(guard.op.interpret(v1, v2))(env)
      case (Right((x, u)), Left(v)) => (guard.op, x, u, v)
      case (Left(v), Right((x, u))) => (guard.op.reversed, x, u, v)
      case _ => throw new PtdaException("At least one side of values of constraint expression must be fixed")
    val c = NumberConstraint.build(v, op)
    c.intersect(u).map(env.updated(x, _))

  def resolveNV(env: NumberValuation[NV], locals: Map[NV, Number], x: NV): Either[Number, (NV, NumberConstraint.Unfixed)] =
    locals.get(x).map(Left(_))
      .orElse(env.get(x).map(_ match
        case NumberConstraint.Fixed(v) => Left(v)
        case u: NumberConstraint.Unfixed => Right((x, u))
      ))
      .getOrElse(throw new PtdaException(s"Undefined variable: $x"))

  def evaluateStringGuards(conf: Conf, trans: Trans, locals: Map[SV, String] = Map.empty): Option[StringValuation[SV]] =
    trans.stringGuards.foldLeft(Some(conf.stringEnv): Option[StringValuation[SV]]) { (optEnv, guard) =>
      optEnv.flatMap(evaluateStringGuard(guard, locals)(_))
    }

  def evaluateStringGuard(guard: StringGuard[SV], locals: Map[SV, String])(env: StringValuation[SV]): Option[StringValuation[SV]] =
    val lhs = resolveSV(env, locals, guard.lhs)
    val rhs = guard.rhs.flatMap(resolveSV(env, locals, _))
    val (op, x, u, v) = (lhs, rhs) match
      case (Left(v1), Left(v2)) => return Option.when(guard.op.interpret(v1, v2))(env)
      case (Right((x, u)), Left(v)) => (guard.op, x, u, v)
      case (Left(v), Right((x, u))) => (guard.op, x, u, v)
      case _ => throw new PtdaException("At least one side of values of constraint expression must be fixed")
    val c = StringConstraint.build(v, op)
    c.intersect(u).map(env.updated(x, _))

  def resolveSV(env: StringValuation[SV], locals: Map[SV, String], x: SV): Either[String, (SV, StringConstraint.Unfixed)] =
    locals.get(x).map(Left(_))
      .orElse(env.get(x).map(_ match
        case StringConstraint.Fixed(v) => Left(v)
        case u: StringConstraint.Unfixed => Right((x, u))
      ))
      .getOrElse(throw new PtdaException(s"Undefined variable: $x"))

  def evaluateTimeGuards(conf: Conf, constraint: TimestampConstraint, trans: Trans): Option[(Map[TP, NumberConstraint], TimestampConstraint)] =
    trans.timeGuards.foldLeft(Some((conf.timeEnv.params, constraint)): Option[(Map[TP, NumberConstraint], TimestampConstraint)]) { (optPair, guard) =>
      optPair.flatMap((params, env) => evaluateTimeGuard(guard, conf.timeEnv.clocks)(params, env))
    }

  def evaluateTimeGuard(guard: TimeGuard[C, TP], clocks: Map[C, Timestamp])(params: Map[TP, NumberConstraint], constraint: TimestampConstraint): Option[(Map[TP, NumberConstraint], TimestampConstraint)] =
    val lhs = resolveC(clocks, constraint, guard.lhs)
    val rhs = guard.rhs.flatMap(resolveTP(params, _))
    val (op, xt, u, v) = (lhs, rhs) match
      case (Left(v1), Left(v2)) =>
        return Option.when(guard.op.interpret(v1, v2))((params, constraint))
      case (Right((t, u)), Left(v)) => (guard.op, Left(t), u, v)
      case (Left(v), Right((x, u))) => (guard.op.reversed, Right(x), u, v)
      case _ => throw new PtdaException("At least one side of values of constraint expression must be fixed")
    val c = NumberConstraint.build(v, op)
    c.intersect(u).map { c =>
      xt match
        case Left(t) => (params, c + t)
        case Right(x) => (params.updated(x, c), constraint)
    }

  def resolveC(clocks: Map[C, Timestamp], constraint: TimestampConstraint, x: C): Either[Number, (Timestamp, NumberConstraint.Unfixed)] =
    val t = clocks.getOrElse(x, throw new PtdaException(s"Undefined variable: $x"))
    (constraint - t) match
      case NumberConstraint.Fixed(v) => Left(v)
      case u: NumberConstraint.Unfixed => Right((t, u))

  def resolveTP(params: Map[TP, NumberConstraint], x: TP): Either[Number, (TP, NumberConstraint.Unfixed)] =
    params.get(x).getOrElse(throw new Exception(s"Undefined variable: $x")) match
      case NumberConstraint.Fixed(v) => Left(v)
      case u: NumberConstraint.Unfixed => Right((x, u))

  def updateConf(conf: Conf, numberEnv: NumberValuation[NV], stringEnv: StringValuation[SV], params: Map[TP, NumberConstraint], trans: Trans, updateClock: Boolean, numberLocals: Map[NV, Number] = Map.empty, stringLocals: Map[SV, String] = Map.empty): Conf =
    val nextClocks = resetClock(conf.timeEnv.clocks, trans, updateClock)
    val nextTimeEnv = TimeValuation(nextClocks, params)
    val nextNumberEnv = evaluateNumberUpdates(numberEnv, trans, numberLocals)
    val nextStringEnv = evaluateStringUpdates(stringEnv, trans, stringLocals)
    conf.copy(loc = trans.to, timeEnv = nextTimeEnv, numberEnv = nextNumberEnv, stringEnv = nextStringEnv)

  def evaluateNumberUpdates(env: NumberValuation[NV], trans: Trans, locals: Map[NV, Number]): NumberValuation[NV] =
    trans.numberUpdates.foldLeft(env) { case (env, (x, u)) =>
      val v = evaluateNumberUpdate(env, u, locals)
      env.updated(x, NumberConstraint.Fixed(v))
    }

  def evaluateNumberUpdate(env: NumberValuation[NV], u: NumberUpdate[NV], locals: Map[NV, Number]): Number =
    u.constant + u.coefficients.map { (x, k) =>
      resolveNV(env, locals, x) match
        case Left(v) => v
        case Right(_) => throw new PtdaException("A value of update expression must be fixed")
    }.sum

  def evaluateStringUpdates(env: StringValuation[SV], trans: Trans, locals: Map[SV, String]): StringValuation[SV] =
    trans.stringUpdates.foldLeft(env) { case (env, (x, u)) =>
      val v = evaluateStringUpdate(env, u, locals)
      env.updated(x, StringConstraint.Fixed(v))
    }

  def evaluateStringUpdate(env: StringValuation[SV], u: StringUpdate[SV], locals: Map[SV, String]): String =
    u.value.flatMap(resolveSV(env, locals, _)) match
      case Left(v) => v
      case Right(_) => throw new PtdaException("A value of update expression must be fixed")    

  def resetClock(clocks: Map[C, Timestamp], trans: Trans, updateClock: Boolean): Map[C, Timestamp] =
    if !updateClock && trans.resets.nonEmpty then
      throw new PtdaException("Epsilon transition cannot reset clocks")
    trans.resets.foldLeft(clocks)(_.updated(_, timestamp))
