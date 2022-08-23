package codes.quine.labs.ptda

import scala.math.Ordering.Implicits.infixOrderingOps

/** Ptda is a parametric timed data automaton (PTDA).
  *
  * @tparam A input action identifier type
  * @tparam L location type
  * @tparam C clock type
  * @tparam TP timing parameter type
  * @tparam NV number variable type
  * @tparam SV string variable type
  */
final case class Ptda[A, L, C, TP, NV, SV](
  alphabet: Set[A],
  locs: Set[L],
  initLoc: L,
  acceptLocs: Set[L],
  clockVars: Set[C],
  timeParams: Set[TP],
  numberVars: Set[NV],
  initNumberEnv: Map[NV, Number],
  stringVars: Set[SV],
  initStringEnv: Map[SV, String],
  transitions: Map[L, Seq[Transition[A, L, C, TP, NV, SV]]]
)

/** Transition is a transition of PTDA.
  *
  * @tparam A input action identifier type
  * @tparam L location type
  * @tparam C clock type
  * @tparam TP timing parameter type
  * @tparam NV number variable type
  * @tparam SV string variable type
  */
final case class Transition[A, L, C, TP, NV, SV](
  action: Option[A],
  timeGuards: Seq[TimeGuard[C, TP]],
  numberGuards: Seq[NumberGuard[NV]],
  stringGuards: Seq[StringGuard[SV]],
  resets: Set[C],
  numberUpdates: Map[NV, NumberUpdate[NV]],
  stringUpdates: Map[SV, StringUpdate[SV]],
  to: L
)

/** Action is an input action type to PTDA.
  *
  * @tparam A input action identifier type
  * @tparam NV number variable type
  * @tparam SV string variable type
  */
trait Action[A, NV, SV]:
  def id: A
  def numbers: Map[NV, Number]
  def strings: Map[SV, String]
  def timestamp: Timestamp

/** Number data type. */
type Number = Double

/** Timestamp data type. */
type Timestamp = Double

/** TimeGuard is a constraint expression for timing parameters.
  *
  * @tparam C clock type
  * @tparam TP timing parameter type
  */
final case class TimeGuard[C, TP](lhs: C, op: ComparisonOp, rhs: Either[Number, TP])

/** NumberGuard is a constraint expression for number variables.
  *
  * @tparam NV number variable type
  */
final case class NumberGuard[NV](lhs: NV, op: ComparisonOp, rhs: Either[Number, NV])

/** ComparisonOp is an enum type of a comparison operators. */
enum ComparisonOp:
  case Lt   // <
  case LtEq // <=
  case Gt   // >
  case GtEq // >=
  case Eq   // ==
  case Ne   // !=

  /** Returns the reversed version of operator. */
  def reversed: ComparisonOp = this match
    case Lt => Gt
    case LtEq => GtEq
    case Gt => Lt
    case GtEq => LtEq
    case Eq => Eq
    case Ne => Ne

  def interpret[T](v1: T, v2: T)(using Ordering[T]): Boolean = this match
    case Lt => v1 < v2
    case LtEq => v1 <= v2
    case Gt => v1 > v2
    case GtEq => v1 >= v2
    case Eq => v1 == v2
    case Ne => v1 != v2

/** StringGuard is a constraint expression for string variables.
  *
  * @tparam SV string variable type
  */
final case class StringGuard[SV](lhs: SV, op: EquivalenceOp, rhs: Either[String, SV])

/** EquivalenceOp is an enum type of equivalence operators. */
enum EquivalenceOp:
  case Eq // ==
  case Ne // !=

  def interpret[T](v1: T, v2: T)(using Equiv[T]): Boolean = this match
    case Eq => v1 == v2
    case Ne => v1 != v2

/** NumberUpdate is an update expression for number variables.
  *
  * @tparam NV number variable type
  */
final case class NumberUpdate[NV](coefficients: Map[NV, Number], constant: Number)

/** StringUpdate is an update expression for string variables.
  *
  * @tparam SV string variable type
  */
final case class StringUpdate[SV](value: Either[String, SV])
