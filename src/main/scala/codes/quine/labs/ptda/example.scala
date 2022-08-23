package codes.quine.labs.ptda

object example:
  type NumberVar = Nothing

  enum StringVar:
    case PacketId
    case CurrentPacketId

  enum ClockVar:
    case ElapsedTime
  
  type TimeParam = Nothing

  enum Loc:
    case Init
    case Wait
    case Good
    case Bad

  enum ActId:
    case Publish
    case PubAck

  enum Act extends Action[ActId, NumberVar, StringVar]:
    case Publish(packetId: String, timestamp: Timestamp)
    case PubAck(packetId: String, timestamp: Timestamp)

    def id: ActId = this match
      case Publish(_, _) => ActId.Publish
      case PubAck(_, _) => ActId.PubAck

    def numbers: Map[NumberVar, Number] = Map.empty

    def strings: Map[StringVar, String] = this match
      case Publish(pid, _) => Map(StringVar.PacketId -> pid)
      case PubAck(pid, _) => Map(StringVar.PacketId -> pid)

  val ptda = Ptda(
    alphabet = Set(ActId.Publish, ActId.PubAck),
    locs = Set(Loc.Init, Loc.Wait, Loc.Good, Loc.Bad),
    initLoc = Loc.Init,
    acceptLocs = Set(Loc.Bad),
    clockVars = Set(ClockVar.ElapsedTime),
    timeParams = Set.empty,
    numberVars = Set.empty,
    initNumberEnv = Map.empty,
    stringVars = Set(StringVar.CurrentPacketId),
    initStringEnv = Map.empty,
    transitions = Map(
      Loc.Init -> Seq(
        Transition(
          Some(ActId.Publish),
          Seq.empty,
          Seq.empty,
          Seq(StringGuard(StringVar.PacketId, EquivalenceOp.Ne, Right(StringVar.CurrentPacketId))),
          Set.empty,
          Map.empty,
          Map.empty,
          Loc.Init,
        ),
        Transition(
          Some(ActId.PubAck),
          Seq.empty,
          Seq.empty,
          Seq(StringGuard(StringVar.PacketId, EquivalenceOp.Ne, Right(StringVar.CurrentPacketId))),
          Set.empty,
          Map.empty,
          Map.empty,
          Loc.Init,
        ),
        Transition(
          Some(ActId.Publish),
          Seq.empty,
          Seq.empty,
          Seq(StringGuard(StringVar.PacketId, EquivalenceOp.Eq, Right(StringVar.CurrentPacketId))),
          Set(ClockVar.ElapsedTime),
          Map.empty,
          Map.empty,
          Loc.Wait,
        ),
        Transition(
          Some(ActId.PubAck),
          Seq.empty,
          Seq.empty,
          Seq(StringGuard(StringVar.PacketId, EquivalenceOp.Eq, Right(StringVar.CurrentPacketId))),
          Set.empty,
          Map.empty,
          Map.empty,
          Loc.Bad,
        ),
      ),
      Loc.Wait -> Seq(
        Transition(
          Some(ActId.Publish),
          Seq.empty,
          Seq.empty,
          Seq(StringGuard(StringVar.PacketId, EquivalenceOp.Ne, Right(StringVar.CurrentPacketId))),
          Set.empty,
          Map.empty,
          Map.empty,
          Loc.Wait,
        ),
        Transition(
          Some(ActId.PubAck),
          Seq.empty,
          Seq.empty,
          Seq(StringGuard(StringVar.PacketId, EquivalenceOp.Ne, Right(StringVar.CurrentPacketId))),
          Set.empty,
          Map.empty,
          Map.empty,
          Loc.Wait,
        ),
        Transition(
          Some(ActId.PubAck),
          Seq(TimeGuard(ClockVar.ElapsedTime, ComparisonOp.Lt, Left(10))),
          Seq.empty,
          Seq(StringGuard(StringVar.PacketId, EquivalenceOp.Eq, Right(StringVar.CurrentPacketId))),
          Set.empty,
          Map.empty,
          Map.empty,
          Loc.Good,
        ),
        Transition(
          None,
          Seq(TimeGuard(ClockVar.ElapsedTime, ComparisonOp.Eq, Left(10))),
          Seq.empty,
          Seq.empty,
          Set.empty,
          Map.empty,
          Map.empty,
          Loc.Bad,
        ),
      )
    )
  )

  val inputs = Seq(
    Act.Publish("1000", 2),
    Act.Publish("1001", 3),
    Act.Publish("1002", 6),
    Act.PubAck("1001", 8),
    Act.PubAck("1002", 11),
    Act.PubAck("1000", 13),
    Act.PubAck("1010", 15),
  )
