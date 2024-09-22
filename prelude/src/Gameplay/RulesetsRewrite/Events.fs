namespace Prelude.Gameplay

open Prelude

type GameplayEvent<'Guts> =
    {
        Index: int
        Time: ChartTime
        Column: int
        Action: 'Guts
    }

type internal GameplayActionInternal =
    | HIT of delta: Time * is_hold_head: bool * missed: bool
    | RELEASE of 
        release_delta: Time * 
        missed: bool * 
        overhold: bool * 
        dropped: bool * 
        head_delta: Time *
        missed_head: bool
    | BREAK_HOLD
    | REGRAB_HOLD
    | GHOST_TAP

type GameplayAction =
    | Hit of
        {|
            Judgement: JudgementId option
            Missed: bool
            Delta: Time
            IsHold: bool
        |}
    | Release of
        {|
            Judgement: JudgementId option
            Missed: bool
            Delta: Time
            Overhold: bool
            Dropped: bool
        |}
    | BreakHold
    | RegrabHold
    | GhostTap

[<Struct>]
type private HoldInternalState =
    | Nothing
    | Holding
    | Dropped
    | MissedHeadThenHeld
    | MissedHead

[<Struct>]
[<RequireQualifiedAccess>]
type HoldState =
    | Released
    | Holding
    | Dropped
    | MissedHead
    | InTheFuture
    member this.ShowInReceptor = this = Holding || this = Dropped || this = Released