namespace Prelude.Gameplay

open Prelude

type HitEventGutsInternal =
    | Hit_ of delta: Time * is_hold_head: bool * missed: bool
    | Release_ of delta: Time * missed: bool * overhold: bool * dropped: bool * missed_head: bool

type HitEventGuts =
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

type HitEvent<'Guts> =
    {
        Index: int
        Time: ChartTime
        Column: int
        Guts: 'Guts
    }

(*
    Accuracy/scoring system metric.
    Each note you hit is assigned a certain number of points - Your % accuracy is points scored out of the possible maximum.
    Combo/combo breaking also built-in - Your combo is the number of notes hit well in a row
*)

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

// todo: remove and have this kind of data calculated by score screen instead
[<Struct>]
type ScoreMetricSnapshot =
    {
        Time: ChartTime
        PointsScored: float
        MaxPointsScored: float
        Combo: int
        Lamp: int
        Mean: Time
        StandardDeviation: Time
        Judgements: int array
    }
    member this.Accuracy = if this.MaxPointsScored = 0.0 then 1.0 else this.PointsScored / this.MaxPointsScored
    static member COUNT = 200