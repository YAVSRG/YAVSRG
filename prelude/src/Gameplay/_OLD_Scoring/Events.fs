namespace Prelude.Gameplay.Rulesets.OLD

open Prelude
open Prelude.Charts
open Prelude.Gameplay.Replays

type HitStatus =
    | NOTHING = 0

    | HIT_REQUIRED = 1
    | HIT_HOLD_REQUIRED = 2
    | HIT_ACCEPTED = 3

    | RELEASE_REQUIRED = 4
    | RELEASE_ACCEPTED = 5

// this data is in-memory only and not exposed much to other parts of the code
// the flags in particular need never be exposed anywhere else, while the hit deltas can be used on the score screen to give useful data
// most interesting things should come out of a specific score system implementation

type InternalScoreDataRow = (struct (Time * Time array * HitStatus array))
type InternalScoreData = InternalScoreDataRow array

module InternalScore =

    let inline offsetOf (struct (t, _, _): InternalScoreDataRow) = t

    let create_gameplay (miss_window: Time) (keys: int) (notes: TimeArray<NoteRow>) : InternalScoreData =
        notes
        |> Array.map (fun { Time = time; Data = nr } ->
            let times = Array.create keys miss_window
            let statuses = Array.create keys HitStatus.NOTHING

            for k = 0 to (keys - 1) do
                if nr.[k] = NoteType.NORMAL then
                    statuses.[k] <- HitStatus.HIT_REQUIRED
                elif nr.[k] = NoteType.HOLDHEAD then
                    statuses.[k] <- HitStatus.HIT_HOLD_REQUIRED
                elif nr.[k] = NoteType.HOLDTAIL then
                    statuses.[k] <- HitStatus.RELEASE_REQUIRED

            struct (time, times, statuses)
        )

    // used for debugging and test rigs as what the internal data should look like after a "perfect" play
    let create_autoplay (keys: int) (notes: TimeArray<NoteRow>) : InternalScoreData =
        notes
        |> Array.map (fun { Time = time; Data = nr } ->
            let times = Array.zeroCreate keys
            let statuses = Array.create keys HitStatus.NOTHING

            for k = 0 to (keys - 1) do
                if nr.[k] = NoteType.NORMAL || nr.[k] = NoteType.HOLDHEAD then
                    statuses.[k] <- HitStatus.HIT_ACCEPTED
                elif nr.[k] = NoteType.HOLDTAIL then
                    statuses.[k] <- HitStatus.RELEASE_ACCEPTED

            struct (time, times, statuses)
        )

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