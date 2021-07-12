namespace Prelude.Scoring

type Lamp =
    | MFC = 9
    | WF = 8
    | SDP = 7
    | PFC = 6
    | BF = 5
    | SDG = 4
    | FC = 3
    | MF = 2
    | SDCB = 1
    | NONE = 0

module Lamp =

    let calculate (state: AccuracySystemState) : Lamp =
        let c count (zero: unit -> Lamp) one singleDigit more = 
            if count = 0 then zero ()
            elif count = 1 then one
            elif count < 10 then singleDigit
            else more
        c state.ComboBreaks (fun () -> (c state.Judgements.[4] (fun () -> (c state.Judgements.[2] (fun () -> Lamp.MFC) Lamp.WF Lamp.SDP Lamp.PFC)) Lamp.BF Lamp.SDG Lamp.FC)) Lamp.MF Lamp.SDCB Lamp.NONE

module Grade =

    // todo: support cb count targets
    let calculate (thresholds: float array) (state: AccuracySystemState) =
        let percent = state.PointsScored / state.MaxPointsScored
        let mutable i = 0
        while i < thresholds.Length && thresholds.[i] > percent do
            i <- i + 1
        i // higher i is worse grade

type PersonalBests<'T> = ('T * float32) * ('T * float32)

type PersonalBestType =
    | FasterBetter = 3
    | Faster = 2
    | Better = 1
    | None = 0

module PersonalBests =
    let map f ((a, f1), (b, f2)) = ((f a, f1), (f b, f2))

    let update (value: 'T, rate: float32) (((bestA, rateA), (bestR, rateR)): PersonalBests<'T>)  =
        let r, rv =
            if rate > rateR then (value, rate), PersonalBestType.Faster
            elif rate = rateR then
                if value > bestR then (value, rate), PersonalBestType.Faster else (bestR, rate), PersonalBestType.None
            else (bestR, rateR), PersonalBestType.None
        let a, av =
            if value > bestA then (value, rate), PersonalBestType.Better
            elif value = bestA then
                if rate > rateA then (value, rate), PersonalBestType.Better else (bestA, rateA), PersonalBestType.None
            else (bestA, rateA), PersonalBestType.None
        (a, r), (av ||| rv) : PersonalBests<'T> * PersonalBestType