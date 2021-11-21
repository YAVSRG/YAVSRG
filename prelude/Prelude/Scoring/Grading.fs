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
        c state.ComboBreaks
            ( fun () -> 
                c state.Judgements.[3]
                    ( fun () -> 
                        c state.Judgements.[2]
                            ( fun () -> Lamp.MFC )
                            Lamp.WF Lamp.SDP Lamp.PFC
                    )
                    Lamp.BF Lamp.SDG Lamp.FC
            )
            Lamp.MF Lamp.SDCB Lamp.NONE

type Grade =
    {
        Name: string
        Accuracy: float option
        ComboBreaks: float
        Color: System.Drawing.Color
    }

module Grade =

    let calculateWithTarget (grades: Grade array) (state: AccuracySystemState) =
        let percent = state.PointsScored / state.MaxPointsScored
        let cbPercent = float state.ComboBreaks / float state.MaxPossibleCombo

        let rec loop xs achieved =
            match xs with
            | x :: xs ->
                let percentNeeded =
                    match x.Accuracy with
                    | Some pc when pc > percent -> Some (pc - percent)
                    | Some _
                    | None -> None
                let lessCbsNeeded =
                    if x.ComboBreaks < cbPercent then 
                        Some (state.ComboBreaks - (x.ComboBreaks * float state.MaxPossibleCombo |> System.Math.Truncate |> int))
                    else None
                if percentNeeded.IsNone && lessCbsNeeded.IsNone then loop xs (achieved + 1)
                else {| Grade = achieved; AccNeeded = percentNeeded; LessCbsNeeded = lessCbsNeeded |}

            | [] -> {| Grade = achieved; AccNeeded = None; LessCbsNeeded = None |}
        loop (List.ofArray grades) -1

    let calculate (grades: Grade array) (state: AccuracySystemState) =
        (calculateWithTarget grades state).Grade

type PersonalBests<'T> = { Best: 'T * float32; Fastest: 'T * float32 }

type PersonalBestType =
    | FasterBetter = 3
    | Faster = 2
    | Better = 1
    | None = 0

module PersonalBests =

    let create (value: 'T, rate: float32) : PersonalBests<'T> = { Best = value, rate; Fastest = value, rate }

    let update (value: 'T, rate: float32) ({ Best = bestA, rateA; Fastest = bestR, rateR }: PersonalBests<'T>)  =
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
        { Best = a; Fastest = r }, (av ||| rv) : PersonalBests<'T> * PersonalBestType