namespace Prelude.Gameplay.Rulesets

open System
open Percyqaz.Common
open Percyqaz.Data
open Prelude

// Judgements are an indicator of how good a hit was, like "Perfect!" or "Nearly!"
// Scores are commonly measured by how many of each judgement you get (for example a good score might be getting all "Perfect!" judgements)

[<Json.AutoCodec>]
type Judgement =
    {
        Name: string
        Color: Color
        BreaksCombo: bool
        TimingWindows: (GameplayTime * GameplayTime) option
    }

// Grades are awarded at the end of a score as a summarising "rank" of how well you did
// They typically follow lettering systems similar to academic exam grades

[<Json.AutoCodec>]
type Grade =
    {
        Name: string
        Accuracy: float
        Color: Color
    }

// Lamps are awarded at the end of the score as a summarising "tag" to indicate certain accomplishments
// Examples: You didn't miss a single note, so you get a "Full Combo" tag, you only got "Perfect" judgements, so you get a "Perfect Full Combo" tag
// These provide alternative accomplishments to grades that can provide different challenges

[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type LampRequirement =
    | JudgementAtMost of judgement_id: int * count: int
    | ComboBreaksAtMost of count: int
    member this.SortKey =
        match this with
        | ComboBreaksAtMost n -> Int32.MaxValue, n
        | JudgementAtMost (j, n) -> j, n
    member this.IsStricterThan(other: LampRequirement) =
        this.SortKey < other.SortKey

[<Json.AutoCodec>]
type Lamp =
    {
        Name: string
        Requirement: LampRequirement
        Color: Color
    }

// Assignment of points per hit
// Your % accuracy is number of points you get / max points possible

[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type AccuracyPoints =
    | WifeCurve of judge: int
    | PointsPerJudgement of points: float array // todo: change all points, and accuracy, to float32

// Behaviour for hit detection / assigning an input to the correct note to hit

[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type NotePriority =
    | OsuMania // earliest note
    | Interlude of cbrush_threshold: GameplayTime // earliest note; if hit is off by `cbrush_threshold` try to find a nearer note
    | Etterna // nearest note
    
[<Json.AutoCodec>]
type HitMechanics =
    {
        NotePriority: NotePriority
        GhostTapJudgement: int option
    }

// Behaviour for hold notes

[<Json.AutoCodec>]
type OsuLnWindows =
    {
        Window320: GameplayTime
        Window300: GameplayTime
        Window200: GameplayTime
        Window100: GameplayTime
        Window50: GameplayTime
        WindowOverhold200: GameplayTime
        WindowOverhold100: GameplayTime
    }
    
[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type HeadTailCombineRule =
    | OsuMania of OsuLnWindows
    | HeadJudgementOr of early_window: GameplayTime * late_window: GameplayTime * judgement_if_dropped: int * judgement_if_overheld: int

[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type HoldMechanics =
    | CombineHeadAndTail of HeadTailCombineRule
    | OnlyRequireHold of release_window: GameplayTime
    | JudgeReleasesSeparately of windows: ((GameplayTime * GameplayTime) option) array * judgement_if_overheld: int
    | OnlyJudgeReleases of judgement_if_dropped: int

type DecimalPlaces =
    | TWO = 2
    | THREE = 3
    | FOUR = 4

[<Json.AutoCodec>]
type RulesetFormatting =
    {
        DecimalPlaces: DecimalPlaces
    }

[<Json.AutoCodec>]
type Ruleset =
    {
        Name: string
        Description: string

        Judgements: Judgement array
        Lamps: Lamp array
        Grades: Grade array
        HitMechanics: HitMechanics
        HoldMechanics: HoldMechanics
        Accuracy: AccuracyPoints
        Formatting: RulesetFormatting
        // todo: judgements are always worth points as fallbacks for certain LN mechanics, even when using wife curve
        // todo: ghost tap ruling
    }
    member this.DefaultJudgement: int = this.Judgements.Length - 1

    member this.GradeName i =
        if i < 0 then "F"
        else if i >= this.Grades.Length then "??"
        else this.Grades.[i].Name
    member this.GradeColor i =
        if i < 0 || i >= this.Grades.Length then
            Color.Gray
        else
            this.Grades.[i].Color

    member this.LampName i =
        if i < 0 then "NONE"
        else if i >= this.Lamps.Length then "??"
        else this.Lamps.[i].Name
    member this.LampColor i =
        if i < 0 || i >= this.Grades.Length then
            Color.White
        else
            this.Lamps.[i].Color

    member this.JudgementName i = 
        if i < 0 || i >= this.Judgements.Length then "??"
        else this.Judgements.[i].Name
    member this.JudgementColor i = 
        if i < 0 || i >= this.Judgements.Length then Color.Gray
        else this.Judgements.[i].Color

    member this.NoteWindows : GameplayTime * GameplayTime =
        match this.Judgements |> Seq.rev |> Seq.tryPick _.TimingWindows with
        | Some (early, late) -> early, late
        | None -> 0.0f<ms / rate>, 0.0f<ms / rate>
    
    member this.ReleaseWindows : GameplayTime * GameplayTime =
        match this.HoldMechanics with
        | HoldMechanics.OnlyRequireHold window -> 
            -window, window
        | HoldMechanics.JudgeReleasesSeparately (windows, _) ->
            match windows |> Seq.rev |> Seq.tryPick id with
            | Some (early, late) -> early, late
            | None -> 0.0f<ms / rate>, 0.0f<ms / rate>
        | HoldMechanics.OnlyJudgeReleases _ -> 
            this.NoteWindows
        | HoldMechanics.CombineHeadAndTail (HeadTailCombineRule.OsuMania w) ->
            -w.Window50, w.Window100 - 1.0f<ms / rate>
        | HoldMechanics.CombineHeadAndTail (HeadTailCombineRule.HeadJudgementOr (early, late, _, _)) ->
            early, late

    member this.LargestWindow : GameplayTime =
        let n_early, n_late = this.NoteWindows
        let r_early, r_late = this.ReleaseWindows
        max
            (max (abs n_early) n_late)
            (max (abs r_early) r_late)

    member this.FormatAccuracy(accuracy: float) =
        match this.Formatting.DecimalPlaces with
        | DecimalPlaces.FOUR -> sprintf "%.4f%%" (accuracy * 100.0)
        | DecimalPlaces.THREE ->
            if accuracy = 1.0 then
                "100.000%"
            elif accuracy > 0.99998 then
                sprintf "%.4f%%" (accuracy * 100.0)
            else
                sprintf "%.3f%%" (accuracy * 100.0)
        | _ ->
            if accuracy = 1.0 then
                "100.00%"
            elif accuracy > 0.99998 then
                sprintf "%.4f%%" (accuracy * 100.0)
            elif accuracy > 0.9998 then
                sprintf "%.3f%%" (accuracy * 100.0)
            else
                sprintf "%.2f%%" (accuracy * 100.0)

module Ruleset =

    open System.IO
    open System.Security.Cryptography

    /// The 'hash' of a ruleset is used to identify it
    /// The ruleset is rendered to an array of bytes, then a hash is computed
    /// Any change to a ruleset that changes how it functions changes the bytes, and the hash will change
    /// Any change is only cosmetic (e.g. color of a judemgent) does not appear in the bytes, and the hash will stay the same

    let hash (ruleset: Ruleset) =
        let h = SHA256.Create()
        use ms = new MemoryStream()
        use bw = new BinaryWriter(ms)

        for j in ruleset.Judgements do
            bw.Write j.BreaksCombo
            match j.TimingWindows with
            | Some (early, late) -> 
                bw.Write (float32 early)
                bw.Write (float32 late)
            | None ->
                bw.Write 0.0f
                bw.Write 0.0f

        for g in ruleset.Grades do
            bw.Write g.Accuracy

        for l in ruleset.Lamps do
            match l.Requirement with
            | LampRequirement.JudgementAtMost (j, c) ->
                bw.Write 0uy
                bw.Write j
                bw.Write c
            | LampRequirement.ComboBreaksAtMost c -> 
                bw.Write 1uy
                bw.Write c

        match ruleset.HitMechanics.GhostTapJudgement with
        | Some j -> bw.Write j
        | None -> bw.Write 0uy

        match ruleset.HitMechanics.NotePriority with
        | NotePriority.OsuMania ->
            bw.Write 0uy
        | NotePriority.Interlude cbrush_threshold ->
            bw.Write 1uy
            bw.Write (float32 cbrush_threshold)
        | NotePriority.Etterna ->
            bw.Write 2uy

        match ruleset.HoldMechanics with
        | HoldMechanics.CombineHeadAndTail rule ->
            match rule with
            | HeadTailCombineRule.OsuMania windows ->
                bw.Write (float32 windows.Window320)
                bw.Write (float32 windows.Window300)
                bw.Write (float32 windows.Window200)
                bw.Write (float32 windows.Window100)
                bw.Write (float32 windows.Window50)
                bw.Write (float32 windows.WindowOverhold200)
                bw.Write (float32 windows.WindowOverhold100)
            | HeadTailCombineRule.HeadJudgementOr (early_window, late_window, judgement_if_dropped, judgement_if_overheld) ->
                bw.Write (float32 early_window)
                bw.Write (float32 late_window)
                bw.Write judgement_if_dropped
                bw.Write judgement_if_overheld
        | HoldMechanics.OnlyRequireHold window ->
            bw.Write 0uy
            bw.Write (float32 window)
        | HoldMechanics.JudgeReleasesSeparately (judgement_windows, judgement_if_dropped) ->
            for j in judgement_windows do
                match j with
                | Some (early, late) -> 
                    bw.Write (float32 early)
                    bw.Write (float32 late)
                | None ->
                    bw.Write 0.0f
                    bw.Write 0.0f
            bw.Write judgement_if_dropped
        | HoldMechanics.OnlyJudgeReleases judgement_if_dropped ->
            bw.Write 1uy
            bw.Write judgement_if_dropped

        match ruleset.Accuracy with
        | AccuracyPoints.WifeCurve judge ->
            bw.Write judge
        | AccuracyPoints.PointsPerJudgement points ->
            for p in points do
                bw.Write p

        let first_character = 
            match ruleset.Name.Trim() with
            | "" -> "_"
            | otherwise ->
                if Char.IsAsciiLetterOrDigit otherwise.[0] then 
                    otherwise.[0].ToString().ToUpperInvariant()
                else "_"
        let hash_string = ms.ToArray() |> h.ComputeHash |> BitConverter.ToString
        first_character + hash_string.Replace("-", "").Substring(0, 8)

    let check (ruleset: Ruleset) : Result<Ruleset, string> =
        let inline valid (x: GameplayTime) = x |> float32 |> Single.IsFinite
        let invalid = valid >> not
        let inline negative (x: GameplayTime) = invalid x || x < 0.0f<ms / rate>
        let inline positive (x: GameplayTime) = invalid x || x > 0.0f<ms / rate>

        try
            if ruleset.Judgements.Length = 0 then failwith "Must have at least one judgement"

            let mutable w_min = 0.0f<ms / rate>
            let mutable w_max = 0.0f<ms / rate>
            for j in ruleset.Judgements do
                match j.TimingWindows with
                | Some (early, late) ->
                    if invalid(early) then failwithf "Invalid floating point early window for '%s'" j.Name
                    if early > w_min then failwithf "Early window %.3fms for '%s' must be %.3fms or earlier" early j.Name w_min
                    w_min <- early
                    if invalid(late) then failwithf "Invalid floating point late window for '%s'" j.Name
                    if late < w_max then failwithf "Late window %.3fms for '%s' must be %.3fms or later" late j.Name w_max
                    w_max <- late
                | None -> ()

            let mutable g_acc = -infinity
            for g in ruleset.Grades do
                if g.Accuracy <= g_acc then failwithf "Grade boundary %.6f for '%s' must be at least %.6f" g.Accuracy g.Name g_acc
                g_acc <- g.Accuracy

            let mutable l_req = LampRequirement.ComboBreaksAtMost Int32.MaxValue
            for l in ruleset.Lamps do
                if not (l.Requirement.IsStricterThan l_req) then
                    failwithf "Lamp requirement %A for '%s' must be stricter than %A" l.Requirement l.Name l_req
                l_req <- l.Requirement

            match ruleset.HitMechanics.GhostTapJudgement with
            | Some judgement -> 
                
                match ruleset.Accuracy with 
                | AccuracyPoints.PointsPerJudgement _ -> ()
                | _ -> failwith "GhostTapJudgement must be used with PointsPerJudgement accuracy (otherwise it isn't clear what points it's worth)"

                if judgement < 0 || judgement >= ruleset.Judgements.Length then failwith "GhostTapJudgement must be a valid judgement"
            | None -> ()

            match ruleset.HitMechanics.NotePriority with
            | NotePriority.OsuMania -> ()
            | NotePriority.Interlude cbrush_threshold ->

                if cbrush_threshold < 0.0f<ms / rate> then failwith "Interlude `cbrush_threshold` must be non-negative"
                if cbrush_threshold > w_max || cbrush_threshold > abs w_min then failwith "Interlude `cbrush_threshold` must not exceed timing windows"

            | NotePriority.Etterna -> ()

            match ruleset.HoldMechanics with
            | HoldMechanics.CombineHeadAndTail rule ->

                match ruleset.Accuracy with 
                | AccuracyPoints.PointsPerJudgement _ -> ()
                | _ -> failwith "CombineHeadAndTail rules must be used with PointsPerJudgement accuracy (otherwise it isn't clear what points you give overholds)"

                match rule with
                | HeadTailCombineRule.OsuMania windows ->

                    if ruleset.Judgements.Length <> 6 then failwith "osu!mania ln mechanics must be used with exactly 6 judgements"
                    
                    if negative(windows.Window320) then failwith "osu!mania ln Window320 must be non-negative"
                    if negative(windows.Window300) then failwith "osu!mania ln Window300 must be non-negative"
                    if negative(windows.Window200) then failwith "osu!mania ln Window200 must be non-negative"
                    if negative(windows.Window100) then failwith "osu!mania ln Window100 must be non-negative"
                    if negative(windows.Window50) then failwith "osu!mania ln Window50 must be non-negative"
                    if negative(windows.WindowOverhold200) then failwith "osu!mania ln WindowOverhold200 must be non-negative"
                    if negative(windows.WindowOverhold100) then failwith "osu!mania ln WindowOverhold100 must be non-negative"

                    // todo: make sure they are ordered in some way

                | HeadTailCombineRule.HeadJudgementOr (early_window, late_window, judgement_if_dropped, judgement_if_overheld) ->

                    if positive(early_window) then failwith "HeadJudgementOr `early_window` must be non-positive"
                    if negative(late_window) then failwith "HeadJudgementOr `late_window` must be non-negative"
                    if judgement_if_dropped < 0 || judgement_if_dropped >= ruleset.Judgements.Length then failwith "HeadJudgementOr `judgement_if_dropped` must be a valid judgement"
                    if judgement_if_overheld < 0 || judgement_if_overheld >= ruleset.Judgements.Length then failwith "HeadJudgementOr `judgement_if_dropped` must be a valid judgement"

            | HoldMechanics.OnlyRequireHold window ->
                if window < 0.0f<ms / rate> then failwith "OnlyRequireHold window must be non-negative"
            | HoldMechanics.JudgeReleasesSeparately (windows, judgement_if_dropped) ->
                if windows.Length <> ruleset.Judgements.Length then failwith "JudgeReleasesSeparately `windows` must match judgement count"
                let mutable w_min = 0.0f<ms / rate>
                let mutable w_max = 0.0f<ms / rate>
                for w in windows do
                    match w with
                    | Some (early, late) ->
                        if early > w_min then failwithf "Early release window %.3fms must be %.3fms or earlier" early w_min
                        w_min <- early
                        if late < w_max then failwithf "Late release window %.3fms must be %.3fms or later" late w_max
                        w_max <- late
                    | None -> ()
                if judgement_if_dropped < 0 || judgement_if_dropped >= ruleset.Judgements.Length then failwith "JudgeReleasesSeparately `judgement_if_dropped` must be a valid judgement"
            | HoldMechanics.OnlyJudgeReleases judgement_if_dropped ->
                if judgement_if_dropped < 0 || judgement_if_dropped >= ruleset.Judgements.Length then failwith "OnlyJudgeReleases `judgement_if_dropped` must be a valid judgement"

            match ruleset.Accuracy with
            | AccuracyPoints.WifeCurve judge -> if judge < 2 || judge > 9 then failwith "WifeCurve `judge` must be a valid judge value"
            | AccuracyPoints.PointsPerJudgement points ->
                for p in points do
                    if not (Double.IsFinite p) then failwith "PointsPerJudgement `points` contains invalid floating point values"

            if not (Enum.IsDefined ruleset.Formatting.DecimalPlaces) then
                failwithf "Unsupported decimal places formatting: %O" ruleset.Formatting.DecimalPlaces

            if ruleset.LargestWindow > 500.0f<ms / rate> then failwithf "Timing window maximum is 500ms, this ruleset contains a largest window of %f" ruleset.LargestWindow

            Ok ruleset
        with err ->
            Error err.Message