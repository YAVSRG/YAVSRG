namespace Prelude.Gameplay.RulesetsV2

open System
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
        TimingWindows: (Time * Time) option
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
    | PointsPerJudgement of points: float array

// Behaviour for hit detection / assigning an input to the correct note to hit

[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type HitMechanics =
    | OsuMania // earliest note
    | Interlude of cbrush_threshold: Time // earliest note; if hit is off by `cbrush_threshold` try to find a nearer note
    | Etterna // nearest note

// Behaviour for hold notes

[<Json.AutoCodec>]
type OsuLnWindows =
    {
        Window320: Time
        Window300: Time
        Window200: Time
        Window100: Time
        Window50: Time
        WindowOverhold200: Time
        WindowOverhold100: Time
    }
    
[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type HeadTailCombineRule =
    | OsuMania of OsuLnWindows
    | HeadJudgementOr of judgement_if_dropped: int * judgement_if_overheld: int

[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type HoldMechanics =
    | CombineHeadAndTail of HeadTailCombineRule
    | OnlyRequireHold
    | JudgeReleasesSeparately of windows: ((Time * Time) option) array * judgement_if_overheld: int
    | OnlyJudgeReleases of judgement_if_dropped: int

[<Json.AutoCodec>]
type RulesetV2 =
    {
        Name: string
        Description: string

        Judgements: Judgement array
        Lamps: Lamp array
        Grades: Grade array
        HitMechanics: HitMechanics
        HoldMechanics: HoldMechanics
        Accuracy: AccuracyPoints
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

module RulesetV2 =

    open System.IO
    open System.Security.Cryptography

    /// The 'hash' of a ruleset is used to identify it
    /// The ruleset is rendered to an array of bytes, then a hash is computed
    /// Any change to a ruleset that changes how it functions changes the bytes, and the hash will change
    /// Any change is only cosmetic (e.g. color of a judemgent) does not appear in the bytes, and the hash will stay the same

    let hash (ruleset: RulesetV2) =
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

        match ruleset.HitMechanics with
        | HitMechanics.OsuMania ->
            bw.Write 0uy
        | HitMechanics.Interlude cbrush_threshold ->
            bw.Write 1uy
            bw.Write (float32 cbrush_threshold)
        | HitMechanics.Etterna ->
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
            | HeadTailCombineRule.HeadJudgementOr (judgement_if_dropped, judgement_if_overheld) ->
                bw.Write judgement_if_dropped
                bw.Write judgement_if_overheld
        | HoldMechanics.OnlyRequireHold ->
            bw.Write 0uy
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

        let s = ms.ToArray() |> h.ComputeHash |> BitConverter.ToString
        ruleset.Name.Replace(" ", "") + s.Replace("-", "").Substring(0, 6)