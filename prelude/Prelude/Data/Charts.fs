namespace Prelude.Data.Charts

open System
open System.IO
open System.IO.Compression
open System.Collections.Generic
open System.Collections.Concurrent
open Percyqaz.Json
open Prelude.Common
open Prelude.ChartFormats.Interlude
open Prelude.ChartFormats.Conversions
open Prelude.Scoring
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Layout
open Prelude.Gameplay.Difficulty

module Caching =

    [<Json.AllRequired>]
    type CachedChart =
        {
            FilePath: string
            Title: string
            Artist: string
            Creator: string
            Pack: string
            Hash: string
            Keys: int
            Length: Time
            BPM: (float32<ms/beat> * float32<ms/beat>)
            DiffName: string
            Physical: float
            Technical: float
        }
        static member Default =
            {
                FilePath = ""
                Title = ""
                Artist = ""
                Creator = ""
                Pack = ""
                Hash = ""
                Keys = 4
                Length = 0.0f<ms>
                BPM = (500.0f<ms/beat>, 500.0f<ms/beat>)
                DiffName = ""
                Physical = 0.0
                Technical = 0.0
            }

    let cacheChart (chart: Chart) : CachedChart =
        let lastNote = chart.LastNote
        let rating = RatingReport(chart.Notes, 1.0f, Layout.Spread, chart.Keys)
        {
            FilePath = chart.FileIdentifier
            Title = chart.Header.Title
            Artist = chart.Header.Artist
            Creator = chart.Header.Creator
            Pack = chart.Header.SourcePack
            Hash = Chart.hash chart
            Keys = chart.Keys
            Length = lastNote - chart.FirstNote
            // todo: move to Chart module
            BPM = ``Interlude to osu!``.minMaxBPM (List.ofSeq chart.BPM.Data) lastNote
            DiffName = chart.Header.DiffName
            Physical = rating.Physical
            Technical = rating.Technical
        }

module Collections =

    [<RequireQualifiedAccess>]
    type Goal =
    | None
    | Clear of rulesetId: string
    | Lamp of rulesetId: string * lamp: int
    | Accuracy of rulesetId: string * float

    type PlaylistData = { Mods: Setting<ModState>; Rate: Setting.Bounded<float32> } with
        static member Make mods rate = { Mods = Setting.simple mods; Rate = Setting.rate rate }
        static member Default = { Mods = Setting.simple Map.empty; Rate = Setting.rate 1.0f }

    type GoalData = { Mods: Setting<ModState>; Rate: Setting.Bounded<float32>; Goal: Setting<Goal> } with
        static member Make mods rate goal = { Mods = Setting.simple mods; Rate = Setting.rate rate; Goal = Setting.simple goal }
        static member Default = { Mods = Setting.simple Map.empty; Rate = Setting.rate 1.0f; Goal = Setting.simple Goal.None }

    type Collection =
    | Collection of List<string> // duplicates not allowed
    | Playlist of List<string * PlaylistData> // order of list matters
    | Goals of List<string * GoalData>
        member this.ToCollection() =
            match this with
            | Collection l -> Collection l
            | Playlist p -> 
                Seq.map fst p
                |> Seq.distinct |> List |> Collection
            | Goals g -> 
                Seq.map fst g
                |> Seq.distinct |> List |> Collection
        member this.ToPlaylist(mods, rate) = 
            match this with
            | Collection l -> 
                Seq.map (fun i -> i, PlaylistData.Make mods rate) l
                |> List |> Playlist
            | Playlist p -> Playlist p
            | Goals g -> 
                Seq.map (fun (x, data: GoalData) -> x, PlaylistData.Make data.Mods.Value data.Rate.Value) g
                |> List |> Playlist
        member this.ToGoals(mods, rate) = 
            match this with
            | Collection l ->
                Seq.map (fun i -> i, GoalData.Make mods rate Goal.None) l
                |> List |> Goals
            | Playlist p ->
                Seq.map (fun (x, data: PlaylistData) -> x, GoalData.Make data.Mods.Value data.Rate.Value Goal.None) p
                |> List |> Goals
            | Goals g -> Goals g
        member this.IsEmpty() =
            match this with
            | Collection l -> l.Count = 0
            | Playlist p -> p.Count = 0
            | Goals g -> g.Count = 0
        static member Blank = Collection (ResizeArray<_>())

    [<RequireQualifiedAccess>]
    type LevelSelectContext =
        | None
        | Playlist of index: int * id: string * data: PlaylistData
        | Goal of index: int * id: string * data: GoalData
        member this.Id =
            match this with
            | None -> -1, ""
            | Playlist (i, id, _)
            | Goal (i, id, _) -> i, id

module Sorting =

    open Caching
    open FParsec

    let private firstCharacter (s: string) =
        if s.Length = 0 then "?"
        elif Char.IsLetterOrDigit s.[0] then s.[0].ToString().ToUpper()
        else "?"

    let dateLastPlayed (c: CachedChart, ctx) =
        match Prelude.Data.Scores.Scores.getScoreData c.Hash with
        | Some d ->
            let daysAgo = (DateTime.Today - d.LastPlayed).TotalDays
            if daysAgo < 0 then 0, "Today"
            elif daysAgo < 1 then 1, "Yesterday"
            elif daysAgo < 7 then 2, "This week"
            elif daysAgo < 30 then 3, "This month"
            elif daysAgo < 60 then 4, "A month ago"
            elif daysAgo < 90 then 5, "2 months ago"
            elif daysAgo < 120 then 6, "3 months ago"
            elif daysAgo < 210 then 7, "6 months ago"
            elif daysAgo < 3600 then 8, "A long time ago"
            else 9, "Never"
        | None -> 9, "Never"

    type GroupContext = { Rate: float32; RulesetId: string; Ruleset: Ruleset }

    let gradeAchieved (c: CachedChart, ctx: GroupContext) =
        match Prelude.Data.Scores.Scores.getScoreData c.Hash with
        | Some d ->
            if d.Bests.ContainsKey ctx.RulesetId then
                match Grading.PersonalBests.best_this_rate ctx.Rate d.Bests.[ctx.RulesetId].Grade with
                | Some i -> i, ctx.Ruleset.GradeName i
                | None -> -2, "No grade achieved"
            else -2, "No grade achieved"
        | None -> -2, "No grade achieved"

    let lampAchieved (c: CachedChart, ctx: GroupContext) =
        match Prelude.Data.Scores.Scores.getScoreData c.Hash with
        | Some d ->
            if d.Bests.ContainsKey ctx.RulesetId then
                match Grading.PersonalBests.best_this_rate ctx.Rate d.Bests.[ctx.RulesetId].Lamp with
                | Some i -> i, ctx.Ruleset.LampName i
                | None -> -2, "No lamp achieved"
            else -2, "No lamp achieved"
        | None -> -2, "No lamp achieved"

    type GroupMethod = CachedChart * GroupContext -> int * string
    let groupBy : IDictionary<string, GroupMethod> = dict[
            "Level", fun (c, _) -> let lvl = int (c.Physical * 5.0) in lvl, sprintf "Level %i" lvl
            "Pack", fun (c, _) -> 0, c.Pack
            "Date Played", dateLastPlayed
            "Grade", gradeAchieved
            "Lamp", lampAchieved
            "Title", fun (c, _) -> 0, firstCharacter c.Title
            "Artist", fun (c, _) -> 0, firstCharacter c.Artist
            "Creator", fun (c, _) -> 0, firstCharacter c.Creator
            "Keymode", fun (c, _) -> c.Keys, c.Keys.ToString() + "K"
            "Collections", fun _ -> 0, "" // Placeholder for UI purposes, UI is hard coded to call collection grouping behaviour when this is chosen
        ]

    let private compareBy (f: CachedChart -> IComparable) = fun a b -> f(fst a).CompareTo <| f(fst b)
    let private thenCompareBy (f: CachedChart -> IComparable) cmp =
        let cmp2 = compareBy f
        fun a b -> match cmp a b with 0 -> cmp2 a b | x -> x

    type SortMethod = Comparison<CachedChart * Collections.LevelSelectContext>
    let sortBy : IDictionary<string, SortMethod> = dict[
            "Physical", Comparison(compareBy (fun x -> x.Physical))
            "Technical", Comparison(compareBy (fun x -> x.Technical))
            "Title", Comparison(compareBy (fun x -> x.Title) |> thenCompareBy (fun x -> x.Physical))
            "Artist", Comparison(compareBy (fun x -> x.Artist) |> thenCompareBy (fun x -> x.Physical))
            "Creator", Comparison(compareBy (fun x -> x.Creator) |> thenCompareBy (fun x -> x.Physical))
        ]

    type FilterPart = 
        | Criterion of string * string
        | String of string
        | Impossible
    type Filter = FilterPart list

    module Filter =

        let private string = " =:<>\"" |> isNoneOf |> many1Satisfy |>> fun s -> s.ToLower()
        let private word = string |>> String
        let private pstring = between (pchar '"') (pchar '"') ("\"" |> isNoneOf |> many1Satisfy) |>> fun s -> String <| s.ToLower()
        let private criterion = string .>>. (pchar '=' >>. string) |>> Criterion
        let private filter = sepBy (attempt criterion <|> pstring <|> word) spaces1 .>> spaces

        let parse (str: string) =
            match run filter (str.Trim()) with
            | Success (x, _, _) -> x
            | Failure (f, _, _) -> [Impossible]

        let apply (filter: Filter) (charts: CachedChart seq) =
            seq {
                for c in charts do
                    let s = (c.Title + " " + c.Artist + " " + c.Creator + " " + c.DiffName + " " + c.Pack).ToLower()
                    if List.forall
                        (
                            function
                            | Impossible -> false
                            | String str -> s.Contains str
                            | Criterion ("k", n)
                            | Criterion ("key", n)
                            | Criterion ("keys", n) -> c.Keys.ToString() = n
                            | _ -> true
                        )
                        filter
                    then yield c
            }

        let applyf (filter: Filter) (charts: (CachedChart * Collections.LevelSelectContext) seq) =
               seq {
                   for c, context in charts do
                       let s = (c.Title + " " + c.Artist + " " + c.Creator + " " + c.DiffName + " " + c.Pack).ToLower()
                       if List.forall
                           (
                               function
                               | Impossible -> false
                               | String str -> s.Contains str
                               | Criterion ("k", n)
                               | Criterion ("key", n)
                               | Criterion ("keys", n) -> c.Keys.ToString() = n
                               | _ -> true
                           )
                           filter
                       then yield c, context
               }