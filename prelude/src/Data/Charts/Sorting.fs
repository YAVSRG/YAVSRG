namespace Prelude.Data.Charts

open System
open System.Collections.Generic
open Prelude
open Prelude.Gameplay

module Sorting =

    open Caching
    open FParsec

    let private firstCharacter (s: string) =
        if s.Length = 0 then "?"
        elif Char.IsLetterOrDigit s.[0] then s.[0].ToString().ToUpper()
        else "?"

    let dateLastPlayed (c: CachedChart, ctx) =
        match Prelude.Data.Scores.Scores.getData c.Hash with
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

    let has_comment (c: CachedChart, comment: string) =
        match Prelude.Data.Scores.Scores.getData c.Hash with
        | Some d -> d.Comment.Contains(comment, StringComparison.OrdinalIgnoreCase)
        | None -> false

    let gradeAchieved (c: CachedChart, ctx: GroupContext) =
        match Prelude.Data.Scores.Scores.getData c.Hash with
        | Some d ->
            if d.Bests.ContainsKey ctx.RulesetId then
                match Grading.PersonalBests.best_this_rate ctx.Rate d.Bests.[ctx.RulesetId].Grade with
                | Some i -> i, ctx.Ruleset.GradeName i
                | None -> -2, "No grade achieved"
            else -2, "No grade achieved"
        | None -> -2, "No grade achieved"

    let lampAchieved (c: CachedChart, ctx: GroupContext) =
        match Prelude.Data.Scores.Scores.getData c.Hash with
        | Some d ->
            if d.Bests.ContainsKey ctx.RulesetId then
                match Grading.PersonalBests.best_this_rate ctx.Rate d.Bests.[ctx.RulesetId].Lamp with
                | Some i -> i, ctx.Ruleset.LampName i
                | None -> -2, "No lamp achieved"
            else -2, "No lamp achieved"
        | None -> -2, "No lamp achieved"

    type GroupMethod = CachedChart * GroupContext -> int * string
    let groupBy : IDictionary<string, GroupMethod> = dict [
            "pack", fun (c, _) -> 0, c.Pack
            "date_played", dateLastPlayed
            "grade", gradeAchieved
            "lamp", lampAchieved
            "title", fun (c, _) -> 0, firstCharacter c.Title
            "artist", fun (c, _) -> 0, firstCharacter c.Artist
            "creator", fun (c, _) -> 0, firstCharacter c.Creator
            "keymode", fun (c, _) -> c.Keys, c.Keys.ToString() + "K"
        ]

    let private compareBy (f: CachedChart -> IComparable) = fun a b -> f(fst a).CompareTo <| f(fst b)
    let private thenCompareBy (f: CachedChart -> IComparable) cmp =
        let cmp2 = compareBy f
        fun a b -> match cmp a b with 0 -> cmp2 a b | x -> x

    type SortMethod = Comparison<CachedChart * Collections.LibraryContext>
    let sortBy : IDictionary<string, SortMethod> = dict [
            "difficulty", Comparison(compareBy (fun x -> x.Physical))
            "bpm", Comparison(compareBy (fun x -> let (a, b) = x.BPM in (1f/a, 1f/b)) |> thenCompareBy (fun x -> x.Physical))
            "title", Comparison(compareBy (fun x -> x.Title) |> thenCompareBy (fun x -> x.Physical))
            "artist", Comparison(compareBy (fun x -> x.Artist) |> thenCompareBy (fun x -> x.Physical))
            "creator", Comparison(compareBy (fun x -> x.Creator) |> thenCompareBy (fun x -> x.Physical))
        ]

    type FilterPart = 
        | Equals of string * string
        | LessThan of string * float
        | MoreThan of string * float
        | String of string
        | Impossible
    type Filter = FilterPart list

    module Filter =

        let private string = " =:<>\"" |> isNoneOf |> many1Satisfy |>> fun s -> s.ToLower()
        let private quoted_string = between (pchar '"') (pchar '"') ("\"" |> isNoneOf |> many1Satisfy)
        let private word = string |>> String
        let private quoted = quoted_string |>> fun s -> String <| s.ToLower()
        let private equals = string .>>. (pchar '=' >>. (string <|> quoted_string)) |>> Equals
        let private less = string .>>. (pchar '<' >>. pfloat) |>> LessThan
        let private more = string .>>. (pchar '>' >>. pfloat) |>> MoreThan
        let private filter = sepBy (attempt equals <|> attempt less <|> attempt more <|> quoted <|> word) spaces1 .>> spaces

        let parse (str: string) =
            match run filter (str.Trim()) with
            | Success (x, _, _) -> x
            | Failure (f, _, _) -> [Impossible]

        let private _f (filter: Filter) (c: CachedChart) : bool =
            let s = (c.Title + " " + c.Artist + " " + c.Creator + " " + c.DiffName + " " + c.Pack).ToLower()
            List.forall
                (
                    function
                    | Impossible -> false
                    | String str -> s.Contains str
                    | Equals ("k", n)
                    | Equals ("key", n)
                    | Equals ("keys", n) -> c.Keys.ToString() = n
                    | Equals ("c", str)
                    | Equals ("comment", str) -> has_comment (c, str)
                    | MoreThan ("d", d)
                    | MoreThan ("diff", d) -> c.Physical > d
                    | LessThan ("d", d)
                    | LessThan ("diff", d) -> c.Physical < d
                    | MoreThan ("l", l)
                    | MoreThan ("length", l) -> float (c.Length / 1000.0f<ms>) > l
                    | LessThan ("l", l)
                    | LessThan ("length", l) -> float (c.Length / 1000.0f<ms>) < l
                    | _ -> true
                ) filter

        let apply (filter: Filter) (charts: CachedChart seq) = Seq.filter (_f filter) charts

        let applyf (filter: Filter) (charts: (CachedChart * Collections.LibraryContext) seq) = Seq.filter (fun (c, _) -> _f filter c) charts