namespace Prelude.Data.Charts

open System
open System.Collections.Generic
open Percyqaz.Json
open Percyqaz.Common
open Prelude
open Prelude.Gameplay
open Prelude.Data.Charts.Tables
open Prelude.Data.Charts.Collections
open Prelude.Charts.Tools.Patterns

module Sorting =

    open Prelude.Data.Charts.Caching
    open FParsec

    let private firstCharacter (s: string) =
        if s.Length = 0 then "?"
        elif Char.IsLetterOrDigit s.[0] then s.[0].ToString().ToUpper()
        else "?"

    let dateLastPlayed (c: CachedChart, _) =
        match Data.Scores.Scores.getData c.Hash with
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
        
    let dateInstalled (c: CachedChart, _) =
        let daysAgo = (DateTime.Today - c.DateAdded).TotalDays
        if daysAgo < 0 then 0, "Today"
        elif daysAgo < 1 then 1, "Yesterday"
        elif daysAgo < 7 then 2, "This week"
        elif daysAgo < 30 then 3, "This month"
        elif daysAgo < 60 then 4, "A month ago"
        elif daysAgo < 90 then 5, "2 months ago"
        elif daysAgo < 120 then 6, "3 months ago"
        elif daysAgo < 210 then 7, "6 months ago"
        else 8, "A long time ago"

    type GroupContext = { Rate: float32; RulesetId: string; Ruleset: Ruleset }

    let has_comment (c: CachedChart, comment: string) =
        match Data.Scores.Scores.getData c.Hash with
        | Some d -> d.Comment.Contains(comment, StringComparison.OrdinalIgnoreCase)
        | None -> false

    let has_pattern (c: CachedChart, pattern: string) =
        if Library.patterns.ContainsKey c.Hash then
            let p = Library.patterns.[c.Hash]
            let pattern_to_find =
                match pattern.ToLower() with
                | "streams"
                | "stream" -> Stream "Streams"
                | "jumpstream"
                | "js" -> Stream "Jumpstream"
                | "handstream"
                | "hs" -> Stream "Handstream"
                | "jacks" -> Jack "Jacks"
                | "chords"
                | "chordjack"
                | "chordjacks"
                | "cj" -> Jack "Chordjacks"
                | "doublestream"
                | "doublestreams"
                | "ds" -> Stream "Double streams"
                | "chordstream"
                | "chordstreams"
                | "cs" -> Stream "Dense chordstream"
                | _ -> Stream ""
            let matching = p |> Seq.where(fun x -> x.Pattern = pattern_to_find) |> Seq.sumBy(fun x -> x.Score)
            let total = p |> Seq.sumBy(fun x -> x.Score)
            matching / total > 0.4f

        else false

    let gradeAchieved (c: CachedChart, ctx: GroupContext) =
        match Data.Scores.Scores.getData c.Hash with
        | Some d ->
            if d.PersonalBests.ContainsKey ctx.RulesetId then
                match PersonalBests.get_best_above ctx.Rate d.PersonalBests.[ctx.RulesetId].Grade with
                | Some i -> i, ctx.Ruleset.GradeName i
                | None -> -2, "No grade achieved"
            else -2, "No grade achieved"
        | None -> -2, "No grade achieved"

    let lampAchieved (c: CachedChart, ctx: GroupContext) =
        match Data.Scores.Scores.getData c.Hash with
        | Some d ->
            if d.PersonalBests.ContainsKey ctx.RulesetId then
                match PersonalBests.get_best_above ctx.Rate d.PersonalBests.[ctx.RulesetId].Lamp with
                | Some i -> i, ctx.Ruleset.LampName i
                | None -> -2, "No lamp achieved"
            else -2, "No lamp achieved"
        | None -> -2, "No lamp achieved"

    type GroupMethod = CachedChart * GroupContext -> int * string
    let groupBy : IDictionary<string, GroupMethod> = dict [
            "none", fun (c, _) -> 0, "No grouping"
            "pack", fun (c, _) -> 0, c.Folder
            "date_played", dateLastPlayed
            "date_installed", dateInstalled
            "grade", gradeAchieved
            "lamp", lampAchieved
            "title", fun (c, _) -> 0, firstCharacter c.Title
            "artist", fun (c, _) -> 0, firstCharacter c.Artist
            "creator", fun (c, _) -> 0, firstCharacter c.Creator
            "keymode", fun (c, _) -> c.Keys, c.Keys.ToString() + "K"
            "patterns", fun (c, _) -> 
                match Library.patterns.TryGetValue(c.Hash) with
                | true, report -> 0, Patterns.categorise_chart report
                | false, _ -> -1, "Not analysed"
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
            let s = (c.Title + " " + c.Artist + " " + c.Creator + " " + c.DifficultyName + " " + c.Folder).ToLower()
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
                    | Equals ("p", str) -> has_pattern (c, str)
                    | Equals ("pattern", str) -> has_pattern (c, str)
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

        let applyf (filter: Filter) (charts: (CachedChart * LibraryContext) seq) = Seq.filter (fun (c, _) -> _f filter c) charts

    type [<RequireQualifiedAccess; Json.AutoCodec>] LibraryMode = All | Collections | Table
    type Group = { Charts: ResizeArray<CachedChart * LibraryContext>; Context: LibraryGroupContext }
    type LexSortedGroups = Dictionary<int * string, Group>
    
    let getGroups (ctx: GroupContext) (grouping: GroupMethod) (sorting: SortMethod) (filter: Filter) : LexSortedGroups =
    
        let groups = new Dictionary<int * string, Group>()
    
        for c in Filter.apply filter Library.cache.Entries.Values do
            let s = grouping (c, ctx)
            if groups.ContainsKey s |> not then groups.Add(s, { Charts = ResizeArray<CachedChart * LibraryContext>(); Context = LibraryGroupContext.None })
            groups.[s].Charts.Add (c, LibraryContext.None)
    
        for g in groups.Keys |> Seq.toArray do
            groups.[g] <- { groups.[g] with Charts = groups.[g].Charts |> Seq.distinctBy (fun (cc, _) -> cc.Hash) |> ResizeArray }
            groups.[g].Charts.Sort sorting
    
        groups
    
    let getCollectionGroups (sorting: SortMethod) (filter: Filter) : LexSortedGroups =
    
        let groups = new Dictionary<int * string, Group>()
    
        for name in Library.collections.Folders.Keys do
            let collection = Library.collections.Folders.[name]
            collection.Charts
            |> Seq.choose
                ( fun entry ->
                    match Cache.by_key entry.Path Library.cache with
                    | Some cc -> Some (cc, LibraryContext.Folder name)
                    | None ->
                    match Cache.by_hash entry.Hash Library.cache with
                    | Some cc -> entry.Path <- cc.Key; Some (cc, LibraryContext.Folder name)
                    | None -> Logging.Warn(sprintf "Could not find chart: %s [%s] for collection %s" entry.Path entry.Hash name); None
                )
            |> Filter.applyf filter
            |> ResizeArray<CachedChart * LibraryContext>
            |> fun x -> x.Sort sorting; x
            |> fun x -> if x.Count > 0 then groups.Add((0, name), { Charts = x; Context = LibraryGroupContext.Folder name })
    
        for name in Library.collections.Playlists.Keys do
            let playlist = Library.collections.Playlists.[name]
            playlist.Charts
            |> Seq.indexed
            |> Seq.choose
                ( fun (i, (entry, info)) ->
                    match Cache.by_key entry.Path Library.cache with
                    | Some cc -> Some (cc, LibraryContext.Playlist (i, name, info))
                    | None ->
                    match Cache.by_key entry.Hash Library.cache with
                    | Some cc -> entry.Path <- cc.Key; Some (cc, LibraryContext.Playlist (i, name, info))
                    | None -> Logging.Warn(sprintf "Could not find chart: %s [%s] for playlist %s" entry.Path entry.Hash name); None
                )
            |> Filter.applyf filter
            |> ResizeArray<CachedChart * LibraryContext>
            |> fun x -> if x.Count > 0 then groups.Add((0, name), { Charts = x; Context = LibraryGroupContext.Playlist name })
    
        groups
    
    let getTableGroups (sorting: SortMethod) (filter: Filter) : LexSortedGroups =
        let groups = new Dictionary<int * string, Group>()
        match Table.current() with
        | Some table ->
            for level_no, level in Seq.indexed table.Levels do
                level.Charts
                |> Seq.choose
                    ( fun (c: TableChart) ->
                        match Cache.by_key (sprintf "%s/%s" table.Name c.Hash) Library.cache with
                        | Some cc -> Some (cc, LibraryContext.Table level.Name)
                        | None ->
                        Cache.by_hash c.Hash Library.cache
                        |> Option.map (fun x -> x, LibraryContext.Table level.Name)
                    )
                |> Filter.applyf filter
                |> ResizeArray<CachedChart * LibraryContext>
                |> fun x -> x.Sort sorting; x
                |> fun x -> if x.Count > 0 then groups.Add((level_no, level.Name), { Charts = x; Context = LibraryGroupContext.Table level.Name })
        | None -> ()
        groups