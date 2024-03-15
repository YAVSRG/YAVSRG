namespace Prelude.Data.Library

open System
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Gameplay
open Prelude.Backbeat
open Prelude.Data.Library.Collections
open Prelude.Charts.Processing.Patterns
open Prelude.Data

// todo: make into separate Grouping, Filtering and Sorting modules
module Sorting =

    open Prelude.Data.Library.Caching
    open FParsec

    type LibraryViewContext =
        {
            Rate: float32
            RulesetId: string
            Ruleset: Ruleset
            Library: Library
            ScoreDatabase: ScoreDatabase
        }

    let private first_character (s: string) =
        if s.Length = 0 then
            "?"
        elif Char.IsLetterOrDigit s.[0] then
            s.[0].ToString().ToUpper()
        else
            "?"

    let private format_date_last_played (cc: CachedChart, ctx: LibraryViewContext) =
        let now = Timestamp.now()
        let ONE_DAY = 24L * 3600_000L
        let days_ago = (now - (ScoreDatabase.get cc.Hash ctx.ScoreDatabase).LastPlayed) / ONE_DAY

        if days_ago < 1 then 0, "Today"
        elif days_ago < 2 then 1, "Yesterday"
        elif days_ago < 7 then 2, "This week"
        elif days_ago < 30 then 3, "This month"
        elif days_ago < 60 then 4, "A month ago"
        elif days_ago < 90 then 5, "2 months ago"
        elif days_ago < 120 then 6, "3 months ago"
        elif days_ago < 210 then 7, "6 months ago"
        elif days_ago < 3600 then 8, "A long time ago"
        else 9, "Never"

    let private format_date_added (c: CachedChart, _) =
        let days_ago = (DateTime.Today - c.DateAdded).TotalDays

        if days_ago < 1 then 0, "Today"
        elif days_ago < 2 then 1, "Yesterday"
        elif days_ago < 7 then 2, "This week"
        elif days_ago < 30 then 3, "This month"
        elif days_ago < 60 then 4, "A month ago"
        elif days_ago < 90 then 5, "2 months ago"
        elif days_ago < 120 then 6, "3 months ago"
        elif days_ago < 210 then 7, "6 months ago"
        else 8, "A long time ago"

    let grade_achieved (cc: CachedChart, ctx: LibraryViewContext) =
        let data = ScoreDatabase.get cc.Hash ctx.ScoreDatabase
        if data.PersonalBests.ContainsKey ctx.RulesetId then
            match PersonalBests.get_best_above ctx.Rate data.PersonalBests.[ctx.RulesetId].Grade with
            | Some i -> i, ctx.Ruleset.GradeName i
            | None -> -2, "No grade achieved"
        else
            -2, "No grade achieved"

    let lamp_achieved (cc: CachedChart, ctx: LibraryViewContext) =
        let data = ScoreDatabase.get cc.Hash ctx.ScoreDatabase
        if data.PersonalBests.ContainsKey ctx.RulesetId then
            match PersonalBests.get_best_above ctx.Rate data.PersonalBests.[ctx.RulesetId].Lamp with
            | Some i -> i, ctx.Ruleset.LampName i
            | None -> -2, "No lamp achieved"
        else
            -2, "No lamp achieved"

    type GroupMethod = CachedChart * LibraryViewContext -> int * string

    let grouping_modes: IDictionary<string, GroupMethod> =
        dict
            [
                "none", (fun (c, _) -> 0, "No grouping")
                "pack", (fun (c, _) -> 0, c.Folder)
                "date_played", format_date_last_played
                "date_installed", format_date_added
                "grade", grade_achieved
                "lamp", lamp_achieved
                "title", (fun (c, _) -> 0, first_character c.Title)
                "artist", (fun (c, _) -> 0, first_character c.Artist)
                "creator", (fun (c, _) -> 0, first_character c.Creator)
                "keymode", (fun (c, _) -> c.Keys, c.Keys.ToString() + "K")
                "patterns",
                fun (c, ctx) ->
                    match ctx.Library.Patterns.TryGetValue(c.Hash) with
                    | true, report -> 0, report.Category
                    | false, _ -> -1, "Not analysed"
            ]

    let private has_comment (query: string) (cc: CachedChart, ctx: LibraryViewContext) =
        let comment = (ScoreDatabase.get cc.Hash ctx.ScoreDatabase).Comment
        not (String.IsNullOrEmpty comment) && comment.Contains(query, StringComparison.OrdinalIgnoreCase)

    let private has_pattern (pattern: string) (cc: CachedChart, ctx: LibraryViewContext) =
        if ctx.Library.Patterns.ContainsKey cc.Hash then
            let p = ctx.Library.Patterns.[cc.Hash].Patterns

            let pattern_to_find =
                match pattern.ToLower() with
                | "streams"
                | "speed"
                | "stream" -> Stream
                | "jumpstream"
                | "js"
                | "handstream"
                | "hs"
                | "cs" -> Chordstream
                | "jacks"
                | "chords"
                | "chordjack"
                | "chordjacks"
                | "cj" -> Jack
                | _ -> Stream

            let matching =
                p
                |> Seq.where (fun x -> x.Pattern = pattern_to_find)
                |> Seq.sumBy (fun x -> x.Amount)

            let total = p |> Seq.sumBy (fun x -> x.Amount)
            matching / total > 0.4f

        else
            false

    let private compare_by (f: CachedChart -> IComparable) =
        fun a b -> f(fst a).CompareTo <| f (fst b)

    let private then_compare_by (f: CachedChart -> IComparable) cmp =
        let cmp2 = compare_by f

        fun a b ->
            match cmp a b with
            | 0 -> cmp2 a b
            | x -> x

    type SortMethod = Comparison<CachedChart * Collections.LibraryContext>

    let sorting_modes: IDictionary<string, SortMethod> =
        dict
            [
                "difficulty", Comparison(compare_by (fun x -> x.Physical))
                "bpm",
                Comparison(
                    compare_by (fun x -> let (a, b) = x.BPM in (1f / a, 1f / b))
                    |> then_compare_by (fun x -> x.Physical)
                )
                "title", Comparison(compare_by (fun x -> x.Title) |> then_compare_by (fun x -> x.Physical))
                "artist", Comparison(compare_by (fun x -> x.Artist) |> then_compare_by (fun x -> x.Physical))
                "creator", Comparison(compare_by (fun x -> x.Creator) |> then_compare_by (fun x -> x.Physical))
            ]

    type FilterPart =
        | Equals of string * string
        | LessThan of string * float
        | MoreThan of string * float
        | String of string
        | Impossible

    type Filter = FilterPart list

    module Filter =

        let private string = " =<>\"" |> isNoneOf |> many1Satisfy |>> fun s -> s.ToLower()

        let private quoted_string =
            between (pchar '"') (pchar '"') ("\"" |> isNoneOf |> many1Satisfy)

        let private word = string |>> String
        let private quoted = quoted_string |>> fun s -> String <| s.ToLower()

        let private equals =
            string .>>. (pchar '=' >>. (string <|> quoted_string)) |>> Equals

        let private less = string .>>. (pchar '<' >>. pfloat) |>> LessThan
        let private more = string .>>. (pchar '>' >>. pfloat) |>> MoreThan

        let private filter =
            sepBy (attempt equals <|> attempt less <|> attempt more <|> quoted <|> word) spaces1
            .>> spaces

        let parse (str: string) =
            match run filter (str.Trim()) with
            | Success(x, _, _) -> x
            | Failure(f, _, _) -> [ Impossible ]

        let private apply (filter: Filter, ctx: LibraryViewContext) (cc: CachedChart) : bool =
            let s =
                (cc.Title
                 + " "
                 + cc.Artist
                 + " "
                 + cc.Creator
                 + " "
                 + cc.DifficultyName
                 + " "
                 + (cc.Subtitle |> Option.defaultValue "")
                 + " "
                 + cc.Folder)
                    .ToLower()

            List.forall
                (function
                | Impossible -> false
                | String str -> s.Contains str
                | Equals("k", n)
                | Equals("key", n)
                | Equals("keys", n) -> cc.Keys.ToString() = n
                | Equals("c", str)
                | Equals("comment", str) -> has_comment str (cc, ctx)
                | Equals("p", str) -> has_pattern str (cc, ctx)
                | Equals("pattern", str) -> has_pattern str (cc, ctx)
                | MoreThan("d", d)
                | MoreThan("diff", d) -> cc.Physical > d
                | LessThan("d", d)
                | LessThan("diff", d) -> cc.Physical < d
                | MoreThan("l", l)
                | MoreThan("length", l) -> float (cc.Length / 1000.0f<ms>) > l
                | LessThan("l", l)
                | LessThan("length", l) -> float (cc.Length / 1000.0f<ms>) < l
                | _ -> true)
                filter

        let apply_seq (filter: Filter, ctx: LibraryViewContext) (charts: CachedChart seq) = Seq.filter (apply (filter, ctx)) charts

        let apply_ctx_seq (filter: Filter, ctx: LibraryViewContext) (charts: (CachedChart * LibraryContext) seq) =
            Seq.filter (fun (cc, _) -> apply (filter, ctx) cc) charts

    [<RequireQualifiedAccess; Json.AutoCodec>]
    type LibraryMode =
        | All
        | Collections
        | Table

    type Group =
        {
            Charts: ResizeArray<CachedChart * LibraryContext>
            Context: LibraryGroupContext
        }

    type LexSortedGroups = Dictionary<int * string, Group>

    let get_groups
        (filter_by: Filter)
        (group_by: GroupMethod)
        (sort_by: SortMethod)
        (ctx: LibraryViewContext)
        : LexSortedGroups =

        let groups = new Dictionary<int * string, Group>()

        for cc in Filter.apply_seq (filter_by, ctx) ctx.Library.Cache.Entries.Values do
            let s = group_by (cc, ctx)

            if groups.ContainsKey s |> not then
                groups.Add(
                    s,
                    {
                        Charts = ResizeArray<CachedChart * LibraryContext>()
                        Context = LibraryGroupContext.None
                    }
                )

            groups.[s].Charts.Add(cc, LibraryContext.None)

        for g in groups.Keys |> Seq.toArray do
            groups.[g] <-
                { groups.[g] with
                    Charts = groups.[g].Charts |> Seq.distinctBy (fun (cc, _) -> cc.Hash) |> ResizeArray
                }

            groups.[g].Charts.Sort sort_by

        groups

    let get_collection_groups (filter_by: Filter) (sort_by: SortMethod) (ctx: LibraryViewContext) : LexSortedGroups =

        let groups = new Dictionary<int * string, Group>()

        for name in ctx.Library.Collections.Folders.Keys do
            let collection = ctx.Library.Collections.Folders.[name]

            collection.Charts
            |> Seq.choose (fun entry ->
                match Cache.by_key entry.Path ctx.Library.Cache with
                | Some cc -> Some(cc, LibraryContext.Folder name)
                | None ->

                match Cache.by_hash entry.Hash ctx.Library.Cache with
                | Some cc ->
                    entry.Path <- cc.Key
                    Some(cc, LibraryContext.Folder name)
                | None ->
                    //Logging.Warn(sprintf "Could not find chart: %s [%s] for collection %s" entry.Path entry.Hash name)
                    None
            )
            |> Filter.apply_ctx_seq (filter_by, ctx)
            |> ResizeArray<CachedChart * LibraryContext>
            |> fun x -> x.Sort sort_by; x
            |> fun x ->
                if x.Count > 0 then
                    groups.Add(
                        (0, name),
                        {
                            Charts = x
                            Context = LibraryGroupContext.Folder name
                        }
                    )

        for name in ctx.Library.Collections.Playlists.Keys do
            let playlist = ctx.Library.Collections.Playlists.[name]

            playlist.Charts
            |> Seq.indexed
            |> Seq.choose (fun (i, (entry, info)) ->
                match Cache.by_key entry.Path ctx.Library.Cache with
                | Some cc -> Some(cc, LibraryContext.Playlist(i, name, info))
                | None ->

                match Cache.by_key entry.Hash ctx.Library.Cache with
                | Some cc ->
                    entry.Path <- cc.Key
                    Some(cc, LibraryContext.Playlist(i, name, info))
                | None ->
                    //Logging.Warn(sprintf "Could not find chart: %s [%s] for playlist %s" entry.Path entry.Hash name)
                    None
            )
            |> Filter.apply_ctx_seq (filter_by, ctx)
            |> ResizeArray<CachedChart * LibraryContext>
            |> fun x ->
                if x.Count > 0 then
                    groups.Add(
                        (0, name),
                        {
                            Charts = x
                            Context = LibraryGroupContext.Playlist name
                        }
                    )

        groups

    let get_table_groups (filter_by: Filter) (sort_by: SortMethod) (table: Table) (ctx: LibraryViewContext) : LexSortedGroups =
        let groups = new Dictionary<int * string, Group>()
        for level, charts in table.Charts |> Seq.groupBy (fun x -> x.Level) do
            charts
            |> Seq.choose (fun (c: TableChart) ->
                match Cache.by_key (sprintf "%s/%s" table.Info.Name c.Hash) ctx.Library.Cache with
                | Some cc -> Some(cc, LibraryContext.Table level)
                | None ->
                    Cache.by_hash c.Hash ctx.Library.Cache
                    |> Option.map (fun x -> x, LibraryContext.Table level)
            )
            |> Filter.apply_ctx_seq (filter_by, ctx)
            |> ResizeArray<CachedChart * LibraryContext>
            |> fun x -> x.Sort sort_by; x
            |> fun x ->
                if x.Count > 0 then
                    groups.Add(
                        (level, table.Info.LevelDisplayNames.TryFind level |> Option.defaultValue (level.ToString())),
                        {
                            Charts = x
                            Context = LibraryGroupContext.Table level
                        }
                    )
        groups

    let get_empty_view () = new Dictionary<int * string, Group>()
