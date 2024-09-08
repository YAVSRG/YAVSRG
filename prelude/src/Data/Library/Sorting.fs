namespace Prelude.Data.Library

open System
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Charts.Processing.Patterns
open Prelude.Gameplay
open Prelude.Backbeat
open Prelude.Data.Library.Collections
open Prelude.Data.Library.Caching
open Prelude.Data

module Sorting =

    let private first_character (s: string) =
        if s.Length = 0 then
            "?"
        elif Char.IsLetterOrDigit s.[0] then
            s.[0].ToString().ToUpper()
        else
            "?"

    let private format_date_last_played (cc: CachedChart, ctx: LibraryViewContext) =
        let now = Timestamp.now ()
        let ONE_DAY = 24L * 3600_000L

        let days_ago =
            (now - (ScoreDatabase.get cc.Hash ctx.ScoreDatabase).LastPlayed) / ONE_DAY

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

        match 
            data.PersonalBests
            |> Bests.ruleset_best_above ctx.RulesetId (_.Grade) ctx.Rate
        with
        | Some (i, _, _) -> i, ctx.Ruleset.GradeName i
        | None -> -2, "No grade achieved"

    let lamp_achieved (cc: CachedChart, ctx: LibraryViewContext) =
        let data = ScoreDatabase.get cc.Hash ctx.ScoreDatabase

        match 
            data.PersonalBests
            |> Bests.ruleset_best_above ctx.RulesetId (_.Lamp) ctx.Rate
        with
        | Some (i, _, _) -> i, ctx.Ruleset.LampName i
        | None -> -2, "No lamp achieved"

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
                fun (cc, ctx) ->
                    match Cache.patterns_by_hash cc.Hash ctx.Library.Cache with
                    | None -> -1, "Not analysed"
                    | Some report -> 0, report.Category.Category
            ]

    let private has_comment (query: string) (cc: CachedChart, ctx: LibraryViewContext) =
        let comment = (ScoreDatabase.get cc.Hash ctx.ScoreDatabase).Comment

        not (String.IsNullOrEmpty comment)
        && comment.Contains(query, StringComparison.OrdinalIgnoreCase)

    let private has_pattern (pattern: string) (cc: CachedChart, ctx: LibraryViewContext) =
        match Cache.patterns_by_hash cc.Hash ctx.Library.Cache with
        | Some report -> 
            report.Category.Category.Contains(pattern, StringComparison.OrdinalIgnoreCase)
            || (report.Category.MajorFeatures |> List.exists (fun f -> f.Contains(pattern, StringComparison.OrdinalIgnoreCase)))
            || (report.Category.MinorFeatures |> List.exists (fun f -> f.Contains(pattern, StringComparison.OrdinalIgnoreCase)))
        | None -> false
    
    let private below_ln_percent (threshold: float) (cc: CachedChart, ctx: LibraryViewContext) =
        match Cache.patterns_by_hash cc.Hash ctx.Library.Cache with
        | Some report -> report.LNPercent < float32 threshold
        | None -> false

    let private above_ln_percent (threshold: float) (cc: CachedChart, ctx: LibraryViewContext) =
        match Cache.patterns_by_hash cc.Hash ctx.Library.Cache with
        | Some report -> report.LNPercent > float32 threshold
        | None -> false

    let private has_sv (cc: CachedChart, ctx: LibraryViewContext) =
        match Cache.patterns_by_hash cc.Hash ctx.Library.Cache with
        | Some report -> report.SVAmount > PatternSummary.SV_AMOUNT_THRESHOLD
        | None -> false

    type SortingTag = string * float32 * float
    type SortMethod = CachedChart * LibraryViewContext -> SortingTag

    let sorting_modes: IDictionary<string, SortMethod> =
        dict
            [
                "difficulty", fun (x, _) -> "", 0.0f, x.Physical
                "bpm", fun (x, _) -> "", (60000.0f<ms/minute> / snd x.BPM * 10.0f |> float32), x.Physical
                "title", fun (x, _) -> x.Title.ToLowerInvariant(), 0.0f, x.Physical
                "artist", fun (x, _) -> x.Artist.ToLowerInvariant(), 0.0f, x.Physical
                "creator", fun (x, _) -> x.Creator.ToLowerInvariant(), 0.0f, x.Physical
                "length", fun (x, _) -> "", (float32 x.Length), x.Physical
                "date_installed", fun (x, _) -> "", (Timestamp.from_datetime x.DateAdded |> float32), x.Physical
                "date_played", fun (x, ctx) -> 
                    let date_played =
                         match ScoreDatabase.get_cached x.Hash ctx.ScoreDatabase with
                         | Some d -> d.LastPlayed |> float32
                         | None -> 0.0f
                    "", date_played, x.Physical
                "grade", fun (x, ctx) ->
                    match 
                        (ScoreDatabase.get x.Hash ctx.ScoreDatabase).PersonalBests
                        |> Bests.ruleset_best_above ctx.RulesetId (_.Grade) ctx.Rate
                    with
                    | Some (i, _, _) -> "", float32 i, x.Physical
                    | None -> "", -2.0f, x.Physical
                "lamp", fun (x, ctx) ->
                    match 
                        (ScoreDatabase.get x.Hash ctx.ScoreDatabase).PersonalBests
                        |> Bests.ruleset_best_above ctx.RulesetId (_.Lamp) ctx.Rate
                    with
                    | Some (i, _, _) -> "", float32 i, x.Physical
                    | None -> "", -2.0f, x.Physical
            ]

    module Filter =

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
                | NotString str -> s.Contains str |> not

                | Equals("k", n)
                | Equals("key", n)
                | Equals("keys", n) -> cc.Keys.ToString() = n

                | NotEquals("k", n)
                | NotEquals("key", n)
                | NotEquals("keys", n) -> cc.Keys.ToString() <> n

                | Equals("p", str) -> has_pattern str (cc, ctx)
                | Equals("pattern", str) -> has_pattern str (cc, ctx)

                | NotEquals("p", str) -> has_pattern str (cc, ctx) |> not
                | NotEquals("pattern", str) -> has_pattern str (cc, ctx) |> not

                | MoreThan("d", d)
                | MoreThan("diff", d) -> cc.Physical >= d
                | LessThan("d", d)
                | LessThan("diff", d) -> cc.Physical <= d

                | MoreThan("l", l)
                | MoreThan("length", l) -> float (cc.Length / 1000.0f<ms>) >= l
                | LessThan("l", l)
                | LessThan("length", l) -> float (cc.Length / 1000.0f<ms>) <= l

                | LessThan("ln", pc)
                | LessThan("holds", pc)
                | LessThan("lns", pc) -> below_ln_percent pc (cc, ctx)
                | MoreThan("ln", pc)
                | MoreThan("holds", pc)
                | MoreThan("lns", pc) -> above_ln_percent pc (cc, ctx)

                | Tag "nosv"
                | Tag "nsv" -> not (has_sv (cc, ctx))
                | Tag "sv" -> has_sv (cc, ctx)

                | _ -> true)
                filter

        let apply_seq (filter: Filter, ctx: LibraryViewContext) (charts: CachedChart seq) =
            Seq.filter (apply (filter, ctx)) charts

        let apply_ctx_seq (filter: Filter, ctx: LibraryViewContext) (charts: (CachedChart * 'T) seq) =
            Seq.filter (fun (cc, _) -> apply (filter, ctx) cc) charts

    [<RequireQualifiedAccess; Json.AutoCodec>]
    type LibraryMode =
        | All
        | Collections
        | Table

    type Group =
        {
            Charts: (CachedChart * LibraryContext) array
            Context: LibraryGroupContext
        }

    type private GroupWithSorting =
        {
            Charts: ResizeArray<CachedChart * LibraryContext * SortingTag>
            Context: LibraryGroupContext
        }
        member this.ToGroup : Group =
            {
                Charts = 
                    this.Charts
                    |> Seq.distinctBy (fun (cc, _, _) -> cc.Hash)
                    |> Seq.sortBy (fun (_, _, key) -> key)
                    |> Seq.map (fun (cc, ctx, _) -> (cc, ctx))
                    |> Array.ofSeq
                Context = this.Context
            }

    type LexSortedGroups = Dictionary<int * string, Group>

    let get_groups
        (filter_by: Filter)
        (group_by: GroupMethod)
        (sort_by: SortMethod)
        (ctx: LibraryViewContext)
        : LexSortedGroups =

        let found_groups = new Dictionary<int * string, GroupWithSorting>()

        for cc in Filter.apply_seq (filter_by, ctx) ctx.Library.Cache.Entries.Values do
            let s = group_by (cc, ctx)

            if found_groups.ContainsKey s |> not then
                found_groups.Add(
                    s,
                    {
                        Charts = ResizeArray<CachedChart * LibraryContext * SortingTag>()
                        Context = LibraryGroupContext.None
                    }
                )

            found_groups.[s].Charts.Add(cc, LibraryContext.None, sort_by (cc, ctx))

        let groups = new Dictionary<int * string, Group>()

        for g in found_groups.Keys |> Seq.toArray do
            groups.[g] <- found_groups.[g].ToGroup

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
                    None
            )
            |> Filter.apply_ctx_seq (filter_by, ctx)
            |> Seq.sortBy (fun (cc, _) -> sort_by (cc, ctx))
            |> Array.ofSeq
            |> fun x ->
                if x.Length > 0 then
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
                    None
            )
            |> Filter.apply_ctx_seq (filter_by, ctx)
            |> Array.ofSeq
            |> fun x ->
                if x.Length > 0 then
                    groups.Add(
                        (0, name),
                        {
                            Charts = x
                            Context = LibraryGroupContext.Playlist name
                        }
                    )

        groups

    let get_table_groups
        (filter_by: Filter)
        (sort_by: SortMethod)
        (table: Table)
        (ctx: LibraryViewContext)
        : LexSortedGroups =
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
            |> Seq.sortBy (fun (cc, _) -> sort_by (cc, ctx))
            |> Array.ofSeq
            |> fun x ->
                if x.Length > 0 then
                    groups.Add(
                        (level,
                         table.Info.LevelDisplayNames.TryFind level
                         |> Option.defaultValue (level.ToString())),
                        {
                            Charts = x
                            Context = LibraryGroupContext.Table level
                        }
                    )

        groups

    let get_empty_view () = new Dictionary<int * string, Group>()
