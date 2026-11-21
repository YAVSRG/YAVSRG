namespace Prelude.Data.Library

open System
open System.Collections.Generic
open Percyqaz.Common
open Prelude.Data.UserDatabase

type GroupFunc = ChartMeta * LibraryViewContext -> int * string

type GroupMethod =
    | Normal of GroupFunc
    | Packs
    | Collections
    | Levels

module Grouping =

    let first_character (s: string) : string =
        if s.Length = 0 then "?"
        elif Char.IsAsciiLetterOrDigit(s.[0]) then s.[0].ToString().ToUpper()
        else "?"

    // A new day starts at 4am, your machine local time
    let days_ago (timestamp: int64) : int =
        let local_date_now = DateTime.Now.Subtract(TimeSpan.FromHours(4.0)).Date

        let local_date =
            Timestamp.to_datetime(timestamp).ToLocalTime().Subtract(TimeSpan.FromHours(4.0)).Date

        (local_date_now - local_date).TotalDays |> floor |> int

    let format_date_last_played (chart_meta: ChartMeta, ctx: LibraryViewContext) : int * string =
        let days_ago =
            days_ago (ctx.Library.UserData.GetChartData(chart_meta.Hash)).LastPlayed

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

    let format_difficulty (chart_meta: ChartMeta, _) : int * string =
        let stars = chart_meta.Rating |> floor |> int
        if stars >= 15 then 15, "15+ Stars" else stars, sprintf "%i Stars" stars

    let format_date_added (c: ChartMeta, _) : int * string =
        let days_ago = days_ago c.DateAdded

        if days_ago < 1 then 0, "Today"
        elif days_ago < 2 then 1, "Yesterday"
        elif days_ago < 7 then 2, "This week"
        elif days_ago < 30 then 3, "This month"
        elif days_ago < 60 then 4, "A month ago"
        elif days_ago < 90 then 5, "2 months ago"
        elif days_ago < 120 then 6, "3 months ago"
        elif days_ago < 210 then 7, "6 months ago"
        else 8, "A long time ago"

    let grade_achieved (chart_meta: ChartMeta, ctx: LibraryViewContext) : int * string =
        let data = ctx.Library.UserData.GetChartData(chart_meta.Hash)
        let ruleset_info = ctx.RulesetProvider(chart_meta)

        match data.PersonalBests |> Bests.ruleset_best_above ruleset_info.Hash _.Grade ctx.Rate with
        | Some(i, _, _) -> i, ruleset_info.Ruleset.GradeName(i)
        | None -> -2, "No grade achieved"

    let lamp_achieved (chart_meta: ChartMeta, ctx: LibraryViewContext) : int * string =
        let data = ctx.Library.UserData.GetChartData(chart_meta.Hash)
        let ruleset_info = ctx.RulesetProvider(chart_meta)

        match data.PersonalBests |> Bests.ruleset_best_above ruleset_info.Hash _.Lamp ctx.Rate with
        | Some(i, _, _) -> i, ruleset_info.Ruleset.LampName(i)
        | None -> -2, "No lamp achieved"

    let modes: IDictionary<string, GroupMethod> =
        dict
            [
                "none", Normal(fun (_, _) -> 0, "No grouping")
                "pack", Packs
                "collection", Collections
                "level", Levels
                "difficulty", Normal format_difficulty
                "date_played", Normal format_date_last_played
                "date_installed", Normal format_date_added
                "grade", Normal grade_achieved
                "lamp", Normal lamp_achieved
                "title", Normal(fun (c, _) -> 0, first_character c.Title)
                "artist", Normal(fun (c, _) -> 0, first_character c.Artist)
                "creator", Normal(fun (c, _) -> 0, first_character c.Creator)
                "keymode", Normal(fun (c, _) -> c.Keys, c.Keys.ToString() + "K")
                "patterns", Normal(fun (c, _) -> 0, c.Patterns.Category)
            ]

type Group = { Charts: (ChartMeta * LibraryContext) array; Context: LibraryGroupContext }

type private GroupWithSorting =
    {
        Charts: ResizeArray<ChartMeta * LibraryContext * SortingTag>
        Context: LibraryGroupContext
    }

    member this.ToGroup(reverse_sorting: bool) : Group =
        {
            Charts =
                this.Charts
                |> Seq.sortBy(fun (_, _, key) -> key)
                |> if reverse_sorting then Seq.rev else id
                |> Seq.map(fun (chart_meta, ctx, _) -> (chart_meta, ctx))
                |> Array.ofSeq
            Context = this.Context
        }
