namespace Prelude.Data.Library

open System
open Percyqaz.Common
open Prelude.Charts.Processing.Patterns
open Prelude.Gameplay
open Prelude.Data.User
open Prelude.Data.Library.Collections

[<AutoOpen>]
module internal Shared =

    let first_character (s: string) =
        if s.Length = 0 then
            "?"
        elif Char.IsAsciiLetterOrDigit s.[0] then
            s.[0].ToString().ToUpper()
        else
            "?"

    let format_date_last_played (cc: ChartMeta, ctx: LibraryViewContext) =
        let now = Timestamp.now ()
        let ONE_DAY = 24L * 3600_000L

        let days_ago =
            (now - (UserDatabase.get_chart_data cc.Hash ctx.UserDatabase).LastPlayed) / ONE_DAY

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

    let format_date_added (c: ChartMeta, _) =
        let days_ago = (DateTime.Today - Timestamp.to_datetime c.DateAdded).TotalDays

        if days_ago < 1 then 0, "Today"
        elif days_ago < 2 then 1, "Yesterday"
        elif days_ago < 7 then 2, "This week"
        elif days_ago < 30 then 3, "This month"
        elif days_ago < 60 then 4, "A month ago"
        elif days_ago < 90 then 5, "2 months ago"
        elif days_ago < 120 then 6, "3 months ago"
        elif days_ago < 210 then 7, "6 months ago"
        else 8, "A long time ago"

    let grade_achieved (cc: ChartMeta, ctx: LibraryViewContext) =
        let data = UserDatabase.get_chart_data cc.Hash ctx.UserDatabase

        match 
            data.PersonalBests
            |> Bests.ruleset_best_above ctx.RulesetId (_.Grade) ctx.Rate
        with
        | Some (i, _, _) -> i, ctx.Ruleset.GradeName i
        | None -> -2, "No grade achieved"

    let lamp_achieved (cc: ChartMeta, ctx: LibraryViewContext) =
        let data = UserDatabase.get_chart_data cc.Hash ctx.UserDatabase

        match 
            data.PersonalBests
            |> Bests.ruleset_best_above ctx.RulesetId (_.Lamp) ctx.Rate
        with
        | Some (i, _, _) -> i, ctx.Ruleset.LampName i
        | None -> -2, "No lamp achieved"

    let has_pattern (pattern: string) (cc: ChartMeta, ctx: LibraryViewContext) =
        let report = cc.Patterns
        report.Category.Category.Contains(pattern, StringComparison.OrdinalIgnoreCase)
        || (report.Category.MajorFeatures |> List.exists (fun f -> f.Contains(pattern, StringComparison.OrdinalIgnoreCase)))
        || (report.Category.MinorFeatures |> List.exists (fun f -> f.Contains(pattern, StringComparison.OrdinalIgnoreCase)))
    
    let below_ln_percent (threshold: float32) (cc: ChartMeta, ctx: LibraryViewContext) =
        cc.Patterns.LNPercent < threshold

    let above_ln_percent (threshold: float32) (cc: ChartMeta, ctx: LibraryViewContext) =
        cc.Patterns.LNPercent > threshold

    let has_sv (cc: ChartMeta, ctx: LibraryViewContext) =
        cc.Patterns.SVAmount > Categorise.SV_AMOUNT_THRESHOLD

[<RequireQualifiedAccess>]
[<NoEquality>]
[<NoComparison>]
type LibraryContext =
    | None
    | Pack of name: string
    | Table of level: int
    | Folder of id: string
    | Playlist of index: int * id: string * data: PlaylistEntryInfo

    member this.Matches(other: LibraryContext) =
        match this, other with
        | None, None -> true
        | Pack p, Pack p2 when p = p2 -> true
        | Table _, Table _ -> true
        | Folder f, Folder f2 when f = f2 -> true
        | Playlist (i, id, _), Playlist (i2, id2, _) when i = i2 && id = id2 -> true
        | _ -> false
    member this.SoftMatches(other: LibraryContext) =
        match this, other with
        | None, _ -> true
        | Pack _, None -> true
        | Table _, None -> true
        | Folder _, None -> true
        | _ -> this.Matches other


[<RequireQualifiedAccess>]
type LibraryGroupContext =
    | None
    | Pack of name: string
    | Table of level: int
    | Folder of id: string
    | Playlist of id: string