namespace Prelude.Data.Library

open System
open Percyqaz.Common
open Prelude.Calculator.Patterns
open Prelude.Data.User
open Prelude.Data.Library

[<AutoOpen>]
module internal Shared =

    let first_character (s: string) =
        if s.Length = 0 then
            "?"
        elif Char.IsAsciiLetterOrDigit s.[0] then
            s.[0].ToString().ToUpper()
        else
            "?"

    // A new day starts at 4am, your machine local time
    let days_ago (timestamp: int64) : int =
        let local_date_now =
            DateTime.Now
                .Subtract(TimeSpan.FromHours(4.0))
                .Date
        let local_date =
            Timestamp.to_datetime(timestamp)
                .ToLocalTime()
                .Subtract(TimeSpan.FromHours(4.0))
                .Date
        (local_date_now - local_date).TotalDays |> floor |> int

    let format_date_last_played (chart_meta: ChartMeta, ctx: LibraryViewContext) : int * string =
        let days_ago = days_ago (UserDatabase.get_chart_data chart_meta.Hash ctx.UserDatabase).LastPlayed

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
        if stars >= 15 then
            15, "15+ Stars"
        else
            stars, sprintf "%i Stars" stars

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
        let data = UserDatabase.get_chart_data chart_meta.Hash ctx.UserDatabase

        match
            data.PersonalBests
            |> Bests.ruleset_best_above ctx.RulesetId (_.Grade) ctx.Rate
        with
        | Some (i, _, _) -> i, ctx.Ruleset.GradeName i
        | None -> -2, "No grade achieved"

    let lamp_achieved (chart_meta: ChartMeta, ctx: LibraryViewContext) : int * string  =
        let data = UserDatabase.get_chart_data chart_meta.Hash ctx.UserDatabase

        match
            data.PersonalBests
            |> Bests.ruleset_best_above ctx.RulesetId (_.Lamp) ctx.Rate
        with
        | Some (i, _, _) -> i, ctx.Ruleset.LampName i
        | None -> -2, "No lamp achieved"

    let below_ln_percent (threshold: float32) (chart_meta: ChartMeta, _: LibraryViewContext) : bool =
        chart_meta.Patterns.LNPercent < threshold

    let above_ln_percent (threshold: float32) (chart_meta: ChartMeta, _: LibraryViewContext) : bool =
        chart_meta.Patterns.LNPercent > threshold

    let has_sv (chart_meta: ChartMeta, _: LibraryViewContext) =
        chart_meta.Patterns.SVAmount > Categorise.SV_AMOUNT_THRESHOLD

[<RequireQualifiedAccess>]
[<NoEquality>]
[<NoComparison>]
type LibraryContext =
    | None
    | Pack of name: string
    | Table of level: int
    | Likes
    | Folder of id: string
    | Playlist of index: int * id: string * data: PlaylistEntryInfo

    member this.Matches(other: LibraryContext) : bool =
        match this, other with
        | None, None -> true
        | None, Pack _ -> true
        | Pack p, Pack p2 when p = p2 -> true
        | Pack _, None -> true
        | Table _, Table _ -> true
        | Likes, Likes -> true
        | Folder f, Folder f2 when f = f2 -> true
        | Playlist (i, id, _), Playlist (i2, id2, _) when i = i2 && id = id2 -> true
        | _ -> false
    member this.SoftMatches(other: LibraryContext) : bool =
        match this, other with
        | None, _ -> true
        | Pack _, None -> true
        | Table _, None -> true
        | Folder _, None -> true
        | Folder _, Pack _ -> true
        | Likes, None -> true
        | Likes, Pack _ -> true
        | _ -> this.Matches other

[<RequireQualifiedAccess>]
type LibraryGroupContext =
    | None
    | Pack of name: string
    | Table of level: int
    | Likes
    | Folder of id: string
    | Playlist of id: string