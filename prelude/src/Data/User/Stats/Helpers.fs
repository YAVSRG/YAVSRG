namespace Prelude.Data.User.Stats

open System
open Percyqaz.Common

[<AutoOpen>]
module StatsHelpers =

    let safe_stat_max (original: float) (incoming: float) : float =
        if Double.IsFinite incoming then
            max original incoming
        else
            Logging.Error "Incoming sync value was %f, ignoring" incoming
            original

    let combine_playtimes (op: float -> float -> float) (total: Map<int, float>) (session: Map<int, float>) : Map<int, float> =
        Map.fold
            (fun (pt: Map<int, float>) keymode time ->
                pt.Change(
                    keymode,
                    function
                    | None -> Some time
                    | Some t -> Some (op t time)
                )
            )
            total
            session

    let add_playtimes = combine_playtimes (+)

    let format_long_time (time: float) : string =
        let seconds = time / 1000.0
        let minutes = seconds / 60.0
        let hours = minutes / 60.0
        let days = hours / 24.0

        if days > 1 then
            sprintf "%id %02ih %02im" (floor days |> int) (floor (hours % 24.0) |> int) (floor (minutes % 60.0) |> int)
        elif hours > 1 then
            sprintf "%ih %02im" (floor hours |> int) (floor (minutes % 60.0) |> int)
        else
            sprintf "%im %02is" (floor minutes |> int) (floor (seconds % 60.0) |> int)

    let format_short_time (time: float) : string =
        let seconds = time / 1000.0
        let minutes = seconds / 60.0
        let hours = minutes / 60.0

        if hours >= 1 then
            sprintf "%i:%02i:%02i" (floor hours |> int) (floor (minutes % 60.0) |> int) (floor (seconds % 60.0) |> int)
        else
            sprintf "%02i:%02i" (floor (minutes % 60.0) |> int) (floor (seconds % 60.0) |> int)

    let current_level (xp: int64) : int =
        (float xp / 999.0 |> sqrt |> floor |> int) + 1

    let xp_for_level (level: int) : int64 =
        int64 (level - 1) * int64 (level - 1) * 999L

    /// The local "rhythm game calendar day" is a term I made up
    /// It is the date this timestamp falls on relative to the user (i.e. respects the timezone of the machine)
    /// BUT the next day starts at 4am instead of midnight
    /// Example: A timestamp for 2am on the 2nd of January your time = "1st of January" as a rhythm game calendar day
    /// Example: A timestamp for 11pm on the 1st of January your time = "1st of January" as well
    /// Used to partition sessions into a calendar for stats viewing purposes
    let timestamp_to_rg_calendar_day (ts: int64) : DateTime = Timestamp.to_datetime(ts).Subtract(TimeSpan.FromHours(4.0)).ToLocalTime().Date

    /// The global "rhythm game month" this timestamp falls into
    /// Used for global monthly leaderboards which should be timezone-independent
    /// The borders between months are exactly the standard borders in UTC
    let timestamp_to_leaderboard_month (ts: int64) : int =
        let utc_date = Timestamp.to_datetime(ts)
        let start = 2025 * 12
        utc_date.Month + utc_date.Year * 12 - start

    let start_of_leaderboard_month (month: int) : int64 =
        DateTime(2025 + month / 12, (month - 1) %% 12 + 1, 1, 0, 0, 0, DateTimeKind.Utc)
        |> Timestamp.from_datetime