namespace Prelude.Data.User.Stats

[<AutoOpen>]
module StatsHelpers =

    let add_playtimes (total: Map<int, float>) (session: Map<int, float>) =
        Map.fold
            (fun (pt: Map<int, float>) keymode time ->
                pt.Change(
                    keymode,
                    function
                    | None -> Some time
                    | Some t -> Some (time + t)
                )
            )
            total
            session

    let format_long_time (time: float) =
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

    let format_short_time (time: float) =
        let seconds = time / 1000.0
        let minutes = seconds / 60.0
        let hours = minutes / 60.0

        if hours >= 1 then
            sprintf "%i:%02i:%02i" (floor hours |> int) (floor (minutes % 60.0) |> int) (floor (seconds % 60.0) |> int)
        else
            sprintf "%02i:%02i" (floor (minutes % 60.0) |> int) (floor (seconds % 60.0) |> int)

    let current_level (xp: int64) =
        (float xp / 999.0 |> sqrt |> floor |> int) + 1

    let xp_for_level (level: int) =
        int64 (level - 1) * int64 (level - 1) * 999L