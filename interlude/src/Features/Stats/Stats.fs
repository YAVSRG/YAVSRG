namespace Interlude.Features.Stats

open Percyqaz.Data
open Prelude.Data.User
open Interlude.Content

module Stats =

    let init_startup () =
        Stats.init Content.Library Content.UserData

    let deinit () =
        Stats.save_current_session Content.UserData

    // helpers

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

        if hours > 1 then
            sprintf "%i:%02i:%02i" (floor hours |> int) (floor (minutes % 60.0) |> int) (floor (seconds % 60.0) |> int)
        else
            sprintf "%02i:%02i" (floor (minutes % 60.0) |> int) (floor (seconds % 60.0) |> int)
