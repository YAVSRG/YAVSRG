namespace Prelude

open System
open System.IO
open System.Diagnostics
open Percyqaz.Common

[<AutoOpen>]
module Helpers =

#if DEBUG
    let DEV_MODE = true
#else
    let DEV_MODE = false
#endif

    /// Takes a function and returns an equivalent function BUT if given the same input repeatedly it will reuse the previous value instead of recalculating
    /// Used to optimise repeated calls to chart calculations where you are likely to make several for one selected chart before changing to another
    let internal cached (f: 'A -> 'B) : 'A -> 'B =
        let LOCK_OBJ = obj()
        let mutable previous : ('A * 'B) option = None
        fun (x: 'A) ->
            match
                lock LOCK_OBJ (fun () ->
                    match previous with
                    | Some (_x, _y) when x = _x -> Some _y
                    | _ -> None
                )
            with
            | Some cached_calculation -> cached_calculation
            | None ->
                let res = f x
                lock LOCK_OBJ (fun () -> previous <- Some (x, res))
                res

    let open_directory (path: string) : unit =
        ProcessStartInfo("file://" + Path.GetFullPath path, UseShellExecute = true)
        |> Process.Start
        |> ignore

    let open_url (url: string) : unit =
        try
            Process.Start(ProcessStartInfo(url, UseShellExecute = true)) |> ignore
        with err ->
            Logging.Debug "Failed to open url '%s' in browser: %O" url err

    let get_game_folder (name: string) : string =
        let p = Path.Combine(Directory.GetCurrentDirectory(), name)
        Directory.CreateDirectory p |> ignore
        p

    let inline format_duration_ms (ms: 'T) : string =
        let ms = float32 ms
        if ms > 3600_000f then
            sprintf
                "%i:%02i:%02i"
                (ms / 3600_000f |> floor |> int)
                ((ms / 60_000f) % 60f |> floor |> int)
                ((ms / 1_000f) % 60f |> floor |> int)
        else
            sprintf "%i:%02i" ((ms / 60_000f) % 60f |> floor |> int) ((ms / 1_000f) % 60f |> floor |> int)

    let format_timespan (ts: TimeSpan) : string =

        if ts < TimeSpan.Zero then
            "IN THE FUTURE?"
        elif ts.TotalDays > 365.0 then
            sprintf "%.0fy" (ts.TotalDays / 365.0)
        elif ts.TotalDays > 30.0 then
            sprintf "%.0fmo" (ts.TotalDays / 30.0)
        elif ts.TotalDays > 7.0 then
            sprintf "%.0fw" (ts.TotalDays / 7.0)
        elif ts.TotalDays > 1.0 then
            sprintf "%.0fd" ts.TotalDays
        elif ts.TotalHours > 1.0 then
            sprintf "%.0fh" ts.TotalHours
        elif ts.TotalMinutes > 1.0 then
            sprintf "%.0fm" ts.TotalMinutes
        else
            sprintf "%.0fs" ts.TotalSeconds