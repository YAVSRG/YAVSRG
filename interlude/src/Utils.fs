namespace Interlude

open System
open System.Reflection
open System.IO
open Prelude

module Utils =

    let get_resource_stream name =
        Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream("Interlude.Resources." + name)

    let get_resource_text name =
        use s = get_resource_stream name
        use tr = new StreamReader(s)
        tr.ReadToEnd()

    let splash_message_picker (name) =
        let r = new Random()
        let text = get_resource_text name
        let lines = text.Split("\n")

        fun () -> lines.[r.Next lines.Length]

    let format_duration_ms (ms: Time) =
        if ms > 3600_000f<ms> then
            sprintf
                "%i:%02i:%02i"
                (ms / 3600_000f<ms> |> floor |> int)
                ((ms / 60_000f<ms>) % 60f |> floor |> int)
                ((ms / 1_000f<ms>) % 60f |> floor |> int)
        else
            sprintf "%i:%02i" ((ms / 60_000f<ms>) % 60f |> floor |> int) ((ms / 1_000f<ms>) % 60f |> floor |> int)

    let format_timespan (ts: TimeSpan) =

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
