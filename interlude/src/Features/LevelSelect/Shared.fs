namespace Interlude.Features.LevelSelect

open System
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Interlude.Features.Gameplay
open Interlude.UI

module LevelSelect =

    /// Set this to true to have level select "consume" it and refresh on the next update frame
    let mutable refresh = false

    /// Same as above, but just refresh minor info like pbs, whether charts are in collections or not, etc
    let mutable minorRefresh = false

    let format_chart_duration() =
        match Chart.cacheInfo with
        | Some cc -> cc.Length
        | None -> 0.0f<ms>
        |> fun x -> x / rate.Value
        |> fun x -> (x / 1000.0f / 60.0f |> int, (x / 1000f |> int) % 60)
        |> fun (x, y) -> sprintf "%s %i:%02i" Icons.time x y

    let format_chart_bpm() =
        match Chart.cacheInfo with
        | Some cc -> cc.BPM
        | None -> (500.0f<ms/beat>, 500.0f<ms/beat>)
        |> fun (b, a) -> (60000.0f<ms> / a * rate.Value |> int, 60000.0f<ms> / b * rate.Value |> int)
        |> fun (a, b) ->
            if a > 9000 || b < 0 then sprintf "%s ∞" Icons.bpm
            elif Math.Abs(a - b) < 5 || b > 9000 then sprintf "%s %i" Icons.bpm a
            else sprintf "%s %i-%i" Icons.bpm a b

    let format_chart_notecounts() =
        match Chart.current with
        | Some c ->
            let mutable notes = 0
            let mutable lnotes = 0
            for (_, nr) in c.Notes.Data do
                for n in nr do
                    if n = NoteType.NORMAL then notes <- notes + 1
                    elif n = NoteType.HOLDHEAD then notes <- notes + 1; lnotes <- lnotes + 1
            sprintf "%iK | %i Notes | %.0f%% Holds" c.Keys notes (100.0f * float32 lnotes / float32 notes)
        | None -> ""