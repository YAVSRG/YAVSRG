namespace Interlude.Features.LevelSelect

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Charts.Caching
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Difficulty
open Prelude.Charts.Formats.Interlude
open Interlude.Options
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu
open Interlude.Features.Gameplay

type ChartInfo() as this =
    inherit StaticContainer(NodeType.None)

    let scores = Scoreboard(Position = Position.TrimBottom 190.0f)
    let mutable length = ""
    let mutable bpm = ""
    let mutable notecounts = ""

    do
        this
        |+ scores

        |+ Text(
            fun () -> sprintf "%.2f%s" (match Chart.rating with None -> 0.0 | Some d -> d.Physical) Icons.star
            ,
            Color = (fun () -> Color.White, match Chart.rating with None -> Color.Black | Some d -> physicalColor d.Physical),
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 10.0f; Top = 1.0f %- 240.0f; Right = 0.5f %+ 0.0f; Bottom = 1.0f %- 170.0f })

        |+ Text(
            fun () -> sprintf "%.2f%s" (match Chart.rating with None -> 0.0 | Some d -> d.Technical) Icons.star
            ,
            Color = (fun () -> Color.White, match Chart.rating with None -> Color.Black | Some d -> technicalColor d.Technical),
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 10.0f; Top = 1.0f %- 170.0f; Right = 0.5f %+ 0.0f; Bottom = 1.0f %- 100.0f })

        |+ Text(
            (fun () -> bpm),
            Align = Alignment.RIGHT,
            Position = { Left = 0.5f %+ 0.0f; Top = 1.0f %- 240.0f; Right = 1.0f %- 10.0f; Bottom = 1.0f %- 170.0f })

        |+ Text(
            (fun () -> length),
            Align = Alignment.RIGHT,
            Position = { Left = 0.5f %+ 0.0f; Top = 1.0f %- 170.0f; Right = 1.0f %- 10.0f; Bottom = 1.0f %- 100.0f })

        |+ Text(
            (fun () -> notecounts),
            Align = Alignment.RIGHT,
            Position = { Left = 0.0f %+ 10.0f; Top = 1.0f %- 100.0f; Right = 1.0f %- 17.0f; Bottom = 1.0f %- 60.0f })

        |+ Text(
            (fun () -> getModString(rate.Value, selectedMods.Value, autoplay)),
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 10.0f; Top = 1.0f %- 100.0f; Right = 1.0f %- 10.0f; Bottom = 1.0f %- 60.0f })
            
        |+ StylishButton(
            (fun () -> Preview().Show()),
            K (Icons.preview + " " + L"levelselect.preview"),
            Style.main 100,
            Hotkey = "preview",
            TiltLeft = false)
            .Tooltip(L"levelselect.preview.tooltip")
            .WithPosition { Left = 0.0f %+ 0.0f; Top = 1.0f %- 50.0f; Right = 0.33f %- 25.0f; Bottom = 1.0f %- 0.0f }

        |+ ModSelect(scores.Refresh)
            .Tooltip(L"levelselect.mods.tooltip")
            .WithPosition { Left = 0.33f %+ 0.0f; Top = 1.0f %- 50.0f; Right = 0.66f %- 25.0f; Bottom = 1.0f %- 0.0f }
        
        |* StylishButton(
            (fun () -> Setting.app CycleList.forward options.Rulesets; LevelSelect.refresh <- true),
            (fun () -> ruleset.Name),
            Style.main 100,
            TiltRight = false)
            .Tooltip(L"levelselect.rulesets.tooltip")
            .WithPosition { Left = 0.66f %+ 0.0f; Top = 1.0f %- 50.0f; Right = 1.0f %- 0.0f; Bottom = 1.0f %- 0.0f }

    member this.Refresh() =
        length <-
            match Chart.cacheInfo with
            | Some cc -> cc.Length
            | None -> 0.0f<ms>
            |> fun x -> x / rate.Value
            |> fun x -> (x / 1000.0f / 60.0f |> int, (x / 1000f |> int) % 60)
            |> fun (x, y) -> sprintf "%s %i:%02i" Icons.time x y
        bpm <-
            match Chart.cacheInfo with
            | Some cc -> cc.BPM
            | None -> (500.0f<ms/beat>, 500.0f<ms/beat>)
            |> fun (b, a) -> (60000.0f<ms> / a * rate.Value |> int, 60000.0f<ms> / b * rate.Value |> int)
            |> fun (a, b) ->
                if a > 9000 || b < 0 then sprintf "%s ∞" Icons.bpm
                elif Math.Abs(a - b) < 5 || b > 9000 then sprintf "%s %i" Icons.bpm a
                else sprintf "%s %i-%i" Icons.bpm a b
        notecounts <-
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
        scores.Refresh()