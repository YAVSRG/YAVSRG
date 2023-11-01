namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude.Common
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Difficulty
open Prelude.Charts.Formats.Interlude
open Interlude.Features
open Interlude.Options
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu
open Interlude.Features.Gameplay

type ChartInfo() as this =
    inherit StaticContainer(NodeType.None)

    let display = Setting.simple Display.Local |> Setting.trigger (fun _ -> this.Refresh())
    let scoreboard = Scoreboard(display, Position = Position.TrimBottom 120.0f)
    let online = Leaderboard(display, Position = Position.TrimBottom 120.0f)
    let details = Details(display, Position = Position.TrimBottom 120.0f)

    let mutable rating = 0.0
    let mutable notecounts = ""

    let changeRate v = 
        if Transitions.active then () else
        rate.Value <- rate.Value + v
        LevelSelect.refresh_details()

    do
        this
        |+ Conditional((fun () -> display.Value = Display.Local), scoreboard)
        |+ Conditional((fun () -> display.Value = Display.Online), online)
        |+ Conditional((fun () -> display.Value = Display.Details), details)

        |+ Text(
            (fun () -> sprintf "%s %.2f" Icons.star rating),
            Color = (fun () -> Color.White, physicalColor rating),
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 10.0f; Top = 1.0f %- 170.0f; Right = 0.33f %- 10.0f; Bottom = 1.0f %- 100.0f })

        |+ Text(
            (fun () -> Chart.FMT_BPM),
            Align = Alignment.CENTER,
            Position = { Left = 0.33f %+ 10.0f; Top = 1.0f %- 170.0f; Right = 0.66f %- 10.0f; Bottom = 1.0f %- 100.0f })

        |+ Text(
            (fun () -> Chart.FMT_DURATION),
            Align = Alignment.RIGHT,
            Position = { Left = 0.66f %+ 10.0f; Top = 1.0f %- 170.0f; Right = 1.0f %- 10.0f; Bottom = 1.0f %- 100.0f })

        |+ Text(
            (fun () -> notecounts),
            Align = Alignment.RIGHT,
            Position = { Left = 0.0f %+ 10.0f; Top = 1.0f %- 100.0f; Right = 1.0f %- 17.0f; Bottom = 1.0f %- 60.0f })

        |+ (
            StaticContainer(NodeType.None, 
                Position = { Left = 0.0f %+ 10.0f; Top = 1.0f %- 100.0f; Right = 0.5f %- 10.0f; Bottom = 1.0f %- 60.0f })
            |+ Text(
            (fun () -> getModString(rate.Value, selectedMods.Value, autoplay)),
            Align = Alignment.LEFT)
           )
           .Tooltip(
            Tooltip.Info("levelselect.selected_mods")
                .Hotkey(L"levelselect.selected_mods.mods.hint", "mods")
                .Body(L"levelselect.selected_mods.rate.hint")
                .Hotkey(L"levelselect.selected_mods.uprate.hint", "uprate")
                .Hotkey(L"levelselect.selected_mods.downrate.hint", "downrate")
           )
            
        |+ StylishButton(
            (fun () ->
                Chart.wait_for_load <| fun () ->
                Preview(Chart.WITH_MODS.Value, changeRate).Show()
            ),
            K (Icons.preview + " " + L"levelselect.preview.name"),
            !%Palette.MAIN_100,
            Hotkey = "preview",
            TiltLeft = false,
            Position = { Left = 0.0f %+ 0.0f; Top = 1.0f %- 50.0f; Right = 0.33f %- 25.0f; Bottom = 1.0f %- 0.0f })
            .Tooltip(Tooltip.Info("levelselect.preview", "preview"))

        |+ ModSelect(scoreboard.Refresh,
             Position = { Left = 0.33f %+ 0.0f; Top = 1.0f %- 50.0f; Right = 0.66f %- 25.0f; Bottom = 1.0f %- 0.0f })
            .Tooltip(Tooltip.Info("levelselect.mods", "mods"))
        
        |* Rulesets.QuickSwitcher(
            options.SelectedRuleset |> Setting.trigger (ignore >> LevelSelect.refresh_all),
            Position = { Left = 0.66f %+ 0.0f; Top = 1.0f %- 50.0f; Right = 1.0f %- 0.0f; Bottom = 1.0f %- 0.0f })
            .Tooltip(Tooltip.Info("levelselect.rulesets", "ruleset_switch").Hotkey(L"levelselect.rulesets.picker_hint", "ruleset_picker"))

        LevelSelect.on_refresh_all.Add this.Refresh
        LevelSelect.on_refresh_details.Add this.Refresh

    member this.Refresh() =
        match display.Value with
        | Display.Local -> scoreboard.Refresh()
        | Display.Online -> online.Refresh()
        | _ -> ()

        Chart.wait_for_load <| fun () ->
            rating <- Chart.RATING.Value.Physical
            notecounts <- Chart.FMT_NOTECOUNTS.Value

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)

        if (+."uprate_small").Tapped() then changeRate(0.01f)
        elif (+."uprate_half").Tapped() then changeRate(0.05f)
        elif (+."uprate").Tapped() then changeRate(0.1f)
        elif (+."downrate_small").Tapped() then changeRate(-0.01f)
        elif (+."downrate_half").Tapped() then changeRate(-0.05f)
        elif (+."downrate").Tapped() then changeRate(-0.1f)