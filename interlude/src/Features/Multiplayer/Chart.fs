namespace Interlude.Features.Multiplayer

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Data.Charts
open Prelude.Gameplay.Mods
open Interlude.Web.Shared
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.Features.Gameplay
open Interlude.Features.Online

module SelectedChart =

    let mutable chart = None
    let mutable found = false
    let mutable difficulty = ""
    let mutable length = ""
    let mutable bpm = ""
    let mutable notecounts = ""

    let update(c: LobbyChart option) =
        
        chart <- c
        match c with
        | None ->
            found <- true
            difficulty <- ""
            length <- ""
            bpm <- ""
            notecounts <- ""
        | Some chart ->
            found <- 
                match Library.lookupHash chart.Hash with
                | Some cc -> 
                    match Library.load cc with 
                    | Some c -> Chart.change(cc, Collections.LibraryContext.None, c); rate.Set chart.Rate; true
                    | None -> false
                | None -> Logging.Info(sprintf "Chart not found locally: %s [%s]" chart.Title chart.Hash); false
            if found then
                difficulty <- sprintf "%s %.2f" Icons.star (match Chart.rating with None -> 0.0 | Some d -> d.Physical)
                length <- Chart.format_duration()
                bpm <- Chart.format_bpm()
                notecounts <- Chart.format_notecounts()
            else
                Lobby.missing_chart()
                difficulty <- ""
                length <- ""
                bpm <- ""
                notecounts <- ""

type SelectedChart() =
    inherit StaticContainer(NodeType.None)

    override this.Init(parent: Widget) =
        
        this
        |+ Text((fun () -> match SelectedChart.chart with Some c -> c.Title | None -> L"lobby.no_song_selected"), Align = Alignment.LEFT, Position = Position.SliceTop(40.0f).Margin(10.0f, 0.0f))
        |+ Text((fun () -> match SelectedChart.chart with Some c -> c.Artist + "  •  " + c.Creator | None -> ""), Color = Style.text_subheading, Align = Alignment.LEFT, Position = Position.TrimTop(40.0f).SliceTop(30.0f).Margin(10.0f, 0.0f))
        |+ Text((fun () -> if SelectedChart.chart.IsSome && SelectedChart.found then Chart.cacheInfo.Value.DiffName else "???"), Color = Style.text_subheading, Align = Alignment.LEFT, Position = Position.TrimTop(70.0f).SliceTop(30.0f).Margin(10.0f, 0.0f))

        |+ Text((fun () -> SelectedChart.difficulty), Align = Alignment.LEFT, Position = Position.TrimTop(100.0f).SliceTop(60.0f))
        |+ Text((fun () -> SelectedChart.length), Align = Alignment.CENTER, Position = Position.TrimTop(100.0f).SliceTop(60.0f))
        |+ Text((fun () -> SelectedChart.bpm), Align = Alignment.RIGHT, Position = Position.TrimTop(100.0f).SliceTop(60.0f))
        |+ Text((fun () -> if SelectedChart.found then getModString(rate.Value, selectedMods.Value, autoplay) else ""), Align = Alignment.LEFT, Position = Position.TrimTop(160.0f).SliceTop(40.0f))
        |+ Text((fun () -> SelectedChart.notecounts), Align = Alignment.RIGHT, Position = Position.TrimTop(160.0f).SliceTop(40.0f))
        |+ Text((fun () -> if SelectedChart.found then "" else L"lobby.missing_chart"), Align = Alignment.CENTER, Position = Position.TrimTop(100.0f).SliceTop(60.0f))

        |+ Conditional(
            (fun () -> Network.lobby.IsSome && Network.lobby.Value.YouAreHost),
            StylishButton(
                (fun () -> Screen.change Screen.Type.LevelSelect Transitions.Flags.Default),
                K (sprintf "%s %s" Icons.reset (L"lobby.change_chart")),
                Style.dark 100,
                TiltRight = false,
                Position = { Position.SliceBottom(50.0f) with Left = 0.66f %- 0.0f }
            )
        )
        |* Conditional(
            (fun () -> Network.lobby.IsSome && Network.lobby.Value.YouAreHost && Network.lobby.Value.Ready),
            StylishButton(
                (fun () -> Network.client.Send Upstream.START_GAME),
                K (sprintf "%s %s" Icons.play (L"lobby.start_game")),
                Style.main 100,
                Position = { Position.SliceBottom(50.0f) with Left = 0.33f %+ 0.0f; Right = 0.66f %- 25.0f }
            )
        )

        SelectedChart.update Network.lobby.Value.Chart
        Network.Events.join_lobby.Add(fun () -> SelectedChart.update None)
        Network.Events.change_chart.Add(fun () -> if Screen.currentType = Screen.Type.Lobby then SelectedChart.update Network.lobby.Value.Chart)

        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.SliceTop(70.0f)) (if SelectedChart.found then Style.dark 180 () else Color.FromArgb(180, 100, 100, 100))
        Draw.rect (this.Bounds.SliceTop(100.0f).SliceBottom(30.0f)) (if SelectedChart.found then Style.darkD 180 () else Color.FromArgb(180, 50, 50, 50))
        Draw.rect (this.Bounds.SliceTop(100.0f).SliceLeft(5.0f)) (if SelectedChart.found then Style.main 255 () else Color.White)

        base.Draw()