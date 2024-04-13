namespace Interlude.Features.Multiplayer

open System.Linq
open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Data.Library.Caching
open Prelude.Data.Library.Collections
open Prelude.Gameplay.Mods
open Interlude.Web.Shared
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Gameplay
open Interlude.Features.Collections
open Interlude.Features.Online
open Interlude.Features.Play

type MultiplayerChartContextMenu(cc: CachedChart) as this =
    inherit Page()

    do
        let content =
            FlowContainer.Vertical(PRETTYHEIGHT, Position = Position.Margin(100.0f, 200.0f))

            |+ PageButton(
                "chart.add_to_collection",
                (fun () -> AddToCollectionPage(cc).Show()),
                Icon = Icons.FOLDER_PLUS
            )

        this.Content content

    override this.Title = cc.Title
    override this.OnClose() = ()

module SelectedChart =

    let mutable chart = None

    let selected () =
        match Chart.CACHE_DATA, chart with
        | Some cc, Some chart -> cc.Hash = chart.Hash
        | _ -> false

    let loaded () = selected () && Chart.WITH_MODS.IsSome

    let ensure_selected () =
        match chart with
        | None -> ()
        | Some chart ->
            if selected () then
                rate.Set chart.Rate

                selected_mods.Set(
                    chart.Mods
                    |> Map.ofArray
                    |> Map.filter (fun id _ -> Mods.AVAILABLE_MODS.ContainsKey id)
                )
            else
                match Cache.by_hash chart.Hash Content.Cache with
                | None ->
                    Logging.Info(sprintf "Chart not found locally: %s [%s]" chart.Title chart.Hash)
                    Network.lobby.Value.ReportMissingChart()
                | Some cc ->
                    Chart.change (cc, LibraryContext.None, true)
                    rate.Set chart.Rate

                    selected_mods.Set(
                        chart.Mods
                        |> Map.ofArray
                        |> Map.filter (fun id _ -> Mods.AVAILABLE_MODS.ContainsKey id)
                    )

    let switch (c: LobbyChart option) =

        chart <- c
        ensure_selected ()

type SelectedChart(lobby: Lobby) =
    inherit Container(NodeType.None)

    override this.Init(parent: Widget) =

        this
        |+ Text(
            (fun () ->
                match SelectedChart.chart with
                | Some c -> c.Title
                | None -> %"lobby.no_song_selected"
            ),
            Align = Alignment.LEFT,
            Position = Position.SliceTop(40.0f).Margin(10.0f, 0.0f)
        )
        |+ Text(
            (fun () ->
                match SelectedChart.chart with
                | Some c -> c.Artist + "  •  " + c.Creator
                | None -> ""
            ),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT,
            Position = Position.TrimTop(40.0f).SliceTop(30.0f).Margin(10.0f, 0.0f)
        )
        |+ Text(
            (fun () ->
                if SelectedChart.loaded () then
                    Chart.CACHE_DATA.Value.DifficultyName
                else
                    "???"
            ),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT,
            Position = Position.TrimTop(70.0f).SliceTop(30.0f).Margin(10.0f, 0.0f)
        )

        |+ Text(
            (fun () ->
                if SelectedChart.loaded () then
                    sprintf "%s %.2f" Icons.STAR Chart.RATING.Value.Physical
                else
                    ""
            ),
            Align = Alignment.LEFT,
            Position = Position.TrimTop(100.0f).SliceTop(60.0f)
        )
        |+ Text(
            (fun () -> if SelectedChart.loaded () then Chart.FMT_DURATION else ""),
            Align = Alignment.CENTER,
            Position = Position.TrimTop(100.0f).SliceTop(60.0f)
        )
        |+ Text(
            (fun () -> if SelectedChart.loaded () then Chart.FMT_BPM else ""),
            Align = Alignment.RIGHT,
            Position = Position.TrimTop(100.0f).SliceTop(60.0f)
        )
        |+ Text(
            (fun () ->
                if SelectedChart.loaded () then
                    Mods.format (rate.Value, selected_mods.Value, false)
                else
                    ""
            ),
            Align = Alignment.LEFT,
            Position = Position.TrimTop(160.0f).SliceTop(40.0f)
        )
        |+ Text(
            (fun () ->
                if SelectedChart.loaded () then
                    Chart.FMT_NOTECOUNTS.Value
                else
                    ""
            ),
            Align = Alignment.RIGHT,
            Position = Position.TrimTop(160.0f).SliceTop(40.0f)
        )
        |+ Text(
            (fun () ->
                if SelectedChart.loaded () || SelectedChart.chart.IsNone then
                    ""
                else
                    %"lobby.missing_chart"
            ),
            Align = Alignment.CENTER,
            Position = Position.TrimTop(100.0f).SliceTop(60.0f)
        )

        |+ Clickable(
            fun () -> if lobby.YouAreHost then Screen.change Screen.Type.LevelSelect Transitions.Flags.Default |> ignore
            , Position = Position.SliceTop(100.0f)
        )

        |+ Conditional(
            (fun () -> 
                SelectedChart.loaded ()
                && not lobby.GameInProgress
                && lobby.ReadyStatus = ReadyFlag.NotReady
            ),

            StylishButton(
                (fun () -> lobby.Spectate <- not lobby.Spectate),
                (fun () ->
                    if lobby.Spectate then
                        sprintf "%s %s" Icons.EYE (%"lobby.spectator")
                    else
                        sprintf "%s %s" Icons.PLAY (%"lobby.player")
                ),
                !%Palette.MAIN_100,
                Position =
                    { Position.SliceBottom(50.0f) with
                        Right = 0.5f %- 25.0f
                    }
            )
        )

        |+ Conditional(
            (fun () ->
                SelectedChart.loaded ()
                && lobby.GameInProgress
            ),
            StylishButton(
                (fun () ->
                    let username =
                        lobby.Players.Keys.First(fun p ->
                            lobby.Players.[p].Status = LobbyPlayerStatus.Playing
                        ) // todo: or fail gracefully

                    Chart.if_loaded
                    <| fun info -> //todo: store info in sync with selected chart in SelectedChart
                        Screen.change_new
                            (fun () -> SpectateScreen.spectate_screen (info, username, Network.lobby.Value))
                            Screen.Type.Replay
                            Transitions.Flags.Default
                        |> ignore
                ),
                K(sprintf "%s %s" Icons.EYE (%"lobby.spectate")),
                !%Palette.DARK_100,
                TiltRight = false,
                Position =
                    { Position.SliceBottom(50.0f) with
                        Left = 0.5f %- 0.0f
                    }
            )
        )

        |+ Conditional(
            (fun () ->
                SelectedChart.loaded ()
                && not Song.loading
                && not lobby.GameInProgress
            ),

            StylishButton(
                (fun () ->
                    Network.lobby.Value.SetReadyStatus (
                        match Network.lobby.Value.ReadyStatus with
                        | ReadyFlag.NotReady ->
                            if Network.lobby.Value.Spectate then
                                ReadyFlag.Spectate
                            else
                                ReadyFlag.Play
                        | _ -> ReadyFlag.NotReady
                    )
                ),
                (fun () ->
                    match Network.lobby with
                    | Some l ->
                        match l.ReadyStatus with
                        | ReadyFlag.NotReady ->
                            if Network.lobby.Value.Spectate then
                                sprintf "%s %s" Icons.EYE (%"lobby.ready")
                            else
                                sprintf "%s %s" Icons.CHECK (%"lobby.ready")
                        | _ -> sprintf "%s %s" Icons.X (%"lobby.not_ready")
                    | None -> "!"
                ),
                !%Palette.DARK_100,
                TiltRight = false,
                Position =
                    { Position.SliceBottom(50.0f) with
                        Left = 0.5f %- 0.0f
                    }
            )
        )

        |* Conditional(
            (fun () ->
                lobby.YouAreHost
                && lobby.ReadyStatus <> ReadyFlag.NotReady
                && not lobby.GameInProgress
            ),

            StylishButton(
                (fun () ->
                    if lobby.Countdown then
                        lobby.CancelRound()
                    else
                        lobby.StartRound()
                ),
                (fun () ->
                    if lobby.Countdown then
                        sprintf "%s %s" Icons.SLASH (%"lobby.cancel_game")
                    else
                        sprintf "%s %s" Icons.PLAY (%"lobby.start_game")
                ),
                !%Palette.MAIN_100,
                Position =
                    { Position.SliceBottom(50.0f) with
                        Right = 0.5f %- 25.0f
                    }
            )
        )

        SelectedChart.switch lobby.Chart
        NetworkEvents.join_lobby.Add(fun lobby -> SelectedChart.switch None)

        lobby.OnChartChanged.Add(fun () ->
            if Screen.current_type = Screen.Type.Lobby then
                SelectedChart.switch lobby.Chart
        )

        base.Init parent

    override this.Draw() =
        Draw.rect
            (this.Bounds.SliceTop(70.0f))
            (if SelectedChart.loaded () then
                 (!*Palette.DARK).O4a 180
             else
                 Color.FromArgb(180, 100, 100, 100))

        Draw.rect
            (this.Bounds.SliceTop(100.0f).SliceBottom(30.0f))
            (if SelectedChart.loaded () then
                 (!*Palette.DARKER).O4a 180
             else
                 Color.FromArgb(180, 50, 50, 50))

        Draw.rect
            (this.Bounds.SliceTop(100.0f).SliceLeft(5.0f))
            (if SelectedChart.loaded () then
                 !*Palette.MAIN
             else
                 Colors.white)

        base.Draw()
