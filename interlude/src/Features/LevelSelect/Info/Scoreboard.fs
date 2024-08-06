namespace Interlude.Features.LevelSelect

open System
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Charts
open Prelude.Gameplay
open Prelude.Data
open Prelude.Data.Library.Caching
open Interlude.Content
open Interlude.UI
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.Features.Score

module Scoreboard =

    [<RequireQualifiedAccess>]
    type Sort =
        | Time = 0
        | Performance = 1
        | Accuracy = 2

    [<RequireQualifiedAccess>]
    type Filter =
        | None = 0
        | CurrentRate = 1
        | CurrentMods = 2

    type ScoreCard(score_info: ScoreInfo) =
        inherit
            FrameContainer(
                NodeType.Button(
                    (fun () ->
                        Screen.change_new
                            (fun () -> new ScoreScreen(score_info, ImprovementFlags.None, false) :> Screen)
                            Screen.Type.Score
                            Transitions.EnterGameplay
                        |> ignore
                    )
                )
            )

        let fade = Animation.Fade(0.0f, Target = 1.0f)
        let animation = Animation.seq [ Animation.Delay 150; fade ]

        override this.Init(parent) =
            this.Fill <-
                fun () ->
                    if this.Focused then
                        Colors.yellow_accent.O1a fade.Alpha
                    else
                        (!*Palette.DARK).O2a fade.Alpha

            this.Border <-
                fun () ->
                    if this.Focused then
                        Colors.yellow_accent.O4a fade.Alpha
                    else
                        (!*Palette.LIGHT).O2a fade.Alpha

            let text_color =
                fun () -> let a = fade.Alpha in (Colors.white.O4a a, Colors.shadow_1.O4a a)

            let text_subcolor =
                fun () -> let a = fade.Alpha in (Colors.grey_1.O4a a, Colors.shadow_2.O4a a)

            let upper = Position.SliceT(47.5f).Shrink(10.0f, 1f).Translate(0.0f, -2.0f)
            let lower = Position.ShrinkT(37.5f).Shrink(10.0f, 1f).Translate(0.0f, -1.0f)

            this
            |+ Text(
                fun () -> score_info.Scoring.FormatAccuracy()
                , Color = text_color
                , Align = Alignment.LEFT
                , Position = upper
            )

            |+ Text(
                fun () ->
                    sprintf
                        "%s  •  %ix  •  %.2f"
                        (score_info.Ruleset.LampName score_info.Lamp)
                        score_info.Scoring.State.BestCombo
                        score_info.Physical
                , Color = text_subcolor
                , Align = Alignment.LEFT
                , Position = lower
            )

            |+ Text(
                K(
                    format_timespan (DateTimeOffset.UtcNow - Timestamp.to_datetimeoffset score_info.TimePlayed)
                    + if score_info.ImportedFromOsu then
                          " " + Icons.DOWNLOAD
                      else
                          ""
                ),
                Color = text_subcolor,
                Align = Alignment.RIGHT,
                Position = lower
            )

            |+ Text(
                score_info.ModString(),
                Color = text_color,
                Align = Alignment.RIGHT,
                Position = upper
            )

            |* Clickable.Focus(this, OnRightClick = (fun () -> ScoreContextMenu(score_info).Show()))

            base.Init parent

        member this.Data = score_info

        member this.FadeOut() = fade.Target <- 0.0f

        override this.OnFocus(by_mouse: bool) =
            base.OnFocus by_mouse
            Style.hover.Play()

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)
            animation.Update elapsed_ms

            if Mouse.hover this.Bounds && (%%"delete").Tapped() then
                ScoreContextMenu.ConfirmDeleteScore(score_info, false)
            elif this.Focused && (%%"context_menu").Tapped() then
                ScoreContextMenu(score_info).Show()

    module Loader =

        type Request =
            {
                RulesetId: string
                Ruleset: Ruleset
                CachedChart: CachedChart
                CurrentChart: Chart
                ChartSaveData: ChartSaveData
                mutable NewBests: Bests option
            }
            override this.ToString() = "<scoreboard calculation>"

        let container = FlowContainer.Vertical(70.0f, Spacing = Style.PADDING * 3.0f)

        let score_loader =
            { new Async.SwitchServiceSeq<Request, unit -> unit>() with
                member this.Process(req: Request) =
                    seq {
                        for score in req.ChartSaveData.Scores do
                            let score_info =
                                ScoreInfo.from_score req.CachedChart req.CurrentChart req.Ruleset score

                            if score_info.ModStatus = Mods.ModStatus.Ranked then
                                req.NewBests <-
                                    Some(
                                        match req.NewBests with
                                        | None -> Bests.create score_info
                                        | Some b -> fst (Bests.update score_info b)
                                    )

                            let sc = ScoreCard score_info
                            yield fun () -> container.Add sc

                        match req.NewBests with
                        | None -> ()
                        | Some new_bests ->
                            let old_bests = req.ChartSaveData.PersonalBests
                            let new_bests = Map.add req.RulesetId new_bests old_bests

                            if new_bests <> old_bests then
                                req.ChartSaveData.PersonalBests <- new_bests
                                yield fun () -> LevelSelect.refresh_details ()

                    }

                member this.Handle(action) = action ()
            }

        let load (info: LoadedChartInfo) =
            score_loader.Request
                {
                    RulesetId = Rulesets.current_hash
                    Ruleset = Rulesets.current
                    CachedChart = info.CacheInfo
                    CurrentChart = info.Chart
                    ChartSaveData = info.SaveData
                    NewBests = None
                }

            container.Clear()

open Scoreboard

type Scoreboard(display: Setting<Display>) =
    inherit Container(NodeType.None)

    let mutable count = -1

    let mutable last_loading = ""
    let mutable last_loaded = ""
    let mutable scoring = ""

    let filter = Setting.simple Filter.None
    let sort = Setting.map enum int options.ScoreSortMode

    let sorter () : ScoreCard -> ScoreCard -> int =
        match sort.Value with
        | Sort.Accuracy -> fun b a -> a.Data.Scoring.Accuracy.CompareTo b.Data.Scoring.Accuracy
        | Sort.Performance -> fun b a -> a.Data.Physical.CompareTo b.Data.Physical
        | Sort.Time
        | _ -> fun b a -> a.Data.TimePlayed.CompareTo b.Data.TimePlayed

    let filterer () : ScoreCard -> bool =
        match filter.Value with
        | Filter.CurrentRate -> (fun a -> a.Data.Rate = SelectedChart.rate.Value)
        | Filter.CurrentMods -> (fun a -> a.Data.Mods = SelectedChart.selected_mods.Value)
        | _ -> K true

    let scroll_container =
        ScrollContainer(Loader.container, Margin = Style.PADDING, Position = Position.ShrinkT(50.0f))

    override this.Init(parent) =
        SelectedChart.on_chart_change_started.Add(fun info ->
            if info.CacheInfo.Hash <> last_loading then
                Loader.container.Iter(fun s -> s.FadeOut())
                last_loading <- info.CacheInfo.Hash
                last_loaded <- ""
        )

        Loader.container.Sort <- sorter ()

        this
        |+ StylishButton(
            (fun () -> display.Set Display.Online),
            K <| %"levelselect.info.scoreboard",
            !%Palette.MAIN_100,
            Hotkey = "scoreboard_storage",
            TiltLeft = false,
            Position =
                {
                    Left = 0.0f %+ 0.0f
                    Top = 0.0f %+ 0.0f
                    Right = 0.33f %- 25.0f
                    Bottom = 0.0f %+ 50.0f
                }
        )
            .Help(Help.Info("levelselect.info.mode", "scoreboard_storage"))
        |+ StylishButton
            .Selector(
                Icons.CHEVRONS_UP,
                [|
                    Sort.Accuracy, %"levelselect.info.scoreboard.sort.accuracy"
                    Sort.Performance, %"levelselect.info.scoreboard.sort.performance"
                    Sort.Time, %"levelselect.info.scoreboard.sort.time"
                |],
                sort |> Setting.trigger (fun _ -> Loader.container.Sort <- sorter ()),
                !%Palette.DARK_100,
                Hotkey = "scoreboard_sort",
                Position =
                    {
                        Left = 0.33f %+ 0.0f
                        Top = 0.0f %+ 0.0f
                        Right = 0.66f %- 25.0f
                        Bottom = 0.0f %+ 50.0f
                    }
            )
            .Help(Help.Info("levelselect.info.scoreboard.sort", "scoreboard_sort"))
        |+ StylishButton
            .Selector(
                Icons.FILTER,
                [|
                    Filter.None, %"levelselect.info.scoreboard.filter.none"
                    Filter.CurrentRate, %"levelselect.info.scoreboard.filter.currentrate"
                    Filter.CurrentMods, %"levelselect.info.scoreboard.filter.currentmods"
                |],
                filter |> Setting.trigger (fun _ -> this.Refresh()),
                !%Palette.MAIN_100,
                Hotkey = "scoreboard_filter",
                TiltRight = false,
                Position =
                    {
                        Left = 0.66f %+ 0.0f
                        Top = 0.0f %+ 0.0f
                        Right = 1.0f %- 0.0f
                        Bottom = 0.0f %+ 50.0f
                    }
            )
            .Help(Help.Info("levelselect.info.scoreboard.filter", "scoreboard_filter"))
        |+ scroll_container
        |+ HotkeyAction(
            "scoreboard",
            fun () ->
                if Loader.container.Focused then
                    Selection.clear ()
                else
                    Loader.container.Focus false
        )
        |* EmptyState(Icons.WIND, %"levelselect.info.scoreboard.empty", Subtitle = %"levelselect.info.scoreboard.empty.subtitle")
            .Conditional(fun () -> count = 0)
        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        Loader.score_loader.Join()

    member this.OnChartUpdated(info: LoadedChartInfo) =
        if
            (let v = info.SaveData.Scores.Length <> count in
             count <- info.SaveData.Scores.Length
             v)
            || info.CacheInfo.Hash <> last_loaded
        then
            last_loaded <- info.CacheInfo.Hash
            Loader.load info
        elif scoring <> Rulesets.current_hash then
            Loader.container.Iter(fun score -> score.Data.Ruleset <- Rulesets.current)
            scoring <- Rulesets.current_hash

        Loader.container.Filter <- filterer ()

    member this.Refresh() = SelectedChart.when_loaded this.OnChartUpdated
