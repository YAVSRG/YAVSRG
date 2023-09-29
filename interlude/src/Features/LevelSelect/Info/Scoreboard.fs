namespace Interlude.Features.LevelSelect

open System
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Scores
open Prelude.Data.Charts.Caching
open Prelude.Gameplay
open Prelude.Charts.Formats.Interlude
open Interlude
open Interlude.UI
open Interlude.Utils
open Interlude.Options
open Interlude.UI.Components
open Interlude.UI.Menu
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

    type ScoreCard(data: ScoreInfoProvider) as this =
        inherit Frame(NodeType.Button((fun () -> 
            Screen.changeNew 
                (fun () -> new ScoreScreen(data, ImprovementFlags.Default) :> Screen)
                Screen.Type.Score
                Transitions.Flags.Default)))

        let fade = Animation.Fade(0.0f, Target = 1.0f)
        let animation = Animation.seq [Animation.Delay 150; fade]

        do
            this.Fill <- fun () -> if this.Focused then Colors.yellow_accent.O1a fade.Alpha else (!*Palette.DARK).O2a fade.Alpha
            this.Border <- fun () -> if this.Focused then Colors.yellow_accent.O4a fade.Alpha else (!*Palette.LIGHT).O2a fade.Alpha
            ignore data.Physical
            ignore data.Lamp

            let text_color = fun () -> let a = fade.Alpha in (Colors.white.O4a a, Colors.shadow_1.O4a a)
            let text_subcolor = fun () -> let a = fade.Alpha in (Colors.grey_1.O4a a, Colors.shadow_2.O4a a)

            this
            |+ Text(
                fun () -> data.Scoring.FormatAccuracy()
                ,
                Color = text_color,
                Align = Alignment.LEFT,
                Position = { Left = 0.0f %+ 5.0f; Top = 0.0f %+ 0.0f; Right = 0.5f %+ 0.0f; Bottom = 0.6f %+ 0.0f })

            |+ Text(
                fun () -> sprintf "%s  •  %ix  •  %.2f" (data.Ruleset.LampName data.Lamp) data.Scoring.State.BestCombo data.Physical
                ,
                Color = text_subcolor,
                Align = Alignment.LEFT,
                Position = { Left = 0.0f %+ 5.0f; Top = 0.6f %- 5.0f; Right = 0.5f %+ 0.0f; Bottom = 1.0f %- 2.0f })

            |+ Text(
                K (formatTimeOffset(DateTime.UtcNow - data.ScoreInfo.time.ToUniversalTime()) + if data.ScoreInfo.layout = Layout.Layout.LeftTwo then " " + Icons.download else ""),
                Color = text_subcolor,
                Align = Alignment.RIGHT,
                Position = { Left = 0.5f %+ 0.0f; Top = 0.6f %- 5.0f; Right = 1.0f %- 5.0f; Bottom = 1.0f %- 2.0f })

            |+ Text(
                K data.Mods,
                Color = text_color,
                Align = Alignment.RIGHT,
                Position = { Left = 0.5f %+ 0.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %- 5.0f; Bottom = 0.6f %+ 0.0f })

            |* Clickable(this.Select,
                OnRightClick = fun () -> ScoreContextMenu(data).Show())

        member this.Data = data

        override this.OnFocus() = Style.hover.Play(); base.OnFocus()

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            animation.Update elapsedTime
            if Mouse.hover this.Bounds && (!|"delete").Tapped() then ScoreContextMenu.ConfirmDeleteScore(data, false)
            elif this.Focused && (!|"context_menu").Tapped() then ScoreContextMenu(data).Show()

    module Loader =

        type Request =
            {
                RulesetId: string
                Ruleset: Ruleset
                CurrentChart: Chart
                ChartSaveData: ChartSaveData option
                mutable NewBests: Bests option
            }
            override this.ToString() = "<scoreboard calculation>"

        let container = FlowContainer.Vertical(75.0f, Spacing = Style.PADDING * 3.0f)

        let score_loader =
            { new Async.SwitchServiceSeq<Request, unit -> unit>() with
            member this.Process(req: Request) =
                match req.ChartSaveData with
                | None -> Seq.empty
                | Some d ->
                    seq { 
                        for score in d.Scores do
                            let s = ScoreInfoProvider(score, req.CurrentChart, req.Ruleset)
                            if s.ModStatus = Mods.ModStatus.Ranked then
                                req.NewBests <- Some (
                                    match req.NewBests with
                                    | None -> Bests.create s
                                    | Some b -> fst(Bests.update s b))
                            let sc = ScoreCard s
                            yield fun () -> container.Add sc

                        match req.NewBests with
                        | None -> ()
                        | Some b ->
                            yield fun () ->
                                if 
                                    not (req.ChartSaveData.Value.PersonalBests.ContainsKey req.RulesetId) 
                                    || b <> req.ChartSaveData.Value.PersonalBests.[req.RulesetId] 
                                then
                                    LevelSelect.refresh_details()
                                    req.ChartSaveData.Value.PersonalBests.[req.RulesetId] <- b
                            
                    }
            member this.Handle(action) = action()
        }

        let load() =
            score_loader.Request
                {
                    RulesetId = Content.Rulesets.current_hash
                    Ruleset = Content.Rulesets.current
                    CurrentChart = Chart.CHART.Value
                    ChartSaveData = Chart.SAVE_DATA
                    NewBests = None
                }
            container.Clear()

open Scoreboard

type Scoreboard(display: Setting<Display>) as this =
    inherit StaticContainer(NodeType.None)

    let mutable count = -1

    let mutable chart = ""
    let mutable scoring = ""

    let filter = Setting.simple Filter.None
    let sort = Setting.map enum int options.ScoreSortMode

    let sorter() : ScoreCard -> ScoreCard -> int =
        match sort.Value with
        | Sort.Accuracy -> fun b a -> a.Data.Scoring.Value.CompareTo b.Data.Scoring.Value
        | Sort.Performance -> fun b a -> a.Data.Physical.CompareTo b.Data.Physical
        | Sort.Time
        | _ -> fun b a -> a.Data.ScoreInfo.time.CompareTo b.Data.ScoreInfo.time

    let filterer() : ScoreCard -> bool =
        match filter.Value with
        | Filter.CurrentRate -> (fun a -> a.Data.ScoreInfo.rate = rate.Value)
        | Filter.CurrentMods -> (fun a -> a.Data.ScoreInfo.selectedMods = selectedMods.Value)
        | _ -> K true

    let scrollContainer = ScrollContainer.Flow(Loader.container, Margin = Style.PADDING, Position = Position.TrimTop(55.0f).TrimBottom(50.0f))

    do
        Loader.container.Sort <- sorter()
        this
        |+ StylishButton(
            (fun () -> display.Set Display.Online),
            K <| Localisation.localise "levelselect.info.scoreboard.name",
            !%Palette.MAIN_100,
            Hotkey = "scoreboard_storage",
            TiltLeft = false,
            Position = { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 0.0f; Right = 0.33f %- 25.0f; Bottom = 0.0f %+ 50.0f })
            .Tooltip(Tooltip.Info("levelselect.info.mode", "scoreboard_storage"))
        |+ StylishButton.Selector(
            Icons.sort,
            [|
                Sort.Accuracy, L"levelselect.info.scoreboard.sort.accuracy"
                Sort.Performance, L"levelselect.info.scoreboard.sort.performance"
                Sort.Time, L"levelselect.info.scoreboard.sort.time"
            |],
            sort |> Setting.trigger (fun _ -> Loader.container.Sort <- sorter()),
            !%Palette.DARK_100,
            Hotkey = "scoreboard_sort",
            Position = { Left = 0.33f %+ 0.0f; Top = 0.0f %+ 0.0f; Right = 0.66f %- 25.0f; Bottom = 0.0f %+ 50.0f })
            .Tooltip(Tooltip.Info("levelselect.info.scoreboard.sort", "scoreboard_sort"))
        |+ StylishButton.Selector(
            Icons.filter,
            [|
                Filter.None, L"levelselect.info.scoreboard.filter.none"
                Filter.CurrentRate, L"levelselect.info.scoreboard.filter.currentrate"
                Filter.CurrentMods, L"levelselect.info.scoreboard.filter.currentmods"
            |],
            filter |> Setting.trigger (fun _ -> this.Refresh()),
            !%Palette.MAIN_100,
            Hotkey = "scoreboard_filter",
            TiltRight = false,
            Position = { Left = 0.66f %+ 0.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %- 0.0f; Bottom = 0.0f %+ 50.0f })
            .Tooltip(Tooltip.Info("levelselect.info.scoreboard.filter", "scoreboard_filter"))
        |+ scrollContainer
        |+ HotkeyAction("scoreboard", fun () -> if Loader.container.Focused then Selection.clear() else Loader.container.Focus())
        |* Conditional((fun () -> count = 0), EmptyState(Icons.empty_scoreboard, L"levelselect.info.scoreboard.empty"))

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        Loader.score_loader.Join()

    member this.Refresh() =
        let h = match Chart.CACHE_DATA with Some c -> c.Hash | None -> ""

        Chart.wait_for_load <| fun () ->

        if (match Chart.SAVE_DATA with None -> false | Some d -> let v = d.Scores.Count <> count in count <- d.Scores.Count; v) || h <> chart then
            chart <- h
            Loader.load()
        elif scoring <> Content.Rulesets.current_hash then
            Loader.container.Iter(fun score -> score.Data.Ruleset <- Content.Rulesets.current)
            scoring <- Content.Rulesets.current_hash
        Loader.container.Filter <- filterer()