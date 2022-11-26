namespace Interlude.Features.LevelSelect

open System
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Scores
open Prelude.Data.Charts.Caching
open Prelude.Scoring
open Prelude.Charts.Formats.Interlude
open Interlude.UI
open Interlude.Utils
open Interlude.Options
open Interlude.UI.Components
open Interlude.UI.Menu
open Interlude.Features.Gameplay
open Interlude.Features.Score

type ScoreContextMenu(score: ScoreInfoProvider) as this =
    inherit Page()

    do
        this.Content(
            column()
            |+ PrettyButton("score.delete", (fun () -> ScoreContextMenu.ConfirmDeleteScore(score, true)), Icon = Icons.delete).Pos(200.0f)
            |+ PrettyButton("score.watchreplay", (fun () -> ScoreScreenHelpers.watchReplay(score.ScoreInfo.rate, score.ReplayData); Menu.Back()), Icon = Icons.preview).Pos(280.0f)
            |+ PrettyButton("score.challenge", (fun () -> Tree.challengeScore(score.ScoreInfo.rate, score.ReplayData); Menu.Back()), Icon = Icons.goal).Pos(360.0f)
        )
    override this.Title = sprintf "%s | %s" (score.Scoring.FormatAccuracy()) (score.Lamp.ToString())
    override this.OnClose() = ()
    
    static member ConfirmDeleteScore(score, is_submenu) =
        let scoreName = sprintf "%s | %s" (score.Scoring.FormatAccuracy()) (score.Lamp.ToString())
        ConfirmPage(
            Localisation.localiseWith [scoreName] "misc.confirmdelete",
            fun () ->
                Chart.saveData.Value.Scores.Remove score.ScoreInfo |> ignore
                LevelSelect.refresh <- true
                Notifications.add (Localisation.localiseWith [scoreName] "notification.deleted", NotificationType.Info)
                if is_submenu then Menu.Back()
        ) |> Menu.ShowPage

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
        inherit StaticContainer(NodeType.None) // todo: make navigable using arrow keys

        let fade = Animation.Fade(0.0f, Target = 1.0f)
        let animation = Animation.seq [Animation.Delay 150; fade]

        do
            ignore data.Physical
            ignore data.Lamp

            let text_color = fun () -> let a = fade.Alpha in (Color.FromArgb(a, Color.White), Color.FromArgb(a, Color.Black))
            let text_subcolor = fun () -> (Color.FromArgb(int (fade.Value * 200.0f), Color.FromArgb(230, 230, 230)), Color.FromArgb(fade.Alpha, Color.Black))

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
                Position = { Left = 0.0f %+ 5.0f; Top = 0.6f %+ 0.0f; Right = 0.5f %+ 0.0f; Bottom = 1.0f %+ 0.0f })

            |+ Text(
                K (formatTimeOffset(DateTime.Now - data.ScoreInfo.time)),
                Color = text_subcolor,
                Align = Alignment.RIGHT,
                Position = { Left = 0.5f %+ 0.0f; Top = 0.6f %+ 0.0f; Right = 1.0f %- 5.0f; Bottom = 1.0f %+ 0.0f })

            |+ Text(
                K data.Mods,
                Color = text_color,
                Align = Alignment.RIGHT,
                Position = { Left = 0.5f %+ 0.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %- 5.0f; Bottom = 0.6f %+ 0.0f })

            |* Clickable((fun () -> 
                Screen.changeNew 
                    (fun () -> new ScoreScreen(data, BestFlags.Default) :> Screen)
                    Screen.Type.Score
                    Transitions.Flags.Default),
                OnRightClick = fun () -> ScoreContextMenu data |> Menu.ShowPage)

        override this.Draw() =
            Draw.rect this.Bounds (Style.color (int (150.0f * fade.Value), 0.5f, 0.2f))
            base.Draw()

        member this.Data = data

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            animation.Update elapsedTime
            if Mouse.hover this.Bounds && (!|"delete").Tapped() then ScoreContextMenu.ConfirmDeleteScore(data, false)

    module Loader =

        type private T =
            {
                RulesetId: string
                Ruleset: Ruleset
                CurrentChart: Chart
                ChartSaveData: ChartSaveData option
                mutable NewBests: Bests option
            }

        let reload (container: FlowContainer.Vertical<ScoreCard>) =
            let worker =
                { new Async.SwitchServiceSeq<T, ScoreInfoProvider>() with
                    member this.Handle(req: T) =
                        match req.ChartSaveData with
                        | None -> Seq.empty
                        | Some d ->
                            sync container.Clear
                            seq { 
                                for score in d.Scores do
                                    let s = ScoreInfoProvider(score, req.CurrentChart, req.Ruleset)
                                    req.NewBests <- Some (
                                        match req.NewBests with
                                        | None -> Bests.create s
                                        | Some b -> fst(Bests.update s b))
                                    yield s
                            }
                    member this.Callback(score: ScoreInfoProvider) =
                        let sc = ScoreCard score
                        sync(fun () -> container.Add sc)
                    member this.JobCompleted(req: T) =
                        match req.NewBests with
                        | None -> ()
                        | Some b ->
                            sync( fun () -> 
                                if not (req.ChartSaveData.Value.Bests.ContainsKey req.RulesetId) || b <> req.ChartSaveData.Value.Bests[req.RulesetId] then
                                    Tree.updateDisplay()
                                req.ChartSaveData.Value.Bests[req.RulesetId] <- b
                            )
                }
            fun () ->
                worker.Request
                    {
                        RulesetId = rulesetId
                        Ruleset = ruleset
                        CurrentChart = Chart.current.Value
                        ChartSaveData = Chart.saveData
                        NewBests = None
                    }

open Scoreboard

type Scoreboard() as this =
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

    let flowContainer =  FlowContainer.Vertical(75.0f, Spacing = Style.padding, Sort = sorter(), Filter = filterer())
    let scrollContainer = ScrollContainer.Flow(flowContainer, Margin = Style.padding, Position = Position.TrimTop(55.0f).TrimBottom(50.0f))

    let loader = Loader.reload flowContainer

    do
        this
        |+ StylishButton(
            this.Refresh,
            K <| Localisation.localise "levelselect.scoreboard.storage.local",
            Style.main 100,
            Hotkey = "scoreboard_storage",
            TiltLeft = false)
            .Tooltip(L"levelselect.scoreboard.storage.tooltip", "scoreboard_storage")
            .WithPosition { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 0.0f; Right = 0.33f %- 25.0f; Bottom = 0.0f %+ 50.0f }
        |+ StylishButton.Selector(
            Icons.sort,
            [|
                Sort.Accuracy, L"levelselect.scoreboard.sort.accuracy"
                Sort.Performance, L"levelselect.scoreboard.sort.performance"
                Sort.Time, L"levelselect.scoreboard.sort.time"
            |],
            sort |> Setting.trigger (fun _ -> flowContainer.Sort <- sorter()),
            Style.dark 100,
            Hotkey = "scoreboard_sort")
            .Tooltip(L"levelselect.scoreboard.sort.tooltip", "scoreboard_sort")
            .WithPosition { Left = 0.33f %+ 0.0f; Top = 0.0f %+ 0.0f; Right = 0.66f %- 25.0f; Bottom = 0.0f %+ 50.0f }
        |+ StylishButton.Selector(
            Icons.filter,
            [|
                Filter.None, L"levelselect.scoreboard.filter.none"
                Filter.CurrentRate, L"levelselect.scoreboard.filter.currentrate"
                Filter.CurrentMods, L"levelselect.scoreboard.filter.currentmods"
            |],
            filter |> Setting.trigger (fun _ -> this.Refresh()),
            Style.main 100,
            Hotkey = "scoreboard_filter",
            TiltRight = false)
            .Tooltip(L"levelselect.scoreboard.filter.tooltip", "scoreboard_filter")
            .WithPosition { Left = 0.66f %+ 0.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %- 0.0f; Bottom = 0.0f %+ 50.0f }
        |+ scrollContainer
        |* Text( (let noLocalScores = L"levelselect.scoreboard.empty" in (fun () -> if count = 0 then noLocalScores else "")),
            Align = Alignment.CENTER,
            Position = { Left = 0.0f %+ 50.0f; Top = 0.3f %+ 0.0f; Right = 1.0f %- 50.0f; Bottom = 0.5f %+ 0.0f })
           

    member this.Refresh() =
        let h = match Chart.cacheInfo with Some c -> c.Hash | None -> ""
        if (match Chart.saveData with None -> false | Some d -> let v = d.Scores.Count <> count in count <- d.Scores.Count; v) || h <> chart then
            chart <- h
            loader() |> ignore
        elif scoring <> rulesetId then
            let s = getCurrentRuleset()
            flowContainer.Iter(fun score -> score.Data.Ruleset <- s)
            scoring <- rulesetId
        flowContainer.Filter <- filterer()