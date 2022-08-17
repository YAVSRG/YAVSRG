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
open Prelude.ChartFormats.Interlude
open Interlude.UI
open Interlude.Utils
open Interlude.Options
open Interlude.UI.Components
open Interlude.Features.Gameplay
open Interlude.Features.Score

module Scoreboard =

    type Sort =
    | Time = 0
    | Performance = 1
    | Accuracy = 2

    type Filter =
    | All = 0
    | CurrentRate = 1
    | CurrentPlaystyle = 2
    | CurrentMods = 3

    type ScoreCard(data: ScoreInfoProvider) as this =
        inherit StaticContainer(NodeType.None) // todo: make selectable with options for watching replay

        let fade = Animation.Fade(0.0f, Target = 1.0f)
        let animation = Animation.seq [Animation.Delay 150; fade]

        do
            data.Physical |> ignore
            data.Lamp |> ignore

            let color = fun () -> let a = fade.Alpha in (Color.FromArgb(a, Color.White), Color.FromArgb(a, Color.Black))

            this
            |+ Text(
                fun () -> data.Scoring.FormatAccuracy()
                ,
                Color = color,
                Align = Alignment.LEFT,
                Position = { Left = 0.0f %+ 5.0f; Top = 0.0f %+ 0.0f; Right = 0.5f %+ 0.0f; Bottom = 0.6f %+ 0.0f })

            |+ Text(
                fun () -> sprintf "%s  •  %ix  •  %.2f" (data.Ruleset.LampName data.Lamp) data.Scoring.State.BestCombo data.Physical
                ,
                Color = color,
                Align = Alignment.LEFT,
                Position = { Left = 0.0f %+ 5.0f; Top = 0.6f %+ 0.0f; Right = 0.5f %+ 0.0f; Bottom = 1.0f %+ 0.0f })

            |+ Text(
                K (formatTimeOffset(DateTime.Now - data.ScoreInfo.time)),
                Color = color,
                Align = Alignment.RIGHT,
                Position = { Left = 0.5f %+ 0.0f; Top = 0.6f %+ 0.0f; Right = 1.0f %- 5.0f; Bottom = 1.0f %+ 0.0f })

            |+ Text(
                K data.Mods,
                Color = color,
                Align = Alignment.RIGHT,
                Position = { Left = 0.5f %+ 0.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %- 5.0f; Bottom = 0.6f %+ 0.0f })

            |* Clickable(fun () -> 
                Screen.changeNew 
                    (fun () -> new ScoreScreen(data, BestFlags.Default) :> Screen.T)
                    Screen.Type.Score
                    Transitions.Flags.Default)

        override this.Draw() =
            Draw.rect this.Bounds (Style.color(int (100.0f * fade.Value), 0.5f, 0.0f))
            base.Draw()
        member this.Data = data

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            animation.Update elapsedTime
            if Mouse.hover this.Bounds && (!|"delete").Tapped() then
                let scoreName = sprintf "%s | %s" (data.Scoring.FormatAccuracy()) (data.Lamp.ToString())
                Notifications.callback (
                    (!|"delete"),
                    Localisation.localiseWith [scoreName] "misc.delete",
                    NotificationType.Warning,
                    fun () ->
                        Chart.saveData.Value.Scores.Remove data.ScoreInfo |> ignore
                        LevelSelect.refresh <- true
                        Notifications.add (Localisation.localiseWith [scoreName] "notification.deleted", NotificationType.Info)
                )

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
                { new Async.SingletonWorkerSeq<T, ScoreInfoProvider>() with
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
    let buttons = SwitchContainer.Row()

    let filter = Setting.simple Filter.All
    let sort = Setting.map enum int options.ScoreSortMode

    let sorter() : ScoreCard -> ScoreCard -> int =
        match sort.Value with
        | Sort.Accuracy -> fun b a -> a.Data.Scoring.Value.CompareTo(b.Data.Scoring.Value)
        | Sort.Performance -> fun b a -> a.Data.Physical.CompareTo(b.Data.Physical)
        | Sort.Time
        | _ -> fun b a -> a.Data.ScoreInfo.time.CompareTo(b.Data.ScoreInfo.time)

    let filterer() : ScoreCard -> bool =
        match filter.Value with
        | Filter.CurrentRate -> (fun a -> a.Data.ScoreInfo.rate = rate.Value)
        | Filter.CurrentPlaystyle -> (fun a -> a.Data.ScoreInfo.layout = options.Playstyles.[a.Data.ScoreInfo.keycount - 3])
        | Filter.CurrentMods -> (fun a -> a.Data.ScoreInfo.selectedMods = selectedMods.Value)
        | _ -> K true


    let flowContainer =  FlowContainer.Vertical(75.0f, Spacing = Style.padding, Sort = sorter(), Filter = filterer())
    let scrollContainer = ScrollContainer.Flow(flowContainer, Margin = Style.padding, Position = Position.TrimTop(10.0f).TrimBottom(50.0f))

    let loader = Loader.reload flowContainer

    do
        this
        |+ scrollContainer
        |+ (
                buttons
                |+ StylishButton.FromEnum(
                    "Sort",
                    sort |> Setting.trigger (fun _ -> flowContainer.Sort <- sorter()),
                    Style.main 100,
                    TiltLeft = false,
                    Position = { Left = 0.0f %+ 0.0f; Top = 1.0f %- 45.0f; Right = 0.25f %- 15.0f; Bottom = 1.0f %- 5.0f })
                    //.Tooltip(L"levelselect.scoreboard.sort.tooltip")
                |+ StylishButton.FromEnum(
                    "Filter",
                    filter |> Setting.trigger (fun _ -> this.Refresh()),
                    Style.main 90,
                    Position = { Left = 0.25f %+ 10.0f; Top = 1.0f %- 45.0f; Right = 0.5f %- 15.0f; Bottom = 1.0f %- 5.0f })
                    //.Tooltip(L"levelselect.scoreboard.filter.tooltip")
                |+ StylishButton(
                    (fun () -> Setting.app WatcherSelection.cycleForward options.Rulesets; LevelSelect.refresh <- true),
                    (fun () -> ruleset.Name),
                    Style.main 80,
                    Position = { Left = 0.5f %+ 10.0f; Top = 1.0f %- 45.0f; Right = 0.75f %- 15.0f; Bottom = 1.0f %- 5.0f })
                    //.Tooltip(L"levelselect.scoreboard.ruleset.tooltip")
                |+ StylishButton(
                    this.Refresh,
                    K <| Localisation.localise "levelselect.scoreboard.storage.local",
                    Style.main 70,
                    TiltRight = false,
                    Position = { Left = 0.75f %+ 10.0f; Top = 1.0f %- 45.0f; Right = 1.0f %- 15.0f; Bottom = 1.0f %- 5.0f })
                    //.Tooltip(L"levelselect.scoreboard.storage.tooltip")
           )
        |* (
                let noLocalScores = L"levelselect.scoreboard.empty"
                Text((fun () -> if count = 0 then noLocalScores else ""),
                    Align = Alignment.CENTER,
                    Position = { Left = 0.0f %+ 50.0f; Top = 0.3f %+ 0.0f; Right = 1.0f %- 50.0f; Bottom = 0.5f %+ 0.0f })
           )

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