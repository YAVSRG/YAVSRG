namespace Interlude.UI.Screens.LevelSelect

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
open Interlude.Gameplay
open Interlude.Options
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Containers

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
        inherit Widget1()

        let fade = Animation.Fade 0.0f

        do
            data.Physical |> ignore
            data.Lamp |> ignore

            let color = fun () -> let a = fade.Alpha in (Color.FromArgb(a, Color.White), Color.FromArgb(a, Color.Black))

            this.Position( Position.SliceTop 75.0f )
            |-+ TextBox((fun() -> data.Scoring.FormatAccuracy()), color, 0.0f)
                .Position { Left = 0.0f %+ 5.0f; Top = 0.0f %+ 0.0f; Right = 0.5f %+ 0.0f; Bottom = 0.6f %+ 0.0f }
            |-+ TextBox((fun () -> sprintf "%s  •  %ix  •  %.2f" (data.Ruleset.LampName data.Lamp) data.Scoring.State.BestCombo data.Physical), color, 0.0f)
                .Position { Left = 0.0f %+ 5.0f; Top = 0.6f %+ 0.0f; Right = 0.5f %+ 0.0f; Bottom = 1.0f %+ 0.0f }
            |-+ TextBox(K (formatTimeOffset(DateTime.Now - data.ScoreInfo.time)), color, 1.0f)
                .Position { Left = 0.5f %+ 0.0f; Top = 0.6f %+ 0.0f; Right = 1.0f %- 5.0f; Bottom = 1.0f %+ 0.0f }
            |-+ TextBox(K data.Mods, color, 1.0f)
                .Position { Left = 0.5f %+ 0.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %- 5.0f; Bottom = 0.6f %+ 0.0f }
            |-+ Clickable(
                    ( fun () -> 
                        Screen.changeNew 
                            (fun () -> new Screens.Score.Screen(data, BestFlags.Default) :> Screen.T)
                            Screen.Type.Score
                            Screen.TransitionFlag.Default
                    ), ignore )
            |-* fade
            |=* Animation.seq [Animation.Delay 150.0 :> Animation; Animation.Action (fun () -> let (l, t, r, b) = this.Anchors in l.Snap(); t.Snap(); r.Snap(); b.Snap(); fade.Target <- 1.0f)]

        override this.Draw() =
            Draw.rect this.Bounds (Style.accentShade(int (100.0f * fade.Value), 0.5f, 0.0f))
            base.Draw()
        member this.Data = data

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if Mouse.hover this.Bounds && (!|"delete").Tapped() then
                let scoreName = sprintf "%s | %s" (data.Scoring.FormatAccuracy()) (data.Lamp.ToString())
                Tooltip.callback (
                    (!|"delete"),
                    Localisation.localiseWith [scoreName] "misc.delete",
                    NotificationType.Warning,
                    fun () ->
                        Chart.saveData.Value.Scores.Remove data.ScoreInfo |> ignore
                        LevelSelect.refresh <- true
                        Notification.add (Localisation.localiseWith [scoreName] "notification.deleted", NotificationType.Info)
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

        let reload (container: FlowContainer) =
            let worker =
                { new Async.SingletonWorkerSeq<T, ScoreInfoProvider>() with
                    member this.Handle(req: T) =
                        match req.ChartSaveData with
                        | None -> Seq.empty
                        | Some d ->
                            container.Synchronized(container.Clear)
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
                        container.Synchronized(fun () -> container.Add sc)
                    member this.JobCompleted(req: T) =
                        match req.NewBests with
                        | None -> ()
                        | Some b ->
                            container.Synchronized( fun () -> 
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
    inherit Selectable()

    let mutable count = -1

    let mutable chart = ""
    let mutable scoring = ""
    let ls = new ListSelectable(true)

    let filter = Setting.simple Filter.All
    let sort = Setting.map enum int options.ScoreSortMode

    let sorter() : Comparison<Widget1> =
        match sort.Value with
        | Sort.Accuracy -> Comparison(fun b a -> (a :?> ScoreCard).Data.Scoring.Value.CompareTo((b :?> ScoreCard).Data.Scoring.Value))
        | Sort.Performance -> Comparison(fun b a -> (a :?> ScoreCard).Data.Physical.CompareTo((b :?> ScoreCard).Data.Physical))
        | Sort.Time
        | _ -> Comparison(fun b a -> (a :?> ScoreCard).Data.ScoreInfo.time.CompareTo((b :?> ScoreCard).Data.ScoreInfo.time))

    let filterer() : Widget1 -> bool =
        match filter.Value with
        | Filter.CurrentRate -> (fun a -> (a :?> ScoreCard).Data.ScoreInfo.rate = rate.Value)
        | Filter.CurrentPlaystyle -> (fun a -> (a :?> ScoreCard).Data.ScoreInfo.layout = options.Playstyles.[(a :?> ScoreCard).Data.ScoreInfo.keycount - 3])
        | Filter.CurrentMods -> (fun a -> (a :?> ScoreCard).Data.ScoreInfo.selectedMods = selectedMods.Value)
        | _ -> K true


    let flowContainer = new FlowContainer(Sort = sorter(), Filter = filterer())

    let loader = Loader.reload flowContainer

    do
        this
        |-+ flowContainer.Position( Position.TrimTop(10.0f).TrimBottom(50.0f) )
        |-+ (
                ls
                |-+ StylishButton.FromEnum("Sort",
                        sort |> Setting.trigger (fun _ -> flowContainer.Sort <- sorter()),
                        Style.main 100, TiltLeft = false )
                    .Tooltip(L"levelselect.scoreboard.sort.tooltip")
                    .Position { Left = 0.0f %+ 0.0f; Top = 1.0f %- 45.0f; Right = 0.25f %- 15.0f; Bottom = 1.0f %- 5.0f }
                |-+ StylishButton.FromEnum("Filter",
                        filter |> Setting.trigger (fun _ -> this.Refresh()),
                        Style.main 90 )
                    .Tooltip(L"levelselect.scoreboard.filter.tooltip")
                    .Position { Left = 0.25f %+ 10.0f; Top = 1.0f %- 45.0f; Right = 0.5f %- 15.0f; Bottom = 1.0f %- 5.0f }
                |-+ StylishButton(
                        (fun () -> Setting.app WatcherSelection.cycleForward options.Rulesets; LevelSelect.refresh <- true),
                        (fun () -> ruleset.Name),
                        Style.main 80 )
                    .Tooltip(L"levelselect.scoreboard.ruleset.tooltip")
                    .Position { Left = 0.5f %+ 10.0f; Top = 1.0f %- 45.0f; Right = 0.75f %- 15.0f; Bottom = 1.0f %- 5.0f }
                |-+ StylishButton(
                        this.Refresh,
                        K <| Localisation.localise "levelselect.scoreboard.storage.local",
                        Style.main 70, TiltRight = false ) //nyi
                    .Tooltip(L"levelselect.scoreboard.storage.tooltip")
                    .Position { Left = 0.75f %+ 10.0f; Top = 1.0f %- 45.0f; Right = 1.0f %- 15.0f; Bottom = 1.0f %- 5.0f }
            )
        |=+ (
                let noLocalScores = L"levelselect.scoreboard.empty"
                TextBox((fun () -> if count = 0 then noLocalScores else ""), K (Color.White, Color.Black), 0.5f)
                    .Position { Left = 0.0f %+ 50.0f; Top = 0.3f %+ 0.0f; Right = 1.0f %- 50.0f; Bottom = 0.5f %+ 0.0f }
            )

    member this.Refresh() =
        let h = match Chart.cacheInfo with Some c -> c.Hash | None -> ""
        if (match Chart.saveData with None -> false | Some d -> let v = d.Scores.Count <> count in count <- d.Scores.Count; v) || h <> chart then
            chart <- h
            loader() |> ignore
        elif scoring <> rulesetId then
            let s = getCurrentRuleset()
            for c in flowContainer.Children do (c :?> ScoreCard).Data.Ruleset <- s
            scoring <- rulesetId
        flowContainer.Filter <- filterer()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        this.HoverChild <- None