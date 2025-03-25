namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User
open Interlude.Content
open Interlude.UI
open Interlude.Options
open Interlude.Features.Gameplay

[<RequireQualifiedAccess>]
type private Sort =
    | Time = 0
    | Performance = 1
    | Accuracy = 2

[<RequireQualifiedAccess>]
type private Filter =
    | None = 0
    | CurrentRate = 1
    | CurrentMods = 2

type Scoreboard(display: Setting<Display>) =
    inherit Container(NodeType.None)

    let mutable count = 0
    let mutable loading = true

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

    let container = FlowContainer.Vertical<ScoreCard>(50.0f, Spacing = Style.PADDING)

    let scroll_container =
        ScrollContainer(container, Position = Position.ShrinkT(50.0f))

    do
        LocalScores.score_loaded.Add (fun score_info -> score_info |> ScoreCard |> container.Add; count <- count + 1)
        LocalScores.scores_loaded.Add (fun () -> loading <- false)

    let refresh_filter () =
        container.Filter <- filterer ()

    override this.Init(parent: Widget) =
        SelectedChart.on_chart_change_started.Add (fun _ -> container.Iter(fun s -> s.FadeOut()); loading <- true)
        SelectedChart.on_chart_change_finished.Add (fun _ -> container.Clear(); count <- 0)
        Rulesets.on_changed.Add (fun _ -> GameThread.defer (fun () -> container.Sort <- sorter ()))
        SelectedChart.on_chart_update_finished.Add (fun _ -> refresh_filter())
        Gameplay.score_deleted.Add (fun timestamp -> container.Iter(fun sc -> if sc.Data.TimePlayed = timestamp then GameThread.defer (fun () -> container.Remove sc)))
        container.Sort <- sorter ()

        this
        |+ LeaningButton(
            %"levelselect.info.scoreboard",
            (fun () -> display.Set Display.Online),
            Palette.MAIN_100
        )
            .Hotkey("scoreboard_storage")
            .LeanLeft(false)
            .Position(
                Position
                    .SliceT(LeaningButton.HEIGHT)
                    .SlicePercentL(0.33f)
                    .ShrinkR(LeaningButton.LEAN_AMOUNT)
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
                sort |> Setting.trigger (fun _ -> container.Sort <- sorter ()),
                !%Palette.DARK_100,
                Hotkey = "scoreboard_sort",
                Position =
                    Position
                        .SliceT(LeaningButton.HEIGHT)
                        .SlicePercentL(0.33f, 0.33f)
                        .ShrinkR(LeaningButton.LEAN_AMOUNT)
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
                filter |> Setting.trigger (fun _ -> refresh_filter()),
                !%Palette.MAIN_100,
                Hotkey = "scoreboard_filter",
                TiltRight = false,
                Position =
                    Position
                        .SliceT(LeaningButton.HEIGHT)
                        .ShrinkPercentL(0.66f)
            )
            .Help(Help.Info("levelselect.info.scoreboard.filter", "scoreboard_filter"))
        |+ scroll_container
        |+ HotkeyListener(
            "scoreboard",
            fun () ->
                if container.Focused then
                    Selection.clear ()
                else
                    container.Focus false
        )
        |* EmptyState(Icons.WIND, %"levelselect.info.scoreboard.empty", Subtitle = %"levelselect.info.scoreboard.empty.subtitle")
            .Conditional(fun () -> not loading && count = 0)
        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        LocalScores.score_loader.Join()

    member this.ModsChanged() = refresh_filter()