namespace Interlude.Features.Import

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library.Imports
open Prelude.Data.Maintenance
open Prelude.Data.OsuClientInterop
open Prelude.Data.Library
open Interlude.UI
open Interlude.Features.Tables.Browser

type ImportStatus(label: string) =
    let mutable status = Generic %"import.waiting"

    let start = Timestamp.now()

    let mutable completed = ValueNone

    member this.Status
        with get() = lock this (fun () -> status)
        and set v = lock this (fun () ->
            status <- v
            match status with
            | Faulted
            | Complete -> completed <- ValueSome (Timestamp.now())
            | _ -> ()
        )
    member val StartedAt : int64 = start
    member this.CompletedAt : int64 voption = lock this (fun () -> completed)
    member this.Label = label

module ImportsInProgress =

    let private list = ResizeArray<ImportStatus>()

    let add (label: string) : ImportStatus =
        assert(GameThread.is_game_thread())

        let status = ImportStatus(label)
        list.Add(status)
        status

    let DELAY_AFTER_COMPLETE = 5000L
    let FADE_TIME = 1000L

    let WIDTH = 600.0f
    let HEIGHT = 45.0f
    let SPACING = 15.0f

    let rec private fmt_status (status: ImportProgress) : string * Color =
        match status with
        | Generic status -> status, Colors.grey_2
        | Downloading percent -> sprintf "%s %.0f%%" Icons.DOWNLOAD (percent * 100.0f), Colors.grey_1
        | Processing (count, total) -> sprintf "%s %i / %i" Icons.DATABASE count total, Colors.grey_1
        | Complete -> Icons.CHECK, Colors.green_accent
        | Faulted -> Icons.ALERT_CIRCLE, Colors.red_accent
        | Nested (label, count, total, inner) ->
            let text, color = fmt_status inner
            sprintf "%s (%i / %i) | %s" label count total text, color

    let private draw_task (now: int64, opacity: float32, y: float32, task: ImportStatus) : float32 =
        let fade_in_out =
            match task.CompletedAt with
            | ValueSome time ->
                let time_ago = now - time
                if time_ago > DELAY_AFTER_COMPLETE + FADE_TIME then GameThread.defer (fun () -> list.Remove task |> ignore)
                1.0f - (float32 (time_ago - DELAY_AFTER_COMPLETE) / float32 FADE_TIME |> max 0.0f |> min 1.0f)
            | ValueNone ->
                let age = now - task.StartedAt
                float32 age / float32 FADE_TIME |> max 0.0f |> min 1.0f

        let opacity = opacity * fade_in_out
        let alpha = 255f * opacity |> int |> max 0 |> min 255

        let bounds = Rect.Box(10.0f, y, WIDTH, HEIGHT)
        Render.border Style.PADDING bounds (Colors.cyan_accent.O4a alpha)
        Render.rect bounds (Colors.cyan_shadow.O4a alpha)

        let status = task.Status

        let top_text = task.Label
        Text.fill_b(Style.font, top_text, bounds.SlicePercentL(0.7f).ShrinkB(15.0f).ShrinkX(10.0f), (Colors.white.O4a alpha, Colors.shadow_2.O4a alpha), Alignment.LEFT)
        let bottom_text, bottom_text_color = fmt_status status
        Text.fill_b(Style.font, bottom_text, bounds.SlicePercentR(0.3f).ShrinkB(15.0f).ShrinkX(10.0f), (bottom_text_color.O4a alpha, Colors.shadow_2.O4a alpha), Alignment.RIGHT)

        let bar_bounds = bounds.SliceB(15.0f).Shrink(5.0f)

        match status with
        | Generic _ -> Render.rect bar_bounds Colors.shadow_2
        | Downloading p ->
            Render.rect bar_bounds Colors.shadow_2
            Render.rect (bar_bounds.SlicePercentL p) Colors.cyan
        | Processing (i, total) ->
            Render.rect bar_bounds Colors.cyan
            Render.rect (bar_bounds.SlicePercentL (float32 i / float32 total)) Colors.green_accent
        | Complete -> Render.rect bar_bounds Colors.green_accent
        | Faulted -> Render.rect bar_bounds Colors.red_accent
        | Nested (_, i, total, _) ->
            Render.rect bar_bounds Colors.cyan
            Render.rect (bar_bounds.SlicePercentL (float32 i / float32 total)) Colors.green_accent

        y + (HEIGHT + SPACING) * fade_in_out

    let draw (opacity: float32) =
        let now = Timestamp.now()
        let mutable y = Toolbar.HEIGHT + 10.0f
        for task in list do
            y <- draw_task(now, opacity, y, task)

    let import_in_progress () =
        import_queue.Status <> Async.ServiceStatus.Idle
        || TableDownloader.download_service.Status <> Async.ServiceStatus.Idle
        || Scores.import_osu_scores_service.Status <> Async.ServiceStatus.Idle
        || PersonalBests.recalculate_service.Status <> Async.ServiceStatus.Idle
        || ChartDatabase.vacuum.Status <> Async.ServiceStatus.Idle
        || ChartDatabase.recalculate_data.Status <> Async.ServiceStatus.Idle