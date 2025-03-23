namespace Interlude.Features.Import

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data
open Prelude.Data.Library.Imports
open Interlude.UI
open Interlude.Features.Tables.Browser

type TrackedTask(label: string) =
    let mutable progress: TaskProgress = Generic %"import.waiting"

    let start = Timestamp.now()

    let mutable completed = ValueNone

    do Logging.Debug "Queuing task '%s'" label

    member this.Progress
        with get() = lock this (fun () -> progress)
        and set v = lock this (fun () ->
            progress <- v
            match progress with
            | Faulted
            | Complete ->
                let now = Timestamp.now()
                Logging.Debug "Task '%s' complete after %s (including time waiting for its turn)" label (format_duration_ms (now - start))
                completed <- ValueSome now
            | _ -> ()
        )
    member val StartedAt : int64 = start
    member this.CompletedAt : int64 voption = lock this (fun () -> completed)
    member this.Label = label

module TaskTracking =

    let private list = ResizeArray<TrackedTask>()

    let add (label: string) : TrackedTask =
        assert(GameThread.is_game_thread())

        let status = TrackedTask(label)
        list.Add(status)
        status

    let DELAY_AFTER_COMPLETE = 3000L
    let FADE_TIME = 300L

    let WIDTH = 600.0f
    let HEIGHT = 45.0f
    let SPACING = 15.0f

    let rec private fmt_progress (progress: TaskProgress) : string * Color =
        match progress with
        | Generic status -> status, Colors.grey_2
        | Downloading percent -> sprintf "%s %.0f%%" Icons.DOWNLOAD (percent * 100.0f), Colors.grey_1
        | Processing (count, total) -> sprintf "%s %i / %i" Icons.DATABASE count total, Colors.grey_1
        | Complete -> Icons.CHECK, Colors.green_accent
        | Faulted -> Icons.ALERT_CIRCLE, Colors.red_accent
        | Nested (label, count, total, inner) ->
            let text, color = fmt_progress inner
            sprintf "%s (%i / %i) | %s" label count total text, color

    let private draw_task (now: int64, opacity: float32, x: float32, y: float32, task: TrackedTask) : float32 =
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

        let bounds = Rect.FromSize(x, y, WIDTH, HEIGHT)
        Render.border Style.PADDING bounds (Colors.cyan_accent.O4a alpha)
        Render.rect bounds (Colors.cyan_shadow.O4a alpha)

        let progress = task.Progress

        let top_text = task.Label
        Text.fill_b(Style.font, top_text, bounds.SlicePercentL(0.7f).ShrinkB(15.0f).ShrinkX(10.0f), (Colors.white.O4a alpha, Colors.shadow_2.O4a alpha), Alignment.LEFT)
        let bottom_text, bottom_text_color = fmt_progress progress
        Text.fill_b(Style.font, bottom_text, bounds.SlicePercentR(0.3f).ShrinkB(15.0f).ShrinkX(10.0f), (bottom_text_color.O4a alpha, Colors.shadow_2.O4a alpha), Alignment.RIGHT)

        let bar_bounds = bounds.SliceB(15.0f).Shrink(5.0f)

        match progress with
        | Generic _ -> Render.rect bar_bounds (Colors.shadow_2.O4a alpha)
        | Downloading p ->
            Render.rect bar_bounds (Colors.shadow_2.O4a alpha)
            Render.rect (bar_bounds.SlicePercentL p) (Colors.cyan.O4a alpha)
        | Processing (i, total) ->
            Render.rect bar_bounds (Colors.cyan.O4a alpha)
            Render.rect (bar_bounds.SlicePercentL (float32 i / float32 total)) (Colors.green_accent.O4a alpha)
        | Complete -> Render.rect bar_bounds (Colors.green_accent.O4a alpha)
        | Faulted -> Render.rect bar_bounds (Colors.red_accent.O4a alpha)
        | Nested (_, i, total, _) ->
            Render.rect bar_bounds (Colors.cyan.O4a alpha)
            Render.rect (bar_bounds.SlicePercentL (float32 i / float32 total)) (Colors.green_accent.O4a alpha)

        y + (HEIGHT + SPACING) * fade_in_out

    let draw (bounds: Rect, opacity: float32) : unit =
        let now = Timestamp.now()

        let bounds =
            let MIN_WIDTH = WIDTH + SPACING * 2.0f
            if bounds.Width < MIN_WIDTH then
                bounds.SliceL(MIN_WIDTH)
            else
                bounds

        let a = int (255.0f * opacity)
        Render.rect (bounds.SlicePercentT opacity) (Colors.shadow_2.O3a a)

        let x = bounds.Left + (bounds.Width - WIDTH) * 0.5f
        let mutable y = bounds.Top + 70.0f
        Text.fill_b(Style.font, sprintf "%s: %i" %"imports.queued" list.Count, bounds.SliceT(60.0f).Shrink(5.0f), (Colors.white.O4a a, Colors.shadow_2.O4a a), Alignment.CENTER)
        for task in list do
            y <- draw_task(now, opacity, x, y, task)

    let in_progress () : bool =
        import_queue.Status <> Async.QueueStatus.Idle
        || general_task_queue.Status <> Async.QueueStatus.Idle
        || TableDownloader.download_service.Status <> Async.QueueStatus.Idle

    let current_progress () : TaskProgress =
        list
        |> Seq.map (_.Progress)
        |> Seq.tryPick (function Complete -> None | otherwise -> Some otherwise)
        |> Option.defaultValue Complete

/// Displayed below the import button when a task is in progress
type TaskProgressMiniBar() =
    inherit StaticWidget(NodeType.None)

    let animation = Animation.Counter(1500.0)
    let fade = Animation.Fade 0.0f

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        animation.Update elapsed_ms
        fade.Target <- if TaskTracking.in_progress() then 1.0f else 0.0f
        fade.Update elapsed_ms

    override this.Draw() =
        let alpha = fade.Alpha
        if alpha > 0 then

            match TaskTracking.current_progress() with
            | Downloading p ->
                Render.rect this.Bounds (Colors.shadow_2.O4a alpha)
                Render.rect (this.Bounds.SlicePercentL p) (Colors.cyan.O4a alpha)
            | Processing (i, total) ->
                Render.rect this.Bounds (Colors.cyan.O4a alpha)
                Render.rect (this.Bounds.SlicePercentL (float32 i / float32 total)) (Colors.green_accent.O4a alpha)
            | Complete -> Render.rect this.Bounds (Colors.green_accent.O4a alpha)
            | Faulted -> Render.rect this.Bounds (Colors.red_accent.O4a alpha)
            | Nested (_, i, total, _) ->
                Render.rect this.Bounds (Colors.cyan.O4a alpha)
                Render.rect (this.Bounds.SlicePercentL (float32 i / float32 total)) (Colors.green_accent.O4a alpha)
            | Generic _ ->
                let tick_width = this.Bounds.Width * 0.2f

                let pos =
                    -tick_width
                    + (this.Bounds.Width + tick_width) * float32 animation.Progress

                Render.rect_edges
                    (this.Bounds.Left + max 0.0f pos)
                    this.Bounds.Top
                    (this.Bounds.Left + min this.Bounds.Width (pos + tick_width))
                    this.Bounds.Bottom
                    (Colors.white.O4a alpha)