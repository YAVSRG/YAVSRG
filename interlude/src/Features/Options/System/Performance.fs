namespace Interlude.Features.OptionsMenu.SystemSettings

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Content
open Interlude.Options
open Interlude.UI.Menu

type PerformanceSettingsPage() =
    inherit Page()

    let mutable closed = false
    let cycle = Animation.Counter(1500.0)
    let texture = Content.Texture "note"
    let screen_tear_alignment = config.SmartCapTearlinePosition |> Setting.trigger (fun v -> tearline_position <- v) |> Setting.f32
    let framerate_multiplier = config.SmartCapFramerateMultiplier |> Setting.trigger (fun v -> framerate_multiplier <- v)

    override this.Content() = 
        page_container()
        |+ PageSetting(
            "system.framelimit",
            SelectDropdown.FromEnum(config.RenderMode |> Setting.trigger (fun _ -> Window.defer (Window.ApplyConfig config)))
        )
            .Tooltip(Tooltip.Info("system.framelimit"))
            .Pos(0)
        |+ Text(%"system.framelimit.unlimited_warning", 
            Color = K Colors.text_red,
            Position = pretty_pos(2, 1, PageWidth.Full).TrimLeft(PRETTYTEXTWIDTH),
            Align = Alignment.LEFT
        )
            .Conditional(fun () -> config.RenderMode.Value = FrameLimit.Unlimited)
        |+ PageSetting("system.performance.antijitter", 
            Checkbox(
                config.SmartCapAntiJitter
                |> Setting.trigger (fun v -> anti_jitter <- v)
            )
        )
            .Tooltip(Tooltip.Info("system.performance.antijitter"))
            .Pos(2)
            .Conditional(fun () -> config.RenderMode.Value = FrameLimit.Smart)
        |+ PageSetting("system.performance.screen_tear_alignment", 
            Slider.Percent(screen_tear_alignment)
        )
            .Pos(4)
            .Conditional(fun () -> config.RenderMode.Value = FrameLimit.Smart && config.SmartCapAntiJitter.Value && config.WindowMode.Value = WindowType.Fullscreen)
        |+ Text(%"system.performance.screen_tear_alignment.hint", 
            Color = K Colors.text,
            Position = pretty_pos(6, 1, PageWidth.Full).TrimLeft(PRETTYTEXTWIDTH),
            Align = Alignment.LEFT
        )
            .Conditional(fun () -> config.RenderMode.Value = FrameLimit.Smart && config.SmartCapAntiJitter.Value && config.WindowMode.Value = WindowType.Fullscreen)
        |+ PageSetting("system.performance.frame_multiplier", 
            SelectDropdown([| 4.0, "4x"; 8.0, "8x"; 16.0, "16x"|], framerate_multiplier)
        )
            .Pos(7)
            .Conditional(fun () -> config.RenderMode.Value = FrameLimit.Smart && config.WindowMode.Value = WindowType.Fullscreen)
        :> Widget

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        cycle.Update elapsed_ms

    override this.Draw() =
        base.Draw()
        if closed then () else
        Draw.rect 
            (this.Bounds.SliceRight(200.0f).Shrink(20.0f))
            (Color.FromHsv(float32 (cycle.Time / cycle.Interval), 0.5f, 1.0f))

        for i = 0 to 10 do
            let anti_jitter = Performance.frame_compensation () / 1.0f<ms>
            let y = (float32 (cycle.Time / cycle.Interval) + (float32 i / 10.0f)) % 1.0f
            Draw.sprite 
                (Rect.Box(this.Bounds.Right - 300.0f, this.Bounds.Top - 100.0f, 100.0f, 100.0f).Translate(0.0f, (this.Bounds.Height + 100.0f) * y + anti_jitter))
                Color.White
                texture

    override this.OnClose() = closed <- true

    override this.Title = %"system.performance"
