namespace Interlude.Features.OptionsMenu.SystemSettings

open System
open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Content
open Interlude.Options
open Interlude.UI

type PerformanceSettingsPage() =
    inherit Page()

    let mutable closed = false
    let cycle = Animation.Counter(1500.0)
    let texture = Content.Texture "note"
    let screen_tear_alignment = config.SmartCapTearlinePosition |> Setting.trigger (fun v -> GameThread.tearline_position <- v) |> Setting.f32
    let framerate_multiplier = config.SmartCapFramerateMultiplier |> Setting.trigger (fun v -> GameThread.framerate_multiplier <- v)

    let is_windows = OperatingSystem.IsWindows()
    let show_tearline_settings () =
        is_windows
        && not GameThread.uses_compositor
        && config.RenderMode.Value = FrameLimit.Smart
        && config.SmartCapAntiJitter.Value

    let mutable msaa_restart = false

    override this.Content() =
        page_container()
        |+ PageSetting(
            %"system.framelimit",
            SelectDropdown(
                [|
                    FrameLimit.Unlimited, %"system.framelimit.unlimited"
                    FrameLimit.Smart, %"system.framelimit.smart"
                |],
                config.RenderMode |> Setting.trigger (ignore >> config.Apply)
            )
        )
            .Help(Help.Info("system.framelimit"))
            .Pos(0)
        |+ Text(%"system.framelimit.unlimited_warning",
            Color = K Colors.text_red,
            Position = page_position(2, 1, PageWidth.Full).ShrinkL(PAGE_LABEL_WIDTH),
            Align = Alignment.LEFT
        )
            .Conditional(fun () -> config.RenderMode.Value = FrameLimit.Unlimited)
        |+ PageSetting(
            %"system.cpu_saver",
            Checkbox(config.InputCPUSaver |> Setting.trigger (ignore >> config.Apply))
        )
            .Help(Help.Info("system.cpu_saver"))
            .Pos(3)
        |+ PageSetting(
            %"system.msaa",
            Selector([| 0, sprintf "%s (0x)" %"system.msaa.off"; 4, sprintf "%s (4x)" %"system.msaa.normal"; 16, sprintf "%s (16x)" %"system.msaa.fancy" |], config.MSAASamples |> Setting.trigger (fun _ -> msaa_restart <- true))
        )
            .Help(Help.Info("system.msaa"))
            .Pos(5)
        |+ Text(%"system.msaa.restart_warning",
            Color = K Colors.text_red,
            Position = page_position(7, 1, PageWidth.Full).ShrinkL(PAGE_LABEL_WIDTH),
            Align = Alignment.LEFT
        )
            .Conditional(fun () -> msaa_restart)

        |+ PageSetting(%"system.performance.antijitter",
            Checkbox(
                config.SmartCapAntiJitter
                |> Setting.trigger (fun v -> GameThread.anti_jitter <- v)
            )
        )
            .Help(Help.Info("system.performance.antijitter"))
            .Pos(8)
            .Conditional(fun () -> config.RenderMode.Value = FrameLimit.Smart)
        |+ PageSetting(%"system.performance.frame_multiplier",
            SelectDropdown([| 4.0, "4x"; 8.0, "8x"; 16.0, "16x"|], framerate_multiplier)
        )
            .Pos(10)
            .Conditional(show_tearline_settings)
        |+ PageSetting(%"system.performance.screen_tear_alignment",
            Slider.Percent(screen_tear_alignment)
        )
            .Pos(12)
            .Conditional(show_tearline_settings)
        |+ Text(%"system.performance.screen_tear_alignment.hint",
            Color = K Colors.text,
            Position = page_position(14, 1, PageWidth.Full).ShrinkL(PAGE_LABEL_WIDTH),
            Align = Alignment.LEFT
        )
            .Conditional(show_tearline_settings)
        :> Widget

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        cycle.Update elapsed_ms

    override this.Draw() =
        base.Draw()
        if closed then () else
        Render.rect
            (this.Bounds.SliceR(200.0f).Shrink(20.0f))
            (Color.FromHsv(float32 cycle.Progress, 0.5f, 1.0f))

        for i = 0 to 10 do
            let anti_jitter = GameThread.frame_compensation () / 1.0f<ms / rate>
            let y = (float32 cycle.Progress + (float32 i / 10.0f)) % 1.0f
            Render.sprite
                (Rect.Box(this.Bounds.Right - 300.0f, this.Bounds.Top - 100.0f, 100.0f, 100.0f).Translate(0.0f, (this.Bounds.Height + 100.0f) * y + anti_jitter))
                Color.White
                texture

    override this.OnClose() = closed <- true

    override this.Title = %"system.performance"