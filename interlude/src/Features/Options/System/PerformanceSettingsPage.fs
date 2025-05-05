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

    let mutable pending_custom_frame_limit = false
    let custom_frame_limit_box = NumberEntry.Create(config.CustomFrameLimit |> Setting.trigger (fun _ -> pending_custom_frame_limit <- true))

    member this.SaveChanges() =
        if pending_custom_frame_limit then
            pending_custom_frame_limit <- false
            config.Apply()
        closed <- true

    override this.Content() =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(
                    %"system.framelimit",
                    SelectDropdown(
                        [|
                            FrameLimit.Unlimited, %"system.framelimit.unlimited"
                            FrameLimit.Smart, %"system.framelimit.smart"
                            FrameLimit.Custom, %"system.framelimit.custom"
                        |],
                        config.RenderMode |> Setting.trigger (ignore >> config.Apply)
                    )
                )
                    .Help(Help.Info("system.framelimit"))
                    .Pos(0),
                Text(%"system.framelimit.unlimited_warning")
                    .Color(Colors.text_red)
                    .Align(Alignment.LEFT)
                    .Position(page_position(2, 1, PageWidth.Full).ShrinkL(PAGE_LABEL_WIDTH))
                    .Conditional(fun () -> config.RenderMode.Value = FrameLimit.Unlimited),

                PageSetting(%"system.performance.antijitter",
                    Checkbox(
                        config.SmartCapAntiJitter
                        |> Setting.trigger (fun v -> GameThread.anti_jitter <- v)
                    )
                )
                    .Help(Help.Info("system.performance.antijitter"))
                    .Pos(2)
                    .Conditional(fun () -> config.RenderMode.Value = FrameLimit.Smart),

                PageSetting(%"system.performance.custom_frame_limit", custom_frame_limit_box)
                    .Pos(2)
                    .Conditional(fun () -> config.RenderMode.Value = FrameLimit.Custom),

                PageSetting(
                    %"system.cpu_saver",
                    Checkbox(config.InputCPUSaver |> Setting.trigger (ignore >> config.Apply))
                )
                    .Help(Help.Info("system.cpu_saver"))
                    .Pos(4),
                PageSetting(
                    %"system.msaa",
                    Selector([| 0, sprintf "%s (0x)" %"system.msaa.off"; 4, sprintf "%s (4x)" %"system.msaa.normal"; 16, sprintf "%s (16x)" %"system.msaa.fancy" |], config.MSAASamples |> Setting.trigger (fun _ -> msaa_restart <- true))
                )
                    .Help(Help.Info("system.msaa"))
                    .Pos(6),
                Text(%"system.msaa.restart_warning")
                    .Color(Colors.text_red)
                    .Position(page_position(8, 1, PageWidth.Full).ShrinkL(PAGE_LABEL_WIDTH))
                    .Align(Alignment.LEFT)
                    .Conditional(fun () -> msaa_restart)
            )
            .WithConditional(
                show_tearline_settings,
                PageSetting(%"system.performance.frame_multiplier",
                    SelectDropdown([| 4.0, "4x"; 8.0, "8x"; 16.0, "16x"|], framerate_multiplier)
                )
                    .Pos(10),
                PageSetting(%"system.performance.screen_tear_alignment",
                    Slider.Percent(screen_tear_alignment)
                )
                    .Pos(12),
                Text(%"system.performance.screen_tear_alignment.hint")
                    .Color(Colors.text)
                    .Align(Alignment.LEFT)
                    .Position(page_position(14, 1, PageWidth.Full).ShrinkL(PAGE_LABEL_WIDTH))
            )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        cycle.Update elapsed_ms
        if pending_custom_frame_limit && not custom_frame_limit_box.Focused then
            pending_custom_frame_limit <- false
            config.Apply()

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
                (Rect.FromSize(this.Bounds.Right - 300.0f, this.Bounds.Top - 100.0f, 100.0f, 100.0f).Translate(0.0f, (this.Bounds.Height + 100.0f) * y + anti_jitter))
                Color.White
                texture

    override this.Title = %"system.performance"