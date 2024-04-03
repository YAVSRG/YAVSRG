namespace Interlude.Features.OptionsMenu.System

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Content
open Interlude.Options
open Interlude.UI.Menu
open Interlude.Utils

type PerformanceSettingsPage() as this =
    inherit Page()

    let cycle = Animation.Counter(1500.0)
    let texture = Content.Texture "note"

    do
        this.Content(
            page_container()
            |+ PageSetting(
                "system.framelimit",
                Selector.FromEnum(config.RenderMode |> Setting.trigger (fun _ -> Window.sync (Window.ApplyConfig config)))
            )
                .Pos(0)
                .Tooltip(Tooltip.Info("system.framelimit"))
            |+ Conditional(
                (fun () -> config.RenderMode.Value = FrameLimit.Unlimited),
                Text(%"system.framelimit.unlimited_warning", 
                    Color = K Colors.text_red,
                    Position = pretty_pos(2, 1, PageWidth.Full).TrimLeft(PRETTYTEXTWIDTH),
                    Align = Alignment.LEFT
                )
            )
            |+ Conditional(
                (fun () -> config.RenderMode.Value = FrameLimit.Smart),
                PageSetting("system.performance.antijitter", 
                    Selector<_>.FromBool(
                        config.SmartCapAntiJitter
                        |> Setting.trigger (fun v -> anti_jitter <- v)
                    )
                )
                    .Pos(2)
                    .Tooltip(Tooltip.Info("system.performance.antijitter"))
            )
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        cycle.Update elapsed_ms

    override this.Draw() =
        base.Draw()
        Draw.rect 
            (this.Bounds.SliceRight(200.0f).Shrink(20.0f))
            (Color.FromHsv(float32 (cycle.Time / cycle.Interval), 0.5f, 1.0f))
        Draw.sprite 
            (Rect.Box(this.Bounds.Right - 300.0f, this.Bounds.Top - 100.0f, 100.0f, 100.0f).Translate(0.0f, (this.Bounds.Height + 100.0f) * float32 (cycle.Time / cycle.Interval)))
            Color.White
            texture

    override this.OnClose() = ()

    override this.Title = %"system.performance.name"
