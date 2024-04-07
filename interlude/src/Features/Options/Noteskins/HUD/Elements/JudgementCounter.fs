namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Content.Noteskins
open Interlude.Content
open Interlude.UI.Menu
open Interlude.Options

type JudgementCounterPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.JudgementCounterPosition

    let animation_time =
        Setting.simple user_options.JudgementCounterFadeTime
        |> Setting.bound 100.0 1000.0

    let use_background = Setting.simple noteskin_options.JudgementCounterUseBackground
    let background_scale = Setting.simple noteskin_options.JudgementCounterBackgroundScale |> Setting.bound 0.5f 2.0f

    let preview =
        { new ConfigPreview(0.35f, pos) with
            override this.DrawComponent(bounds) =
                Draw.rect bounds (Color.FromArgb(127, 255, 255, 255))
        }

    do
        this.Content(
            page_container()
            |+ PageSetting("hud.judgementcounter.animationtime", Slider(animation_time |> Setting.f32, Step = 5f))
                .Pos(0)
                .Tooltip(Tooltip.Info("hud.judgementcounter.animationtime"))
            |+ ([
                PageSetting("hud.judgementcounter.usebackground", Selector<_>.FromBool(use_background))
                    .Pos(2)
                    .Tooltip(Tooltip.Info("hud.judgementcounter.usebackground")) :> Widget
                Conditional(use_background.Get, 
                    PageSetting("hud.judgementcounter.backgroundscale", Slider.Percent(background_scale))
                        .Pos(4)
                        .Tooltip(Tooltip.Info("hud.judgementcounter.backgroundscale"))
                )
                ] |> or_require_noteskin)
            |>> Container
            |+ preview
        )

    override this.Title = %"hud.judgementcounter.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                JudgementCounterFadeTime = animation_time.Value
            }

        Noteskins.save_hud_config 
            { Content.NoteskinConfig.HUD with
                JudgementCounterUseBackground = use_background.Value
                JudgementCounterBackgroundScale = background_scale.Value
            }

        on_close ()