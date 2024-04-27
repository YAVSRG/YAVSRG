namespace Interlude.Features.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skinning.Noteskins
open Interlude.Content
open Interlude.UI.Menu
open Interlude.Options

type TimingDisplayPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.TimingDisplayPosition

    let show_guide = Setting.simple user_options.TimingDisplayShowGuide
    let show_non_judgements = Setting.simple user_options.TimingDisplayShowNonJudgements

    let thickness =
        Setting.simple user_options.TimingDisplayThickness |> Setting.bound 1.0f 25.0f

    let release_thickness =
        Setting.simple user_options.TimingDisplayReleasesExtraHeight
        |> Setting.bound 0.0f 20.0f

    let half_scale_releases = Setting.simple user_options.TimingDisplayHalfScaleReleases

    let animation_time =
        Setting.simple user_options.TimingDisplayFadeTime
        |> Setting.bound 100.0f 2000.0f

    let preview =
        { new ConfigPreview(0.35f, pos) with
            override this.DrawComponent(bounds) =
                Draw.rect
                    (Rect.Create(
                        bounds.CenterX - thickness.Value / 2.0f,
                        bounds.Top,
                        bounds.CenterX + thickness.Value / 2.0f,
                        bounds.Bottom
                    ))
                    Color.White
        }

    do
        this.Content(
            page_container()
            |+ PageSetting("hud.timingdisplay.showguide", Selector<_>.FromBool show_guide)
                .Pos(0)
                .Tooltip(Tooltip.Info("hud.timingdisplay.showguide"))
            |+ PageSetting("hud.timingdisplay.shownonjudgements", Selector<_>.FromBool show_non_judgements)
                .Pos(2)
                .Tooltip(Tooltip.Info("hud.timingdisplay.shownonjudgements"))
            |+ PageSetting("hud.timingdisplay.halfscalereleases", Selector<_>.FromBool half_scale_releases)
                .Pos(4)
                .Tooltip(Tooltip.Info("hud.timingdisplay.halfscalereleases"))
            |+ PageSetting("hud.timingdisplay.thickness", Slider(thickness, Step = 1f))
                .Pos(6)
                .Tooltip(Tooltip.Info("hud.timingdisplay.thickness"))
            |+ PageSetting("hud.timingdisplay.releasesextraheight", Slider(release_thickness, Step = 1f))
                .Pos(8)
                .Tooltip(Tooltip.Info("hud.timingdisplay.releasesextraheight"))
            |+ PageSetting("hud.timingdisplay.animationtime", Slider(animation_time, Step = 5f))
                .Pos(10)
                .Tooltip(Tooltip.Info("hud.timingdisplay.animationtime"))
            |>> Container
            |+ preview
        )

    override this.Title = %"hud.timingdisplay.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                TimingDisplayShowGuide = show_guide.Value
                TimingDisplayShowNonJudgements = show_non_judgements.Value
                TimingDisplayThickness = thickness.Value
                TimingDisplayReleasesExtraHeight = release_thickness.Value
                TimingDisplayFadeTime = animation_time.Value
            }

        on_close ()