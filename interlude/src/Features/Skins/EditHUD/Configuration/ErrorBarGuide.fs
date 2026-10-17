namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI

type ErrorBarGuidePage() =
    inherit Page()

    let config = Content.HUD

    let show_guide = Setting.simple config.TimingDisplayShowGuide
    let guide_thickness = Setting.percentf config.TimingDisplayGuideThickness

    member this.SaveChanges() =
        Skins.save_hud_config
            { Content.HUD with
                TimingDisplayShowGuide = show_guide.Value
                TimingDisplayGuideThickness = guide_thickness.Value
            }
    
    override this.Content() =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(%"hud.error_bar.showguide", Checkbox show_guide)
                    .Help(Help.Info("hud.error_bar.showguide"))
                    .Pos(0),
                PageSetting(%"hud.error_bar.guide_thickness", Slider.Percent(guide_thickness))
                    .Help(Help.Info("hud.error_bar.guide_thickness"))
                    .Pos(2)
                    .Conditional(show_guide.Get)
            )
    
    override this.Title = %"hud.error_bar.guide_page"