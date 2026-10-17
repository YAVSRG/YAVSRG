namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI

type ColumnErrorBarsGuidePage() =
    inherit Page()

    let config = Content.HUD

    let show_guide = Setting.simple config.ColumnErrorBarsShowGuide
    let guide_thickness = Setting.percentf config.ColumnErrorBarsGuideThickness

    member this.SaveChanges() =
        Skins.save_hud_config
            { Content.HUD with
                ColumnErrorBarsShowGuide = show_guide.Value
                ColumnErrorBarsGuideThickness = guide_thickness.Value
            }
    
    override this.Content() =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(%"hud.column_error_bars.show_guide", Checkbox(show_guide))
                    .Help(Help.Info("hud.column_error_bars.show_guide"))
                    .Pos(0),
                PageSetting(%"hud.column_error_bars.guide_thickness", Slider.Percent(guide_thickness))
                    .Help(Help.Info("hud.column_error_bars.guide_thickness"))
                    .Pos(2)
                    .Conditional(show_guide.Get)
            )
    
    override this.Title = %"hud.column_error_bars.guide_settings"