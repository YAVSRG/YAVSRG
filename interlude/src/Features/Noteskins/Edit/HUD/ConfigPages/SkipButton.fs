namespace Interlude.Features.Noteskins.Edit

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skinning.Noteskins
open Interlude.Content
open Interlude.UI.Menu
open Interlude.Options

type SkipButtonPage(on_close: unit -> unit) =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let use_background = Setting.simple noteskin_options.SkipButtonBackground.Enable
    let background_scale = Setting.simple noteskin_options.SkipButtonBackground.Scale |> Setting.bound 0.5f 2.0f
    let background_offset_x = Setting.percentf noteskin_options.SkipButtonBackground.AlignmentX
    let background_offset_y = Setting.percentf noteskin_options.SkipButtonBackground.AlignmentY

    let pos = Setting.simple noteskin_options.SkipButtonPosition

    let preview_text = [ (%%"skip").ToString() ] %> "play.skiphint"

    let preview =
        { new ConfigPreviewNew(pos.Value) with
            override this.DrawComponent(bounds) =
                Text.fill_b (Style.font, preview_text, bounds, Colors.text, Alignment.CENTER)
        }

    override this.Content() =
        page_container()
        |+ ([
            PageSetting(%"hud.skipbutton.usebackground", Checkbox use_background)
                .Tooltip(Tooltip.Info("hud.skipbutton.usebackground"))
                .Pos(0)
            PageSetting(%"hud.skipbutton.backgroundscale", Slider.Percent(background_scale))
                .Tooltip(Tooltip.Info("hud.skipbutton.backgroundscale"))
                .Pos(2)
                .Conditional(use_background.Get)
            PageSetting(%"hud.skipbutton.background_offset_x", Slider.Percent(background_offset_x))
                .Tooltip(Tooltip.Info("hud.skipbutton.background_offset_x"))
                .Pos(4)
                .Conditional(use_background.Get)
            PageSetting(%"hud.skipbutton.background_offset_y", Slider.Percent(background_offset_y))
                .Tooltip(Tooltip.Info("hud.skipbutton.background_offset_y"))
                .Pos(6)
                .Conditional(use_background.Get)
        ] |> or_require_noteskin)
        |>> Container
        |+ preview
        :> Widget

    override this.Title = %"hud.skipbutton"

    override this.OnClose() =
        Noteskins.save_hud_config 
            { Content.NoteskinConfig.HUD with
                SkipButtonBackground = 
                    {
                        Enable = use_background.Value
                        Scale = background_scale.Value
                        AlignmentX = background_offset_x.Value
                        AlignmentY = background_offset_y.Value
                    }
            }
        on_close ()