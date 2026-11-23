namespace Interlude.Features.OptionsMenu.System

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type CustomWindowedOffsetPage(setting: Setting<float32 * float32>) =
    inherit Page()

    let horizontal = Setting.percentf(fst setting.Value)
    let vertical = Setting.percentf(snd setting.Value)

    member this.SaveChanges() : unit =
        setting.Set(horizontal.Value, vertical.Value)

    override this.Content() : Widget =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(%"system.windowresolution.offset.horizontal", Slider.Percent(horizontal)).Pos(0),
                PageSetting(%"system.windowresolution.offset.vertical", Slider.Percent(vertical)).Pos(2),
                PageButton(%"system.windowresolution.offset.apply", fun () -> this.HandleClose()).Pos(5),
                PageButton(%"confirm.yes", Menu.Back).Pos(7)
            )

    override this.Title = %"system.windowresolution.offset"
