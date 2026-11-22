namespace Interlude.Features.OptionsMenu.SystemSettings

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type CustomWindowedResolutionPage(setting: Setting<int * int>) =
    inherit Page()

    let tab = Bind.mk Keys.Tab

    let max_width, max_height =
        let mode = Monitors.get_current_supported_video_modes() |> Array.last
        mode.Width, mode.Height

    let width = Setting.bounded (min 320 (max_width - 1), max_width) (fst setting.Value)

    let height =
        Setting.bounded (min 240 (max_height - 1), max_height) (snd setting.Value)

    let width_box = NumberEntry.Create(width)
    let height_box = NumberEntry.Create(height)

    member this.SaveChanges() : unit =
        if height.Value * 4 / 3 <= width.Value then
            setting.Set(width.Value, height.Value)

    override this.Content() : Widget =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(%"system.windowresolution.width", width_box).Pos(0),
                PageSetting(%"system.windowresolution.height", height_box).Pos(2),
                Text(%"system.windowresolution.aspect_ratio_warning")
                    .Color(Colors.text_red)
                    .Align(Alignment.CENTER)
                    .Conditional(fun () -> height.Value * 4 / 3 > width.Value)
                    .Pos(4, 1, PageWidth.Normal),
                PageButton(%"confirm.yes", Menu.Back).Pos(6)
            )

    override this.Update(elapsed_ms: float, moved: bool) : unit =
        let selected =
            Selection.get_selected_element() |> Option.map(fun w -> (w :?> Widget).Parent)

        if selected = Some width_box && (tab.Pressed() || (%%"select").Pressed()) then
            height_box.Select(false)
        elif selected = Some height_box && (tab.Pressed() || (%%"select").Pressed()) then
            Menu.Back()

        base.Update(elapsed_ms, moved)

    override this.Title = %"system.windowresolution"
