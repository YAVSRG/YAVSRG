namespace Interlude.Features.Export

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Mods
open Prelude.Data.Exports
open Interlude.UI

type OsuExportOptionsPage(title: string, selected_mods: ModState, on_submit: bool -> OsuExportOptions -> unit) =
    inherit Page()

    let od = Setting.bounded (0.0f, 10.0f) 8.0f
    let hp = Setting.bounded (0.0f, 10.0f) 8.0f
    let apply_mods = Setting.simple(not selected_mods.IsEmpty)

    override this.Content() : Widget =
        page_container()
            .With(
                PageSetting(%"osz_export.od", Slider(od, Step = 0.1f)).Pos(0),
                PageSetting(%"osz_export.hp", Slider(hp, Step = 0.1f)).Pos(2),
                PageSetting(%"osz_export.apply_mods", Checkbox apply_mods)
                    .Pos(5)
                    .Conditional(K(not selected_mods.IsEmpty)),
                PageButton
                    .Once(
                        %"osz_export.confirm",
                        fun () ->
                            on_submit apply_mods.Value { OD = float od.Value; HP = float hp.Value }
                            Menu.Exit()
                    )
                    .Pos(8)
            )

    override this.Title = title
