namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude

type PageTextEntry(name: string, setting: Setting<string>) =
    inherit
        PageSetting(
            name,
            let entry =
                { new TextEntry(setting, "none", false) with
                    override this.OnFocus by_mouse =
                        base.OnFocus(by_mouse)
                        Style.hover.Play()
                }

            entry.With(
                Frame(
                    Position = Position.DEFAULT.ExpandX(Style.PADDING * 3.0f),
                    Fill = K Color.Transparent,
                    Border =
                        fun () ->
                            if entry.Selected then Colors.pink_accent
                            elif entry.Focused then Colors.yellow_accent
                            else Colors.grey_2
                )
            )
        )
