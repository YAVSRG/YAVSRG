namespace Interlude.Features.OptionsMenu

open Interlude.Utils
open Interlude.Options
open Interlude.UI.Menu

module Advanced =

    type AdvancedPage() as this =
        inherit Page()

        do

            this.Content(
                column()
                |+ PageSetting("advanced.enableconsole", Selector<_>.FromBool options.EnableConsole)
                    .Pos(200.0f)
                |+ PageSetting("advanced.vanishingnotes", Selector<_>.FromBool options.VanishingNotes)
                    .Pos(270.0f)
                |+ PageSetting("advanced.autocalibrateoffset", Selector<_>.FromBool options.AutoCalibrateOffset)
                    .Pos(340.0f)
            )
        override this.Title = L"advanced.name"
        override this.OnClose() = ()