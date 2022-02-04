namespace Interlude.UI.Screens.Play

open Prelude.Common
open Interlude
open Interlude.Options
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Controls
open Interlude.UI.Components.Selection.Menu

module QuickOptions =

    let page() : SelectionPage =
        {
            Content = fun add ->
                let firstNote = Gameplay.currentChart.Value.FirstNote
                column [
                    PrettySetting("quick.localoffset",
                        Slider(
                            Setting.make
                                (fun v -> Gameplay.chartSaveData.Value.Offset <- toTime v + firstNote; Audio.changeLocalOffset(toTime v))
                                (fun () -> float (Gameplay.chartSaveData.Value.Offset - firstNote))
                            |> Setting.bound -200.0 200.0
                            |> Setting.round 0, 0.01f)
                    ).Position(200.0f)
                    PrettySetting("gameplay.scrollspeed", Slider<_>.Percent(options.ScrollSpeed, 0.0025f)).Position(280.0f)
                    PrettySetting("gameplay.hitposition", Slider(options.HitPosition, 0.005f)).Position(360.0f)
                    PrettySetting("gameplay.upscroll", Selector.FromBool options.Upscroll).Position(440.0f)
                    //PrettySetting("gameplay.backgrounddim", Slider(options.BackgroundDim :?> FloatSetting, 0.01f)).Position(440.0f)
                ] :> Selectable
            Callback = Audio.playLeadIn
        }

    let show() = { new SelectionMenu(page()) with override this.OnClose() = base.OnClose(); Audio.playLeadIn() }.Show()