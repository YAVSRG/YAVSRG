namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI

// todo: make into a static class with constructors
// todo: possible way to create tab view + tabs along top together?
module RadioButtons =

    type RadioButtonOptions<'T> =
        {
            Setting: Setting<'T>
            Options: ('T * string * (unit -> bool)) array
            Height: float32
        }

    type TabButton(label: string, on_click: unit -> unit, is_disabled: unit -> bool, is_chosen: unit -> bool) =
        inherit Button(label, on_click, Disabled = is_disabled)

        override this.Draw() =
            if is_chosen() then
                Render.rect (this.Bounds.BorderT(Style.PADDING).ShrinkR(Style.PADDING)) Colors.grey_2.O2
            else
                Render.rect (this.Bounds.SliceB(this.Bounds.Height + Style.PADDING)) (if this.Focused then Colors.yellow_accent.O1 else Colors.shadow_1.O1)
                Render.rect (this.Bounds.BorderB(Style.PADDING).ShrinkR(Style.PADDING)) Colors.grey_2.O2
            Render.rect (this.Bounds.SliceR(Style.PADDING).Expand(0.0f, Style.PADDING)) Colors.grey_2.O2
            if this.Focused then
                Render.rect (this.Bounds.SliceB(Style.PADDING).Shrink(20.0f, 0.0f)) Colors.yellow_accent.O3
            base.Draw()

    // alternative designed to represent tabs on a tabbed container or view
    let create_tabs (options: RadioButtonOptions<'T>) : GridFlowContainer<TabButton> =
        GridFlowContainer(options.Height, options.Options.Length, Spacing = (0.0f, 0.0f), WrapNavigation = false)
        |+ seq {
            for value, label, disabled in options.Options do
                yield TabButton(
                    label,
                    (fun () -> options.Setting.Set value),
                    disabled,
                    (fun () -> options.Setting.Value = value)
                )
        }

    // todo: alternative designed to look like actual radio buttons