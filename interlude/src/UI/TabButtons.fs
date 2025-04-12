namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI

type private TabButton(label: string, on_click: unit -> unit, is_disabled: unit -> bool, is_chosen: unit -> bool) =
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

type TabButtons =

    static member HEIGHT = 50.0f

    static member Create(options: ('T * string * (unit -> bool)) array, setting: Setting<'T>) : Widget =
        NavigationContainer.Row()
            .WrapNavigation(false)
            .With(seq{
                for i, (value, label, disabled) in Seq.indexed options do
                    yield TabButton(
                        label,
                        (fun () -> setting.Set value),
                        disabled,
                        (fun () -> setting.Value = value)
                    )
                        .Position(Position.GridX(i + 1, options.Length))
                    :> Widget
            })

    static member Create(options: ('T * string) array, setting: Setting<'T>) : Widget =
        TabButtons.Create(
            options |> Array.map (fun (value, label) -> (value, label, K false)),
            setting
        )

    static member Create(options: (Widget * string * (unit -> bool)) array, container: SwapContainer) : Widget =
        TabButtons.Create(
            options,
            Setting.make container.set_Current container.get_Current
        )

    static member Create(options: (Widget * string) array, container: SwapContainer) : Widget =
        TabButtons.Create(
            options,
            Setting.make container.set_Current container.get_Current
        )

    static member CreatePersistent(options: (Widget * string * (unit -> bool)) array, container: SwapContainer, setting: Setting<int>) : Widget =
        let x = setting.Value
        let widgets = options |> Array.map (fun (w, _, _) -> w)

        if x >= 0 && x < widgets.Length then
            container.Current <- widgets.[x]

        TabButtons.Create(
            options |> Array.mapi (fun i (_, label, disabled) -> (i, label, disabled)),
            setting |> Setting.trigger (fun i -> container.Current <- widgets.[i])
        )

    static member CreatePersistent(options: (Widget * string) array, container: SwapContainer, setting: Setting<int>) : Widget =
        TabButtons.CreatePersistent(
            options |> Array.map (fun (value, label) -> (value, label, K false)),
            container,
            setting
        )