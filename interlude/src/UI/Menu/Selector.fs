namespace Interlude.UI

open System
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input

type Selector<'T>(items: ('T * string) array, setting: Setting<'T>) =
    inherit Container(NodeType.Leaf)

    let mutable index =
        items
        |> Array.tryFindIndex (fun (v, _) -> Object.Equals(v, setting.Value))
        |> Option.defaultValue 0

    let fd () =
        index <- (index + 1) % items.Length
        setting.Value <- fst items.[index]
        Style.click.Play()

    let bk () =
        index <- (index + items.Length - 1) % items.Length
        setting.Value <- fst items.[index]
        Style.click.Play()

    override this.Init(parent: Widget) =
        this
            .Add(
                Text(fun () -> snd items.[index])
                    .Align(Alignment.LEFT),

                MouseListener()
                    .SelectOnClick(this, fd)
                    .FocusOnHover(this)
        )

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if this.Selected then
            if (%%"left").Pressed() then
                bk ()
            elif (%%"right").Pressed() then
                fd ()
            elif (%%"up").Pressed() then
                fd ()
            elif (%%"down").Pressed() then
                bk ()

    static member FromEnum(setting: Setting<'T>) =
        let names = Enum.GetNames(typeof<'T>)
        let values = Enum.GetValues(typeof<'T>) :?> 'T array
        Selector(Array.zip values names, setting)

type SelectDropdown<'T when 'T : equality>(items: ('T * string) array, setting: Setting<'T>) as this =
    inherit Container(NodeType.Button(fun () -> this.ToggleDropdown()))

    let dropdown_wrapper = DropdownWrapper(fun d ->
        let height = min d.Height (Render.height() - this.Bounds.Bottom - Style.PADDING * 2.0f)
        Position.BorderB(height + 2.0f * Style.PADDING).Shrink(Style.PADDING)
    )

    let wrapped_setting =
        let current_value = setting.Value
        match items |> Array.tryFind (fun (v, _) -> v = current_value) with
        | Some v -> v
        | None -> items.[0]
        |> Setting.simple
        |> Setting.trigger (fun (v, _) -> setting.Set v)

    override this.Init(parent) =
        this
        |+ Text((fun () -> snd wrapped_setting.Value))
            .Align(Alignment.LEFT)
        |+ MouseListener().Button(this)
        |* dropdown_wrapper

        base.Init parent

    member this.ToggleDropdown() =
        dropdown_wrapper.Toggle(fun () ->
            Dropdown
                {
                    Items = items |> Array.map (fun (v, label) -> ((v, label), label))
                    ColorFunc = K Colors.text
                    Setting = wrapped_setting
                }
        )

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    static member FromEnum(setting: Setting<'T>) =
        let names = Enum.GetNames(typeof<'T>)
        let values = Enum.GetValues(typeof<'T>) :?> 'T array
        SelectDropdown(Array.zip values names, setting)