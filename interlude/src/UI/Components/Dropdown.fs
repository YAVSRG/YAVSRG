namespace Interlude.UI.Components

open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI

module private Dropdown =

    let ITEMSIZE = 60.0f
    
    type Item(label: string, onclick: unit -> unit) as this =
        inherit StaticContainer(NodeType.Button onclick)

        do
            this
            |+ Clickable.Focus this
            |* Text(label,
                Align = Alignment.LEFT,
                Position = Position.Margin(10.0f, 5.0f))

        override this.Draw() =
            if this.Focused then Draw.rect this.Bounds (!*Palette.HOVER)
            base.Draw()

type Dropdown(items: (string * (unit -> unit)) seq, onclose: unit -> unit) as this =
    inherit Frame(NodeType.Switch (fun _ -> this.Items),
        Fill = !%Palette.DARK, Border = !%Palette.LIGHT)

    let flow = FlowContainer.Vertical(Dropdown.ITEMSIZE)

    do
        for (label, action) in items do
            flow |* Dropdown.Item(label, fun () -> action(); this.Close())
        this.Add flow

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        this.VisibleBounds <- Viewport.bounds
        if 
            not this.Focused
            || Mouse.leftClick()
            || Mouse.rightClick()
        then this.Close()

    override this.Init(parent: Widget) =
        base.Init parent
        this.VisibleBounds <- Viewport.bounds
        this.Focus()

    member this.Close() = onclose()
    member private this.Items = flow

    member this.Place (x, y, width) =
        this.Position <- Position.Box(0.0f, 0.0f, x, y, width, this.Height)

    member this.Height = float32 (Seq.length items) * Dropdown.ITEMSIZE

    static member Selector (items: 'T seq) (labelFunc: 'T -> string) (selectFunc: 'T -> unit) (onclose: unit -> unit) =
        Dropdown(Seq.map (fun item -> (labelFunc item, fun () -> selectFunc item)) items, onclose)

open Percyqaz.Common
open Interlude.UI
open Interlude

type RulesetDropdown(setting: Setting<string>) =
    inherit StaticContainer(NodeType.None)

    override this.Init(parent: Widget) =
        this 
        |* StylishButton(
            ( fun () -> this.ToggleDropdown() ),
            ( fun () -> Content.Rulesets.current.Name ),
            Style.main 100,
            TiltRight = false,
            Hotkey = "ruleset")
        base.Init parent

    member this.ToggleDropdown() =
        match this.Dropdown with
        | Some _ -> this.Dropdown <- None
        | _ ->
            let rulesets = Content.Rulesets.list() |> Map.ofSeq
            let favouriteRulesets = 
                Options.options.FavouriteRulesets.Value
                |> List.choose (fun id -> match rulesets.TryFind id with Some rs -> Some id | None -> None)
            let d = Dropdown.Selector favouriteRulesets (fun id -> rulesets.[id].Name) (fun id -> setting.Set id) (fun () -> this.Dropdown <- None)
            d.Position <- Position.SliceBottom(d.Height + 60.0f).TrimBottom(60.0f).Margin(Style.padding, 0.0f)
            d.Init this
            this.Dropdown <- Some d

    member val Dropdown : Dropdown option = None with get, set

    override this.Draw() =
        base.Draw()
        match this.Dropdown with
        | Some d -> d.Draw()
        | None -> ()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        match this.Dropdown with
        | Some d -> d.Update(elapsedTime, moved)
        | None -> ()