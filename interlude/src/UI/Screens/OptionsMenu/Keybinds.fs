namespace Interlude.UI.OptionsMenu

open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude.Options
open Interlude.UI
open Interlude.UI.Components.Selection.Menu

module Keybinds = 

    type GameplayKeybinder(keymode: Setting<Keymode>) as this =
        inherit StaticContainer(NodeType.Leaf)

        let mutable progress = 0

        let mutable text = options.GameplayBinds.[int keymode.Value - 3] |> Seq.map (sprintf "%O") |> String.concat ",  "
        let refreshText() : unit =
            let binds = options.GameplayBinds.[int keymode.Value - 3]
            if not this.Selected then
                text <- binds |> Seq.map (sprintf "%O") |> String.concat ",  "
            else
                text <- ""
                for i = 0 to progress - 1 do
                    text <- text + binds.[i].ToString() + ",  "
                text <- text + "..."

        let rec inputCallback(b) =
            let binds = options.GameplayBinds.[int keymode.Value - 3]
            match b with
            | Key (k, _) ->
                binds.[progress] <- Key (k, (false, false, false))
                progress <- progress + 1
                if progress = int keymode.Value then this.Focus()
                else Input.grabNextEvent inputCallback
                refreshText()
            | _ -> Input.grabNextEvent inputCallback

        do
            this
            |+ Text((fun () -> text),
                Color = (fun () -> (if this.Selected then Style.accentShade(255, 1.0f, 0.5f) else Color.White), Color.Black),
                Align = Alignment.LEFT)
            |* Clickable((fun () -> if not this.Selected then this.Select()),
                OnHover = fun b -> if b then this.Focus())

        override this.OnSelected() =
            base.OnSelected()
            progress <- 0
            refreshText()
            Input.grabNextEvent inputCallback

        override this.OnDeselected() =
            base.OnDeselected()
            Input.removeInputMethod()
            text <- options.GameplayBinds.[int keymode.Value - 3] |> Seq.map (sprintf "%O") |> String.concat ",  "

        member this.OnKeymodeChanged() = refreshText()

    type Keybinder(hotkey: Hotkey) as this =
        inherit StaticContainer(NodeType.Leaf)

        do
            this
            |+ Text((fun () -> (!|hotkey).ToString()),
                Color = (fun () -> (if this.Selected then Style.accentShade(255, 1.0f, 0.0f) else Color.White), Color.Black),
                Align = Alignment.LEFT,
                Position = Position.TrimLeft 20.0f)
            |* Clickable((fun () -> if not this.Selected then this.Select()), 
                OnHover = fun b -> if b then this.Focus())

        // todo: ensure this updates the live binds properly
        let set = fun v -> options.Hotkeys.[hotkey] <- v
    
        override this.Draw() =
            if this.Selected then Draw.rect this.Bounds (Style.accentShade(180, 1.0f, 0.5f))
            elif this.Focused then Draw.rect this.Bounds (Style.accentShade(120, 1.0f, 0.8f))
            Draw.rect (this.Bounds.Shrink(0.0f, 40.0f)) (Style.accentShade(127, 0.8f, 0.0f))
            base.Draw()
    
        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if this.Selected then
                match Input.consumeAny InputEvType.Press with
                | ValueNone -> ()
                | ValueSome b ->
                    match b with
                    | Key (k, (ctrl, _, shift)) ->
                        if k = Keys.Escape then set Dummy
                        else set (Key (k, (ctrl, false, shift)))
                        this.Focus()
                    | _ -> ()

    type HotkeysPage() as this =
        inherit Page()

        let container =
            FlowContainer.Vertical<PrettySetting>(PRETTYHEIGHT)
        let scrollContainer = ScrollContainer.Flow(container)

        do
            for hk in Hotkeys.hotkeys.Keys do
                if hk <> "none" then
                    container.Add( PrettySetting("hotkeys." + hk.ToLower(), Keybinder hk) )
            this.Content scrollContainer

        override this.Title = N"keybinds.hotkeys"
        override this.OnClose() = ()

    type KeybindsPage() as this =
        inherit Page()
        
        let keycount = Setting.simple options.KeymodePreference.Value
        let binds = GameplayKeybinder(keycount)

        do
            this.Content(
                column()
                |+ PrettySetting("generic.keymode", Selector<Keymode>.FromEnum(keycount |> Setting.trigger (ignore >> binds.OnKeymodeChanged))).Pos(200.0f)
                |+ PrettySetting("keybinds.gameplay", binds).Pos(280.0f, Viewport.vwidth - 200.0f)
                |+ PrettyButton("keybinds.hotkeys", (fun () -> Menu.ShowPage HotkeysPage)).Pos(400.0f)
            )
        override this.Title = N"keybinds"
        override this.OnClose() = ()