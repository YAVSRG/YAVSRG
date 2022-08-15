namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
        
module Screen =
    
    type Type =
        | SplashScreen = 0
        | MainMenu = 1
        | Import = 2
        | LevelSelect = 3
        | Play = 4
        | Replay = 5
        | Score = 6

    [<AbstractClass>]
    type T() =
        inherit StaticContainer(NodeType.None)
        abstract member OnEnter: Type -> unit
        abstract member OnExit: Type -> unit
    
    module Toolbar =
        let HEIGHT = 70.0f
        let expandAmount = Animation.Fade 1.0f
        let mutable hidden = false

        let hide() = hidden <- true
        let show() = hidden <- false

    let globalAnimation = Animation.fork [Style.accentColor; Toolbar.expandAmount]

    let logo = Logo.display
    
    let mutable private current = Unchecked.defaultof<T>
    let private screens : T array = Array.zeroCreate 4
    let init (_screens: T array) =
        for i = 0 to 3 do screens.[i] <- _screens.[i]
        current <- screens.[0]
    let mutable exit = false
    let mutable currentType = Type.SplashScreen

    let changeNew (thunk: unit -> #T) (screenType: Type) (flags: Transitions.Flags) =
        if (screenType <> currentType || screenType = Type.Play) then
            Transitions.tryStart((fun () -> 
                let s = thunk()
                current.OnExit screenType
                s.OnEnter currentType
                currentType <- screenType
                current <- s
            ), flags)
            |> globalAnimation.Add

    let change (screenType: Type) (flags: Transitions.Flags) = changeNew (K screens.[int screenType]) screenType flags

    let back (flags: Transitions.Flags) =
        match currentType with
        | Type.SplashScreen -> exit <- true
        | Type.MainMenu -> change Type.SplashScreen flags
        | Type.LevelSelect -> change Type.MainMenu flags
        | Type.Import
        | Type.Play
        | Type.Replay
        | Type.Score -> change Type.LevelSelect flags
        | _ -> Logging.Critical (sprintf "No back-behaviour defined for %A" currentType)

    type Container(toolbar: Widget) =
        inherit Overlay(NodeType.None)
    
        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            Background.update elapsedTime
            if currentType <> Type.Play || Dialog.exists() then Tooltip.display.Update (elapsedTime, moved)
            if Viewport.vwidth > 0.0f then
                let x, y = Mouse.pos()
                Background.setParallaxPos(x / Viewport.vwidth, y / Viewport.vheight)
            Style.accentColor.SetColor Content.accentColor
            Dialog.display.Update(elapsedTime, moved)

            globalAnimation.Update elapsedTime
            toolbar.Update (elapsedTime, moved)
            logo.Update (elapsedTime, moved)
            let screenBounds = if Toolbar.hidden then this.Bounds else this.Bounds.Shrink(0.0f, Toolbar.HEIGHT * Toolbar.expandAmount.Value)
            current.Update (elapsedTime, moved)
    
        override this.Draw() =
            Background.drawWithDim (this.Bounds, Color.White, 1.0f)
            current.Draw()
            logo.Draw()
            toolbar.Draw()
            if Transitions.active then
                Transitions.draw this.Bounds
                if (Transitions.flags &&& Transitions.Flags.UnderLogo = Transitions.Flags.UnderLogo) then logo.Draw()
            Dialog.display.Draw()
            if currentType <> Type.Play || Dialog.exists() then 
                let x, y = Mouse.pos()
                Draw.sprite (Rect.Box(x, y, Content.themeConfig().CursorSize, Content.themeConfig().CursorSize)) (Style.color(255, 1.0f, 0.5f)) (Content.getTexture "cursor")
                Tooltip.display.Draw()

        override this.Init(parent: Widget) =
            base.Init parent
            Logo.display.Init this
            toolbar.Init this
            Tooltip.display.Init this
            Dialog.display.Init this
            current.OnEnter Type.SplashScreen

type Screen = Screen.T