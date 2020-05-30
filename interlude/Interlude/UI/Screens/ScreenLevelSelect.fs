namespace Interlude.UI

open System
open Prelude.Common
open Prelude.Data.ScoreManager
open Prelude.Data.ChartManager
open Interlude.Themes
open Interlude.Utils
open Interlude.Render
open Interlude.Input
open Interlude.UI.Animation
open Interlude.UI.Components
open Interlude.Gameplay
open OpenTK

module ScreenLevelSelect =

    type ScoreCard(data: ScoreInfoProvider) as this =
        inherit Widget()

        do
            this.Add(new TextBox(data.Accuracy.ToString() |> K, K Color.White, 0.5f))

        override this.Draw() =
            Draw.rect this.Bounds (Screens.accentShade(127, 0.8f, 0.0f)) Sprite.Default
            base.Draw()

    type Scoreboard() as this =
        inherit FlowContainer()
        //...

    type SelectableItem(content: Choice<CachedChart, string * SelectableItem list>) as this =
        
        let hover = new AnimationFade(0.0f)
        let color = new AnimationColorMixer(Color.White)
        let animation = new AnimationGroup()
        let mutable expand = false

        do
            animation.Add(hover)
            animation.Add(color)

        member this.Draw(top: float32): float32 =
            if top > Render.vheight then
                top + 80.0f
            else
                match content with
                | Choice1Of2 cc ->
                    if (top > 200.0f) then
                        Draw.rect(Rect.create (Render.vwidth * 0.4f) top (Render.vwidth * 0.9f) (top + 75.0f)) (Screens.accentShade(127, 0.5f, 0.0f)) Sprite.Default
                        Text.draw(font(), cc.Title, 25.0f, Render.vwidth * 0.4f, top, Color.White)
                    top + 80.0f
                | Choice2Of2 (name, items) ->
                    if (top > 200.0f) then
                        Draw.rect(Rect.create (Render.vwidth * 0.4f) top (Render.vwidth * 0.9f) (top + 85.0f)) (Screens.accentShade(127, 0.5f, 0.0f)) Sprite.Default
                        Text.draw(font(), name, 40.0f, Render.vwidth * 0.4f, top, Color.White)
                    List.fold (fun t (i: SelectableItem) -> i.Draw(t)) (top + 90.0f) items

        member this.Update(top: float32, elapsedTime): float32 =
            match content with
            | Choice1Of2 cc ->
                if (top > 200.0f) then
                    let bounds = Rect.create (Render.vwidth * 0.4f) top (Render.vwidth * 0.9f) (top + 85.0f)
                    if Mouse.Hover(bounds) then 
                        hover.SetTarget(1.0f)
                        if Mouse.Click(Input.MouseButton.Left) then
                            match cache.LoadChart(cc) with
                            | Some c -> changeChart(cc, c)
                            | None -> Logging.Error("Couldn't load cached file: " + cc.FilePath) ""
                    else
                        hover.SetTarget(0.0f)
                    animation.Update(elapsedTime)
                top + 80.0f
            | Choice2Of2 (name, items) ->
                if (top > 200.0f) then
                    animation.Update(elapsedTime)
                List.fold (fun t (i: SelectableItem) -> i.Update(t, elapsedTime)) (top + 90.0f) items

open ScreenLevelSelect

type ScreenLevelSelect() as this =
    inherit Screen()

    let mutable selection: SelectableItem list = []
    let scrollPos = new AnimationFade(300.0f)

    do
        this.Animation.Add(scrollPos)
        let groups = cache.GetGroups (K "Ungrouped") (System.Comparison(fun a b -> 0))
        selection <- 
            groups.Keys
            |> Seq.map
                (fun k ->
                    groups.[k]
                    |> Seq.map (fun cc -> SelectableItem(Choice1Of2 cc))
                    |> List.ofSeq
                    |> fun l -> SelectableItem(Choice2Of2 (k, l)))
            |> List.ofSeq

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        let height =
            selection
            |> List.fold (fun t (i: SelectableItem) -> i.Update(t, elapsedTime)) scrollPos.Value
        //scroll logic
        
        scrollPos.SetTarget(Math.Clamp(scrollPos.Target + float32 (Mouse.Scroll()) * 100.0f, -height, 300.0f))
        K () ()

    override this.Draw() =
        selection
        |> List.fold (fun t (i: SelectableItem) -> i.Draw(t)) scrollPos.Value
        |> ignore
        base.Draw()