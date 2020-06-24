namespace Interlude.UI

open System
open Prelude.Common
open Prelude.Data.ScoreManager
open Prelude.Data.ChartManager
open Interlude.Themes
open Interlude.Utils
open Interlude.Render
open Interlude.Options
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

    type Scoreboard() =
        inherit FlowContainer()
        //...

    type SelectableItem(content: Choice<CachedChart, string * SelectableItem list>) =
        
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
                        Draw.rect(Rect.create (Render.vwidth * 0.4f) top (Render.vwidth - 10.0f) (top + 75.0f)) (Screens.accentShade(127, 0.5f, 0.0f)) Sprite.Default
                        Text.draw(font(), cc.Title + " // " + cc.Artist, 25.0f, Render.vwidth * 0.4f, top, Color.White)
                        Text.draw(font(), cc.DiffName + " // " + cc.Creator, 20.0f, Render.vwidth * 0.4f, top + 40.0f, Color.White)
                    top + 80.0f
                | Choice2Of2 (name, items) ->
                    if (top > 200.0f) then
                        Draw.rect(Rect.create (Render.vwidth * 0.4f) top (Render.vwidth * 0.9f) (top + 85.0f)) (Screens.accentShade(127, 0.5f, 0.0f)) Sprite.Default
                        Text.draw(font(), name, 40.0f, Render.vwidth * 0.4f, top, Color.White)
                    if expand then
                        List.fold (fun t (i: SelectableItem) -> i.Draw(t)) (top + 90.0f) items
                    else
                        top + 90.0f

        member this.Update(top: float32, elapsedTime): float32 =
            match content with
            | Choice1Of2 cc ->
                if (top > 200.0f) then
                    let bounds = Rect.create (Render.vwidth * 0.4f) top (Render.vwidth - 10.0f) (top + 85.0f)
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
                    let bounds = Rect.create (Render.vwidth * 0.4f) top (Render.vwidth * 0.9f) (top + 85.0f)
                    if Mouse.Hover(bounds) then 
                        hover.SetTarget(1.0f)
                        if Mouse.Click(Input.MouseButton.Left) then
                            expand <- not expand
                    else
                        hover.SetTarget(0.0f)
                    animation.Update(elapsedTime)
                if expand then
                    List.fold (fun t (i: SelectableItem) -> i.Update(t, elapsedTime)) (top + 90.0f) items
                else
                    top + 90.0f

    let private firstCharacter(s : string) =
        if (s.Length = 0) then "?"
        else
            if Char.IsLetterOrDigit(s.[0]) then s.[0].ToString().ToUpper() else "?"

    //todo: maybe move to ChartManagement in Prelude
    let groupBy = dict[
            "Physical", fun c -> let i = int (c.Physical / 2.0) * 2 in i.ToString().PadLeft(2, '0') + " - " + (i + 2).ToString().PadLeft(2, '0')
            "Technical", fun c -> let i = int (c.Technical / 2.0) * 2 in i.ToString().PadLeft(2, '0') + " - " + (i + 2).ToString().PadLeft(2, '0')
            "Pack", fun c -> c.Pack
            "Title", fun c -> firstCharacter(c.Title)
            "Artist", fun c -> firstCharacter(c.Artist)
            "Creator", fun c -> firstCharacter(c.Creator)
            "Keymode", fun c -> c.Keys.ToString() + "k"
        ]

    let sortBy = dict[
            "Physical", Comparison(fun a b -> a.Physical.CompareTo(b.Physical))
            "Technical", Comparison(fun a b -> a.Technical.CompareTo(b.Technical))
            "Title", Comparison(fun a b -> a.Title.CompareTo(b.Title))
            "Artist", Comparison(fun a b -> a.Artist.CompareTo(b.Artist))
            "Creator", Comparison(fun a b -> a.Creator.CompareTo(b.Creator))
        ]

open ScreenLevelSelect

type ScreenLevelSelect() as this =
    inherit Screen()

    let mutable selection: SelectableItem list = []
    let scrollPos = new AnimationFade(300.0f)
    let searchText = new Setting<string>("")
    let searchTimer = System.Diagnostics.Stopwatch()

    let refresh() =
        let groups = cache.GetGroups groupBy.[Options.profile.ChartGroupMode.Get()] sortBy.[Options.profile.ChartSortMode.Get()] <| searchText.Get()
        selection <- 
            groups.Keys
            |> Seq.sort
            |> Seq.map
                (fun k ->
                    groups.[k]
                    |> Seq.map (fun cc -> SelectableItem(Choice1Of2 cc))
                    |> List.ofSeq
                    |> fun l -> SelectableItem(Choice2Of2 (k + " (" + l.Length.ToString() + ")", l)))
            |> List.ofSeq
        //scroll to correct selected chart

    do
        if not <| sortBy.ContainsKey(Options.profile.ChartSortMode.Get()) then Options.profile.ChartSortMode.Set("Title")
        if not <| groupBy.ContainsKey(Options.profile.ChartGroupMode.Get()) then Options.profile.ChartGroupMode.Set("Pack")
        this.Animation.Add(scrollPos)

        this.Add(
            let sorts = sortBy.Keys |> Array.ofSeq
            new Dropdown(sorts, Array.IndexOf(sorts, Options.profile.ChartSortMode.Get()),
                (fun i -> Options.profile.ChartSortMode.Set(sorts.[i]); refresh()), "Sort by", 50.0f)
            |> positionWidget(-400.0f, 1.0f, 100.0f, 0.0f, -250.0f, 1.0f, 400.0f, 0.0f))

        this.Add(
            let groups = groupBy.Keys |> Array.ofSeq
            new Dropdown(groups, Array.IndexOf(groups, Options.profile.ChartGroupMode.Get()),
                (fun i -> Options.profile.ChartGroupMode.Set(groups.[i]); refresh()), "Group by", 50.0f)
            |> positionWidget(-200.0f, 1.0f, 100.0f, 0.0f, -50.0f, 1.0f, 400.0f, 0.0f))

        this.Add(
            new TextEntry(new WrappedSetting<string, string>(searchText, (fun s -> searchTimer.Restart(); s), id), Some (Options.options.Hotkeys.Search :> ISettable<Bind>), "search")
            |> positionWidget(-600.0f, 1.0f, 20.0f, 0.0f, -50.0f, 1.0f, 80.0f, 0.0f))
        refresh()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        let bottom =
            selection
            |> List.fold (fun t (i: SelectableItem) -> i.Update(t, elapsedTime)) scrollPos.Value
        //right click scroll logic
        let height = bottom - scrollPos.Value - 300.0f
        scrollPos.SetTarget(Math.Clamp(scrollPos.Target + float32 (Mouse.Scroll()) * 100.0f, -height, 300.0f))
        if searchTimer.ElapsedMilliseconds > 400L then searchTimer.Reset(); refresh()
        if Options.options.Hotkeys.Select.Get().Tapped(false) then
            Screens.addScreen(new ScreenPlay())

    override this.Draw() =
        selection
        |> List.fold (fun t (i: SelectableItem) -> i.Draw(t)) scrollPos.Value
        |> ignore
        base.Draw()