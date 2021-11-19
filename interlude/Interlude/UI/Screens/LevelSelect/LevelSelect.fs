namespace Interlude.UI.Screens.LevelSelect

open System
open System.Drawing
open System.Linq
open OpenTK.Mathematics
open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Prelude.Data.ScoreManager
open Prelude.Data.Charts
open Prelude.Data.Charts.Sorting
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Collections
open Prelude.Scoring
open Interlude
open Interlude.UI
open Interlude.Utils
open Interlude.Graphics
open Interlude.Input
open Interlude.Gameplay
open Interlude.Content
open Interlude.Options
open Interlude.UI.Animation
open Interlude.UI.Components
open Interlude.UI.Screens.LevelSelect.Globals
open Interlude.UI.Screens.Score
open Interlude.UI.Components.Selection.Menu

[<AbstractClass>]
type private LevelSelectItem() =
    abstract member Bounds: float32 -> Rect
    abstract member Selected: bool
    abstract member Navigate: unit -> unit
    abstract member OnDraw: Rect * bool -> unit
    abstract member OnUpdate: Rect * bool * float -> unit

    abstract member Draw: float32 * float32 -> float32
    default this.Draw(top: float32, topEdge: float32) =
        let bounds = this.Bounds top
        let struct (_, _, _, bottom) = bounds
        if bottom > topEdge + 170.0f && top < Render.vheight - topEdge then this.OnDraw(bounds, this.Selected)
        top + Rect.height bounds + 15.0f

    abstract member Update: float32 * float32 * float -> float32
    default this.Update(top: float32, topEdge: float32, elapsedTime) =
        this.Navigate()
        let bounds = this.Bounds top
        let struct (_, _, _, bottom) = bounds
        if bottom > topEdge + 170.0f && top < Render.vheight - topEdge then this.OnUpdate(bounds, this.Selected, elapsedTime)
        top + Rect.height bounds + 15.0f

type private LevelSelectChartItem(groupName, cc) =
    inherit LevelSelectItem()

    let hover = new AnimationFade(0.0f)
    let mutable colorVersion = -1
    let mutable color = Color.Transparent
    let mutable chartData = None
    let mutable pbData = (None, None, None)
    let mutable collectionIcon = ""

    override this.Bounds(top) = Rect.create (Render.vwidth * 0.4f) top Render.vwidth (top + 90.0f)
    override this.Selected = selectedChart = cc.FilePath
    member this.Chart = cc

    override this.Navigate() =
        match navigation with
        | Navigation.Nothing -> ()
        | Navigation.Forward b ->
            if b then
                switchCurrentChart(cc, groupName)
                navigation <- Navigation.Nothing
            elif groupName = selectedGroup && this.Selected then navigation <- Navigation.Forward true
        | Navigation.Backward (groupName2, cc2) ->
            if groupName = selectedGroup && this.Selected then
                switchCurrentChart(cc2, groupName2)
                navigation <- Navigation.Nothing
            else navigation <- Navigation.Backward(groupName, cc)

    override this.OnDraw(bounds, selected) =
        let struct (left, top, right, bottom) = bounds
        let accent = Style.accentShade(80 + int (hover.Value * 40.0f), 1.0f, 0.2f)
        Draw.rect bounds (if this.Selected then Style.main 80 () else Style.black 80 ()) Sprite.Default
        let stripeLength = (right - left) * (0.4f + 0.6f * hover.Value)
        Draw.quad
            (Quad.create <| new Vector2(left, top) <| new Vector2(left + stripeLength, top) <| new Vector2(left + stripeLength * 0.9f, bottom - 25.0f) <| new Vector2(left, bottom - 25.0f))
            (struct(accent, Color.Transparent, Color.Transparent, accent))
            Sprite.DefaultQuad

        let (accAndGrades, lamp, clear) = pbData
        let f (p: PersonalBests<'T> option) (format: 'T -> string) (color: 'T -> Color) (pos: float32) =
            let (t, t2, c) =
                match p with
                | None -> ("", "", Color.Transparent)
                | Some ((p1, r1), (p2, r2)) ->
                    if r1 < rate then (format p2, sprintf "(%.2fx)" r2, if r2 < rate then Color.FromArgb(127, Color.White) else color p2)
                    else (format p1, sprintf "(%.2fx)" r1, color p1)
            if c.A > 0uy then
                Draw.rect(Rect.create (right - pos - 40.0f) top (right - pos + 40.0f) bottom) accent Sprite.Default
                Text.drawJustB(font(), t, 20.0f, right - pos, top + 8.0f, (c, Color.Black), 0.5f)
                Text.drawJustB(font(), t2, 14.0f, right - pos, top + 35.0f, (c, Color.Black), 0.5f)

        f accAndGrades (fun (x, _) -> sprintf "%.2f%%" (100.0 * x)) (snd >> Helpers.gradeToColor) 450.0f
        f lamp (fun x -> x.ToString()) Helpers.lampToColor 300.0f
        f clear (fun x -> if x then "CLEAR" else "FAILED") Helpers.clearToColor 150.0f

        Draw.rect(Rect.sliceBottom 25.0f bounds) (Color.FromArgb(60, 0, 0, 0)) Sprite.Default
        Text.drawB(font(), cc.Title, 23.0f, left, top, (Color.White, Color.Black))
        Text.drawB(font(), cc.Artist + "  •  " + cc.Creator, 18.0f, left, top + 34.0f, (Color.White, Color.Black))
        Text.drawB(font(), cc.DiffName, 15.0f, left, top + 65.0f, (Color.White, Color.Black))
        Text.drawB(font(), collectionIcon, 35.0f, right - 95.0f, top + 10.0f, (Color.White, Color.Black))

        let border = Rect.expand(5.0f, 5.0f) bounds
        let border2 = Rect.expand(5.0f, 0.0f) bounds
        let borderColor = if selected then Style.accentShade(180, 1.0f, 0.5f) else color
        if borderColor.A > 0uy then
            Draw.rect(Rect.sliceLeft 5.0f border2) borderColor Sprite.Default
            Draw.rect(Rect.sliceTop 5.0f border) borderColor Sprite.Default
            Draw.rect(Rect.sliceRight 5.0f border2) borderColor Sprite.Default
            Draw.rect(Rect.sliceBottom 5.0f border) borderColor Sprite.Default

    override this.OnUpdate(bounds, selected, elapsedTime) =
        if colorVersion < colorVersionGlobal then
            let f key (d: Collections.Generic.Dictionary<string, PersonalBests<_>>) =
                if d.ContainsKey(key) then Some d.[key] else None
            colorVersion <- colorVersionGlobal
            if chartData.IsNone then chartData <- scores.GetScoreData cc.Hash
            match chartData with
            | Some d -> pbData <- (f scoreSystem d.Accuracy |> Option.map (PersonalBests.map (fun x -> x, Grade.calculateFromAcc (themeConfig().GradeThresholds) x)), f scoreSystem d.Lamp, f (scoreSystem + "|" + hpSystem) d.Clear)
            | None -> ()
            color <- colorFunc pbData
            collectionIcon <-
                if options.ChartGroupMode.Value <> "Collections" then
                    match snd Collections.selected with
                    | Collection ccs -> if ccs.Contains cc.FilePath then "✭" else ""
                    | Playlist ps -> if ps.Exists(fun (id, _, _) -> id = cc.FilePath) then "➾" else ""
                    | Goals gs -> if gs.Exists(fun ((id, _, _), _) -> id = cc.FilePath) then "@" else ""
                else ""
        if Mouse.Hover(bounds) then
            hover.Target <- 1.0f
            if Mouse.Click(MouseButton.Left) then
                if selected then playCurrentChart()
                else switchCurrentChart(cc, groupName)
            elif Mouse.Click(MouseButton.Right) then
                expandedGroup <- ""
                scrollTo <- ScrollTo.Pack groupName
            elif options.Hotkeys.Delete.Value.Tapped() then
                let chartName = sprintf "%s [%s]" cc.Title cc.DiffName
                Tooltip.callback (
                    options.Hotkeys.Delete.Value,
                    Localisation.localiseWith [chartName] "misc.Delete",
                    Warning,
                    fun () -> 
                        Library.delete cc
                        LevelSelect.refresh <- true
                        Notification.add (Localisation.localiseWith [chartName] "notification.Deleted", Info)
                )
        else hover.Target <- 0.0f
        hover.Update(elapsedTime) |> ignore
    override this.Update(top, topEdge, elapsedTime) =
        if scrollTo = ScrollTo.Chart && groupName = selectedGroup && this.Selected then
            scrollBy(-top + 500.0f)
            scrollTo <- ScrollTo.Nothing
        base.Update(top, topEdge, elapsedTime)

type private LevelSelectPackItem(name, items: LevelSelectChartItem list) =
    inherit LevelSelectItem()

    override this.Bounds(top) = Rect.create (Render.vwidth * 0.5f) top (Render.vwidth - 15.0f) (top + 65.0f)
    override this.Selected = selectedGroup = name
    member this.Expanded = expandedGroup = name

    override this.Navigate() = ()

    override this.OnDraw(bounds, selected) =
        Draw.rect bounds (if selected then Style.accentShade(127, 1.0f, 0.2f) else Style.accentShade(127, 0.5f, 0.0f)) Sprite.Default
        Text.drawFillB(font(), name, bounds, (Color.White, Color.Black), 0.5f)
    override this.Draw(top, topEdge) =
        let b = base.Draw(top, topEdge)
        if this.Expanded then
            let b2 = List.fold (fun t (i: LevelSelectChartItem) -> i.Draw(t, topEdge)) b items
            if b < topEdge + 170.0f && b2 > topEdge + 170.0f then Text.drawJustB(font(), name, 15.0f, Render.vwidth, topEdge + 180.0f, (Color.White, Color.Black), 1.0f)
            b2
        else b

    override this.OnUpdate(bounds, selected, elapsedTime) =
        if Mouse.Hover(bounds) then
            if Mouse.Click(MouseButton.Left) then
                if this.Expanded then expandedGroup <- "" else (expandedGroup <- name; scrollTo <- ScrollTo.Pack name)
            elif options.Hotkeys.Delete.Value.Tapped() then
                let groupName = sprintf "%s (%i charts)" name (items.Count())
                Tooltip.callback (
                    options.Hotkeys.Delete.Value,
                    Localisation.localiseWith [groupName] "misc.Delete",
                    Warning,
                    fun () ->
                        items |> Seq.map (fun i -> i.Chart) |> Library.deleteMany
                        LevelSelect.refresh <- true
                        Notification.add (Localisation.localiseWith [groupName] "notification.Deleted", Info)
                )

    override this.Update(top, topEdge, elapsedTime) =
        match scrollTo with
        | ScrollTo.Pack s when s = name ->
            if this.Expanded then scrollBy(-top + topEdge + 185.0f) else scrollBy(-top + topEdge + 400.0f)
            scrollTo <- ScrollTo.Nothing
        | _ -> ()
        let b = base.Update(top, topEdge, elapsedTime)
        if this.Expanded then List.fold (fun t (i: LevelSelectChartItem) -> i.Update(t, topEdge, elapsedTime)) b items
        else List.iter (fun (i: LevelSelectChartItem) -> i.Navigate()) items; b

type Screen() as this =
    inherit Screen.T()

    let mutable scrolling = false
    let mutable folderList: LevelSelectPackItem list = []
    let mutable lastItem: (string * CachedChart) option = None
    let mutable filter: Filter = []
    let scrollPos = new AnimationFade(300.0f)
    let searchText = Setting.simple ""
    let infoPanel = new InfoPanel()

    let refresh() =
        scoreSystem <- (fst options.AccSystems.Value).ToString()
        infoPanel.Refresh()
        let groups =
            if options.ChartGroupMode.Value <> "Collections" then
                Library.getGroups groupBy.[options.ChartGroupMode.Value] sortBy.[options.ChartSortMode.Value] filter
            else Library.getCollectionGroups sortBy.[options.ChartSortMode.Value] filter
        if groups.Count = 1 then
            let g = groups.Keys.First()
            if groups.[g].Count = 1 then
                let cc = groups.[g].[0]
                if cc.FilePath <> selectedChart then
                    match Library.load cc with
                    | Some c -> changeChart(cc, c)
                    | None -> Logging.Error("Couldn't load cached file: " + cc.FilePath)
        lastItem <- None
        colorVersionGlobal <- 0
        folderList <-
            groups.Keys
            |> Seq.sort
            |> Seq.map
                (fun k ->
                    groups.[k]
                    |> Seq.map (fun cc ->
                        match currentCachedChart with
                        | None -> ()
                        | Some c -> if c.FilePath = cc.FilePath then selectedChart <- c.FilePath; selectedGroup <- k
                        lastItem <- Some (k, cc)
                        LevelSelectChartItem(k, cc))
                    |> List.ofSeq
                    |> fun l -> LevelSelectPackItem(k, l))
            |> List.ofSeq
        scrollTo <- ScrollTo.Chart
        expandedGroup <- selectedGroup

    let changeRate(v) = Interlude.Gameplay.changeRate(v); colorVersionGlobal <- colorVersionGlobal + 1; infoPanel.Refresh()

    do
        Setting.app (fun s -> if sortBy.ContainsKey s then s else "Title") options.ChartSortMode
        Setting.app (fun s -> if groupBy.ContainsKey s then s else "Pack") options.ChartGroupMode
        this.Animation.Add scrollPos
        scrollBy <- fun (amt: float32) -> scrollPos.Target <- scrollPos.Target + amt

        new TextBox((fun () -> match currentCachedChart with None -> "" | Some c -> c.Title), K (Color.White, Color.Black), 0.5f)
        |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.4f, 100.0f, 0.0f)
        |> this.Add

        new TextBox((fun () -> match currentCachedChart with None -> "" | Some c -> c.DiffName), K (Color.White, Color.Black), 0.5f)
        |> positionWidget(0.0f, 0.0f, 100.0f, 0.0f, 0.0f, 0.4f, 160.0f, 0.0f)
        |> this.Add

        new SearchBox(searchText, fun f -> filter <- f; refresh())
        |> TooltipRegion.Create (Localisation.localise "levelselect.tooltip.Search")
        |> positionWidget(-600.0f, 1.0f, 30.0f, 0.0f, -50.0f, 1.0f, 90.0f, 0.0f)
        |> this.Add

        new ModSelect()
        |> TooltipRegion.Create (Localisation.localise "levelselect.tooltip.Mods")
        |> positionWidget(25.0f, 0.4f, 120.0f, 0.0f, -25.0f, 0.55f, 170.0f, 0.0f)
        |> this.Add

        new CollectionManager()
        |> TooltipRegion.Create (Localisation.localise "levelselect.tooltip.Collections")
        |> positionWidget(0.0f, 0.55f, 120.0f, 0.0f, -25.0f, 0.7f, 170.0f, 0.0f)
        |> this.Add

        let sorts = sortBy.Keys |> Array.ofSeq
        new Dropdown(sorts, Array.IndexOf(sorts, options.ChartSortMode.Value),
            (fun i -> options.ChartSortMode.Value <- sorts.[i]; refresh()), "Sort", 50.0f, fun () -> Style.accentShade(100, 0.4f, 0.6f))
        |> TooltipRegion.Create (Localisation.localise "levelselect.tooltip.SortBy")
        |> positionWidget(0.0f, 0.7f, 120.0f, 0.0f, -25.0f, 0.85f, 400.0f, 0.0f)
        |> this.Add

        let groups = groupBy.Keys |> Array.ofSeq
        new Dropdown(groups, Array.IndexOf(groups, options.ChartGroupMode.Value),
            (fun i -> options.ChartGroupMode.Value <- groups.[i]; refresh()), "Group", 50.0f, fun () -> Style.accentShade(100, 0.2f, 0.8f))
        |> TooltipRegion.Create (Localisation.localise "levelselect.tooltip.GroupBy")
        |> positionWidget(0.0f, 0.85f, 120.0f, 0.0f, 0.0f, 1.0f, 400.0f, 0.0f)
        |> this.Add

        infoPanel
        |> positionWidget(10.0f, 0.0f, 180.0f, 0.0f, -10.0f, 0.4f, 0.0f, 1.0f)
        |> this.Add

        onChartChange <- infoPanel.Refresh

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if LevelSelect.refresh then refresh(); LevelSelect.refresh <- false

        if options.Hotkeys.Select.Value.Tapped() then playCurrentChart()

        elif options.Hotkeys.UpRateSmall.Value.Tapped() then changeRate(0.01f)
        elif options.Hotkeys.UpRateHalf.Value.Tapped() then changeRate(0.05f)
        elif options.Hotkeys.UpRate.Value.Tapped() then changeRate(0.1f)
        elif options.Hotkeys.DownRateSmall.Value.Tapped() then changeRate(-0.01f)
        elif options.Hotkeys.DownRateHalf.Value.Tapped() then changeRate(-0.05f)
        elif options.Hotkeys.DownRate.Value.Tapped() then changeRate(-0.1f)

        elif options.Hotkeys.Next.Value.Tapped() then
            if lastItem.IsSome then
                let (g, c) = lastItem.Value
                navigation <- Navigation.Forward (selectedGroup = g && selectedChart = c.FilePath)
        elif options.Hotkeys.Previous.Value.Tapped() then
            if lastItem.IsSome then navigation <- Navigation.Backward lastItem.Value

        let struct (left, top, right, bottom) = this.Bounds
        let bottomEdge =
            folderList
            |> List.fold (fun t (i: LevelSelectPackItem) -> i.Update(t, top, elapsedTime)) scrollPos.Value
        if Mouse.Click MouseButton.Right then scrolling <- true
        if Mouse.Held MouseButton.Right |> not then scrolling <- false

        let pheight = bottom - top - 170.0f
        let height = bottomEdge - scrollPos.Value - top * 2.0f - 170.0f
        if scrolling then scrollPos.Target <- -(Mouse.Y() - top - 170.0f) / pheight * height
        scrollPos.Target <- Math.Min (Math.Max (scrollPos.Target + Mouse.Scroll() * 100.0f, pheight - height - top), 190.0f + top)

    override this.Draw() =
        let struct (left, top, right, bottom) = this.Bounds
        Stencil.create(false)
        Draw.rect (Rect.create 0.0f (top + 170.0f) Render.vwidth bottom) Color.Transparent Sprite.Default
        Stencil.draw()
        let bottomEdge = folderList |> List.fold (fun t (i: LevelSelectPackItem) -> i.Draw (t, top)) scrollPos.Value
        Stencil.finish()
        let pheight = bottom - top - 170.0f - 40.0f
        let height = bottomEdge - scrollPos.Value - top * 2.0f - 170.0f
        let lb = pheight - height - top
        let ub = 190.0f + top
        let scrollPos = -(scrollPos.Value - ub) / (ub - lb) * pheight
        Draw.rect (Rect.create (Render.vwidth - 10.0f) (top + 170.0f + 10.0f + scrollPos) (Render.vwidth - 5.0f) (top + 170.0f + 30.0f + scrollPos)) Color.White Sprite.Default

        let w = (right - left) * 0.4f
        Draw.quad
            ( Quad.create <| Vector2(left, top) <| Vector2(left + w + 85.0f, top) <| Vector2(left + w, top + 170.0f) <| Vector2(left, top + 170.0f) )
            (Quad.colorOf (Style.accentShade (120, 0.6f, 0.0f))) Sprite.DefaultQuad
        Draw.quad
            ( Quad.create <| Vector2(left + w + 85.0f, top) <| Vector2(right, top) <| Vector2(right, top + 170.0f) <| Vector2(left + w, top + 170.0f) )
            (Quad.colorOf (Style.accentShade (120, 0.1f, 0.0f))) Sprite.DefaultQuad
        Draw.rect (Rect.create left (top + 170.0f) right (top + 175.0f)) (Style.accentShade (255, 0.8f, 0.0f)) Sprite.Default
        base.Draw()

    override this.OnEnter prev =
        Audio.trackFinishBehaviour <- Audio.TrackFinishBehaviour.Action (fun () -> Audio.playFrom currentChart.Value.Header.PreviewTime)
        refresh()

    override this.OnExit next =
        Input.removeInputMethod()