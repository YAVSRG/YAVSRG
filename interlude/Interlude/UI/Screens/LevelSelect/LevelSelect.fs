namespace Interlude.UI.Screens.LevelSelect

open System
open System.Linq
open OpenTK.Mathematics
open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Prelude.Scoring
open Prelude.Data.Charts
open Prelude.Data.Charts.Sorting
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Collections
open Interlude
open Interlude.UI
open Interlude.Utils
open Interlude.Graphics
open Interlude.Input
open Interlude.Gameplay
open Interlude.Options
open Interlude.UI.Animation
open Interlude.UI.Components
open Interlude.UI.Screens.LevelSelect.Globals

type Screen() as this =
    inherit Screen.T()

    let mutable scrolling = false
    let mutable folderList: GroupItem list = []
    let mutable lastItem: (string * CachedChart * LevelSelectContext) option = None
    let mutable filter: Filter = []
    let scrollPos = new AnimationFade(300.0f)
    let searchText = Setting.simple ""
    let infoPanel = new InfoPanel()

    let refresh() =
        ruleset <- getCurrentRuleset()
        rulesetId <- Ruleset.hash ruleset
        infoPanel.Refresh()
        let groups =
            let ctx : GroupContext = { Rate = rate.Value; RulesetId = rulesetId; Ruleset = ruleset }
            if options.ChartGroupMode.Value <> "Collections" then
                Library.getGroups ctx groupBy.[options.ChartGroupMode.Value] sortBy.[options.ChartSortMode.Value] filter
            else Library.getCollectionGroups sortBy.[options.ChartSortMode.Value] filter
        if groups.Count = 1 then
            let g = groups.Keys.First()
            if groups.[g].Count = 1 then
                let cc, context = groups.[g].[0]
                if cc.FilePath <> selectedChart then
                    match Library.load cc with
                    | Some c -> changeChart(cc, context, c)
                    | None -> Logging.Error("Couldn't load cached file: " + cc.FilePath)
        lastItem <- None
        colorVersionGlobal <- 0
        folderList <-
            groups.Keys
            |> Seq.sort
            |> Seq.map
                (fun (sortIndex, groupName) ->
                    groups.[(sortIndex, groupName)]
                    |> Seq.map (fun (cc, context) ->
                        match currentCachedChart with
                        | None -> ()
                        | Some c -> if c.FilePath = cc.FilePath && context.Id = Collections.contextIndex then selectedChart <- c.FilePath; selectedGroup <- groupName
                        lastItem <- Some (groupName, cc, context)
                        ChartItem(groupName, cc, context))
                    |> List.ofSeq
                    |> fun l -> GroupItem(groupName, l))
            |> List.ofSeq
        scrollTo <- ScrollTo.Chart
        expandedGroup <- selectedGroup

    let changeRate v = rate.Value <- rate.Value + v; colorVersionGlobal <- colorVersionGlobal + 1; infoPanel.Refresh()

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
                let (g, cc, _) = lastItem.Value
                navigation <- Navigation.Forward (selectedGroup = g && selectedChart = cc.FilePath)
        elif options.Hotkeys.Previous.Value.Tapped() then
            if lastItem.IsSome then navigation <- Navigation.Backward lastItem.Value

        let struct (left, top, right, bottom) = this.Bounds
        let bottomEdge =
            folderList |> List.fold (fun t (i: GroupItem) -> i.Update(t, top, elapsedTime)) scrollPos.Value
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
        let bottomEdge = folderList |> List.fold (fun t (i: GroupItem) -> i.Draw (t, top)) scrollPos.Value
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

    override this.OnExit next = Input.removeInputMethod()