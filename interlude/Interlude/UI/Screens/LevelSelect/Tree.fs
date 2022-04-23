namespace Interlude.UI.Screens.LevelSelect

open System
open System.Linq
open OpenTK.Mathematics
open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Prelude.Scoring.Grading
open Prelude.Data.Scores
open Prelude.Data.Charts
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Sorting
open Prelude.Data.Charts.Collections
open Interlude.UI
open Interlude.Graphics
open Interlude.Input
open Interlude.Content
open Interlude.Options
open Interlude.Gameplay
open Interlude.UI.Animation
open Interlude.UI.Screens.Play
open Interlude.UI.Components.Selection.Controls

[<RequireQualifiedAccess>]
type Navigation =
| Nothing
| Backward of string * CachedChart * Collections.LevelSelectContext
/// Forward false is consumed by the selected chart, and replaced with Forward true
/// Forward true is consumed by any other chart, and switched to instantly
/// Combined effect is navigating forward by one chart
| Forward of bool
| ForwardGroup of bool
| BackwardGroup of string
| StartGroup
| EndGroup

[<Struct>]
[<RequireQualifiedAccess>]
type ScrollTo =
| Nothing
| Chart
| Pack of string

module Tree = 
    
    (*
         functionality wishlist:
         - "random chart" hotkey
         - cropping of text that is too long
        
         eventual todo:
         - goals and playlists editor
    *)
    
    /// Group's name = this string => Selected chart is in this group
    let mutable private selectedGroup = ""
    /// Chart's filepath = this string && contextIndex match => It's the selected chart
    let mutable private selectedChart = ""
    /// Group's name = this string => That group is expanded in level select
    /// Only one group can be expanded at a time, and it is independent of the "selected" group
    let mutable private expandedGroup = ""
    
    let private scrollPos = new AnimationFade(300.0f)
    let private scroll(amount: float32) = scrollPos.Target <- scrollPos.Target + amount
    let mutable private right_click_scrolling = false

    /// Flag that is cached by items in level select tree.
    /// When the value increases by 1, update cache and recalculate certain things like color
    /// Gets incremented whenever this needs to happen
    let mutable private cacheFlag = 0

    // future todo: different color settings?
    let private colorFunc : Bests option -> Color = 
        function
        | None -> Color.FromArgb(90, 150, 150, 150)
        | Some b -> Color.FromArgb(80, Color.White)
    
    /// Set these globals to have them "consumed" in the next frame by a level select item with sufficient knowledge to do so
    let mutable private scrollTo = ScrollTo.Nothing

    let mutable private dropdownMenu : Dropdown.Container option = None

    let private showDropdown(cc: CachedChart) (context: LevelSelectContext) (tree_x, tree_y) =
        if dropdownMenu.IsSome then dropdownMenu.Value.Destroy()
        let d = 
            Dropdown.create (
                CollectionManager.dropdownMenuOptions(cc, context) @
                [
                    sprintf "%s Add to table" Icons.sparkle, ignore
                    sprintf "%s Delete" Icons.delete, ignore
                    sprintf "%s Edit note" Icons.tag, ignore
                ] ) (fun () -> dropdownMenu <- None)
        dropdownMenu <- Some d
        d.Reposition(tree_x, 0.0f, tree_y, 0.0f, tree_x + 400.0f, 0.0f, tree_y + Dropdown.ITEMSIZE * 5.0f, 0.0f)

    let private getPb ({ Best = p1, r1; Fastest = p2, r2 }: PersonalBests<'T>) (colorFunc: 'T -> Color) =
        if r1 < rate.Value then ( p2, r2, if r2 < rate.Value then Color.FromArgb(127, Color.White) else colorFunc p2 )
        else ( p1, r1, colorFunc p1 )
    
    let private switchChart(cc, context, groupName) =
        match Library.load cc with
        | Some c ->
            Chart.change(cc, context, c)
            selectedChart <- cc.FilePath
            expandedGroup <- groupName
            selectedGroup <- groupName
            scrollTo <- ScrollTo.Chart
        | None -> Logging.Error("Couldn't load cached file: " + cc.FilePath)
    
    let play() =
        match Chart.saveData with
        | Some data ->
            data.LastPlayed <- DateTime.Now
            Screen.changeNew
                ( fun () -> if autoplay then ReplayScreen(ReplayMode.Auto) :> Screen.T else Screen() )
                ( if autoplay then Screen.Type.Replay else Screen.Type.Play )
                Screen.TransitionFlag.Default
        | None -> Logging.Warn "There is no chart selected"

    [<AbstractClass>]
    type private TreeItem() =
        abstract member Bounds: float32 -> Rect
        abstract member Selected: bool

        member this.CheckBounds(top: float32, origin: float32, originB: float32, if_visible: Rect -> unit) =
            let bounds = this.Bounds top
            let struct (_, _, _, bottom) = bounds
            if bottom > origin && top < originB then if_visible bounds
            top + Rect.height bounds + 15.0f

    type private ChartItem(groupName: string, cc: CachedChart, context: LevelSelectContext) =
        inherit TreeItem()

        let hover = new AnimationFade(0.0f)
        let mutable localCacheFlag = -1
        let mutable color = Color.Transparent
        let mutable chartData = None
        let mutable pbData: Bests option = None
        let mutable collectionIcon = ""

        override this.Bounds(top) = Rect.create (Render.vwidth * 0.4f) top Render.vwidth (top + 90.0f)
        override this.Selected = selectedChart = cc.FilePath && Chart.context = context
        member this.Chart = cc

        member this.Select() = switchChart(cc, context, groupName)

        member private this.OnDraw(bounds) =
            let struct (left, top, right, bottom) = bounds

            // draw base
            let accent = Style.accentShade(80 + int (hover.Value * 40.0f), 1.0f, 0.2f)
            Draw.rect bounds (if this.Selected then Style.main 80 () else Style.black 80 ()) Sprite.Default
            let stripeLength = (right - left) * (0.4f + 0.6f * hover.Value)
            Draw.quad
                (Quad.create <| new Vector2(left, top) <| new Vector2(left + stripeLength, top) <| new Vector2(left + stripeLength * 0.9f, bottom - 25.0f) <| new Vector2(left, bottom - 25.0f))
                (struct(accent, Color.Transparent, Color.Transparent, accent))
                Sprite.DefaultQuad

            let border = Rect.expand(5.0f, 5.0f) bounds
            let border2 = Rect.expand(5.0f, 0.0f) bounds
            let borderColor = if this.Selected then Style.accentShade(180, 1.0f, 0.5f) else color
            if borderColor.A > 0uy then
                Draw.rect(Rect.sliceLeft 5.0f border2) borderColor Sprite.Default
                Draw.rect(Rect.sliceTop 5.0f border) borderColor Sprite.Default
                Draw.rect(Rect.sliceRight 5.0f border2) borderColor Sprite.Default
                Draw.rect(Rect.sliceBottom 5.0f border) borderColor Sprite.Default

            // draw pbs
            let disp (pb: PersonalBests<'T>) (format: 'T -> string) (colorFunc: 'T -> Color) (pos: float32) =
                let value, rate, color = getPb pb colorFunc
                let formatted = format value
                let rateLabel = sprintf "(%.2fx)" rate
                if color.A > 0uy then
                    Draw.rect(Rect.create (right - pos - 40.0f) top (right - pos + 40.0f) bottom) accent Sprite.Default
                    Text.drawJustB(font, formatted, 20.0f, right - pos, top + 8.0f, (color, Color.Black), 0.5f)
                    Text.drawJustB(font, rateLabel, 14.0f, right - pos, top + 35.0f, (color, Color.Black), 0.5f)
        
            match pbData with
            | Some d ->
                disp 
                    d.Grade
                    ruleset.GradeName
                    (fun _ -> let (_, _, c) = getPb d.Grade ruleset.GradeColor in c)
                    425.0f
                disp
                    d.Lamp
                    ruleset.LampName
                    ruleset.LampColor
                    300.0f
                disp
                    d.Clear
                    (fun x -> if x then "CLEAR" else "FAILED")
                    Themes.clearToColor
                    175.0f
            | None -> ()

            // draw text
            Draw.rect(Rect.sliceBottom 25.0f bounds) (Color.FromArgb(60, 0, 0, 0)) Sprite.Default
            Text.drawB(font, cc.Title, 23.0f, left + 5f, top, (Color.White, Color.Black))
            Text.drawB(font, cc.Artist + "  •  " + cc.Creator, 18.0f, left + 5f, top + 34.0f, (Color.White, Color.Black))
            Text.drawB(font, cc.DiffName, 15.0f, left + 5f, top + 65.0f, (Color.White, Color.Black))
            Text.drawB(font, collectionIcon, 35.0f, right - 95.0f, top + 10.0f, (Color.White, Color.Black))

        member this.Draw(top, origin, originB) = this.CheckBounds(top, origin, originB, this.OnDraw)

        member private this.OnUpdate(bounds, elapsedTime, origin) =
            if localCacheFlag < cacheFlag then
                localCacheFlag <- cacheFlag
                if chartData.IsNone then chartData <- Scores.getScoreData cc.Hash
                match chartData with
                | Some d when d.Bests.ContainsKey rulesetId ->
                    pbData <- Some d.Bests.[rulesetId]
                | _ -> ()
                color <- colorFunc pbData
                collectionIcon <-
                    if options.ChartGroupMode.Value <> "Collections" then
                        match Collections.selectedCollection with
                        | Collection ccs -> if ccs.Contains cc.FilePath then Icons.star else ""
                        | Playlist ps -> if ps.Exists(fun (id, _) -> id = cc.FilePath) then Icons.playlist else ""
                        | Goals gs -> if gs.Exists(fun (id, _) -> id = cc.FilePath) then Icons.goal else ""
                    else ""
            if Mouse.Hover bounds then
                hover.Target <- 1.0f
                if Mouse.Click MouseButton.Left then
                    if this.Selected then play()
                    else this.Select()
                elif Mouse.Click MouseButton.Right then
                    let struct (l, t, r, b) = bounds
                    showDropdown cc context (min (Render.vwidth - 405f) (Mouse.X()), Mouse.Y() - scrollPos.Value - origin)
                elif (!|Hotkey.Delete).Tapped() then
                    let chartName = sprintf "%s [%s]" cc.Title cc.DiffName
                    Tooltip.callback (
                        (!|Hotkey.Delete),
                        Localisation.localiseWith [chartName] "misc.delete",
                        NotificationType.Warning,
                        fun () -> 
                            Library.delete cc
                            LevelSelect.refresh <- true
                            Notification.add (Localisation.localiseWith [chartName] "notification.deleted", NotificationType.Info)
                    )
            else hover.Target <- 0.0f
            hover.Update(elapsedTime) |> ignore

        member this.Update(top, origin, originB, elapsedTime) =
            if scrollTo = ScrollTo.Chart && groupName = selectedGroup && this.Selected then
                scroll(-top + 500.0f)
                scrollTo <- ScrollTo.Nothing
            this.CheckBounds(top, origin, originB, fun b -> this.OnUpdate(b, elapsedTime, origin))

    type private GroupItem(name: string, items: ChartItem list) =
        inherit TreeItem()

        override this.Bounds(top) = Rect.create (Render.vwidth * 0.5f) top (Render.vwidth - 15.0f) (top + 65.0f)
        override this.Selected = selectedGroup = name

        member this.Items = items
        member this.Expanded = expandedGroup = name

        member this.SelectFirst() = items.First().Select()
        member this.SelectLast() = items.Last().Select()

        member private this.OnDraw(bounds) =
            let borderb = Rect.expand(5.0f, 5.0f) bounds
            let colorb = if this.Selected then Style.accentShade(200, 1.0f, 0.5f) else Style.accentShade(100, 0.7f, 0.0f)
            Draw.rect (Rect.sliceLeft 5.0f borderb) colorb Sprite.Default
            Draw.rect (Rect.sliceRight 5.0f borderb) colorb Sprite.Default
            Draw.rect (Rect.sliceTop 5.0f borderb) colorb Sprite.Default
            Draw.rect (Rect.sliceBottom 5.0f borderb) colorb Sprite.Default
            Draw.rect bounds (if this.Selected then Style.accentShade(127, 1.0f, 0.2f) else Style.accentShade(127, 0.3f, 0.0f)) Sprite.Default
            Text.drawFillB(font, name, bounds |> Rect.expand(-5.0f, -5.0f), (Color.White, Color.Black), 0.5f)

        member this.Draw(top, origin, originB) =
            let b = this.CheckBounds(top, origin, originB, this.OnDraw)
            if this.Expanded then
                let b2 = List.fold (fun t (i: ChartItem) -> i.Draw(t, origin, originB)) b items
                if b < origin && b2 > origin then Text.drawJustB(font, name, 20.0f, Render.vwidth - 20f, origin + 10.0f, (Color.White, Color.Black), 1.0f)
                b2
            else b

        member private this.OnUpdate(bounds, elapsedTime) =
            if Mouse.Hover(bounds) then
                if Mouse.Click(MouseButton.Left) then
                    if this.Expanded then expandedGroup <- "" else (expandedGroup <- name; scrollTo <- ScrollTo.Pack name)
                elif (!|Hotkey.Delete).Tapped() then
                    let groupName = sprintf "%s (%i charts)" name (items.Count())
                    Tooltip.callback (
                        (!|Hotkey.Delete),
                        Localisation.localiseWith [groupName] "misc.delete",
                        NotificationType.Warning,
                        fun () ->
                            items |> Seq.map (fun i -> i.Chart) |> Library.deleteMany
                            LevelSelect.refresh <- true
                            Notification.add (Localisation.localiseWith [groupName] "notification.deleted", NotificationType.Info)
                    )

        member this.Update(top, origin, originB, elapsedTime) =
            match scrollTo with
            | ScrollTo.Pack s when s = name ->
                if this.Expanded then 
                     scroll(-top + origin + 185.0f)
                else scroll(-top + origin + 400.0f)
                scrollTo <- ScrollTo.Nothing
            | _ -> ()
            let b = this.CheckBounds(top, origin, originB, fun b -> this.OnUpdate(b, elapsedTime))
            if this.Expanded then
                List.fold (fun t (i: ChartItem) -> i.Update(t, origin, originB, elapsedTime)) b items
            else b
            
    
    let mutable filter: Filter = []
    let mutable private groups: GroupItem list = []
    let mutable private lastItem : ChartItem option = None

    let updateDisplay() =
        cacheFlag <- cacheFlag + 1

    let refresh() =
        // fetch groups
        let library_groups =
            let ctx : GroupContext = { Rate = rate.Value; RulesetId = rulesetId; Ruleset = ruleset }
            match options.ChartGroupMode.Value with
            | "Collections" -> Library.getCollectionGroups sortBy.[options.ChartSortMode.Value] filter
            | "Table" -> Library.getTableGroups sortBy.[options.ChartSortMode.Value] filter
            | grouping -> Library.getGroups ctx groupBy.[grouping] sortBy.[options.ChartSortMode.Value] filter
        // if exactly 1 result, switch to it
        if library_groups.Count = 1 then
            let g = library_groups.Keys.First()
            if library_groups.[g].Count = 1 then
                let cc, context = library_groups.[g].[0]
                if cc.FilePath <> selectedChart then
                    switchChart(cc, context, snd g)
        // build groups ui
        lastItem <- None
        groups <-
            library_groups.Keys
            |> Seq.sort
            |> Seq.map
                (fun (sortIndex, groupName) ->
                    library_groups.[(sortIndex, groupName)]
                    |> Seq.map
                        ( fun (cc, context) ->
                            match Chart.cacheInfo with
                            | None -> ()
                            | Some c -> if c.FilePath = cc.FilePath && context = Chart.context then selectedChart <- c.FilePath; selectedGroup <- groupName
                            let i = ChartItem(groupName, cc, context)
                            lastItem <- Some i
                            i
                        )
                    |> List.ofSeq
                    |> fun l -> GroupItem(groupName, l))
            |> List.ofSeq
        cacheFlag <- 0
        expandedGroup <- selectedGroup
        scrollTo <- ScrollTo.Chart

    let previous() =
        match lastItem with
        | Some l ->
            let mutable looping = true
            let mutable last = l
            for g in groups do
                for c in g.Items do
                    if c.Selected && looping then last.Select(); looping <- false else last <- c
        | None -> ()

    let next() =
        match lastItem with
        | Some l ->
            let mutable goNext = l.Selected
            for g in groups do
                for c in g.Items do
                    if goNext then c.Select(); goNext <- false
                    elif c.Selected then goNext <- true
        | None -> ()

    let previousGroup() =
        match lastItem with
        | Some l ->
            let mutable looping = true
            let mutable last = groups.Last()
            for g in groups do
                if g.Selected && looping then last.SelectFirst(); looping <- false else last <- g
        | None -> ()
    
    let nextGroup() =
        match lastItem with
        | Some l ->
            let mutable goNext = groups.Last().Selected
            for g in groups do
                if goNext then g.SelectFirst(); goNext <- false
                elif g.Selected then goNext <- true
        | None -> ()

    let beginGroup() =
        for g in groups do
            if g.Selected then g.SelectFirst()
        
    let endGroup() =
        for g in groups do
            if g.Selected then g.SelectLast()

    let update(origin: float32, originB: float32, elapsedTime: float) =
        if LevelSelect.minorRefresh then LevelSelect.minorRefresh <- false; updateDisplay()
        scrollPos.Update(elapsedTime) |> ignore
        
        if dropdownMenu.IsSome then dropdownMenu.Value.Update(elapsedTime, Rect.create 0.0f (origin + scrollPos.Value) Render.vwidth (originB + scrollPos.Value))
        let bottomEdge =
            List.fold 
                (fun t (i: GroupItem) -> i.Update(t, origin, originB, elapsedTime))
                scrollPos.Value
                groups

        let total_height = originB - origin
        let tree_height = bottomEdge - scrollPos.Value
        if Mouse.Click MouseButton.Right then right_click_scrolling <- true
        if not (Mouse.Held MouseButton.Right) then right_click_scrolling <- false
        if (!|Hotkey.Up).Tapped() && expandedGroup <> "" then
            scrollTo <- ScrollTo.Pack expandedGroup
            expandedGroup <- ""
        if right_click_scrolling then scrollPos.Target <- -(Mouse.Y() - origin) / total_height * tree_height

        scrollPos.Target <- Math.Min (Math.Max (scrollPos.Target + Mouse.Scroll() * 100.0f, total_height - tree_height - origin), 20.0f + origin)

    let draw(origin: float32, originB: float32) =
        Stencil.create(false)
        Draw.rect (Rect.create 0.0f origin Render.vwidth originB) Color.Transparent Sprite.Default
        Stencil.draw()
        let bottomEdge =
            List.fold 
                (fun t (i: GroupItem) -> i.Draw (t, origin, originB))
                scrollPos.Value
                groups
        if dropdownMenu.IsSome then dropdownMenu.Value.Draw()
        Stencil.finish()

        let total_height = originB - origin
        let tree_height = bottomEdge - scrollPos.Value
        let lb = total_height - tree_height - origin
        let ub = 20.0f + origin
        let scrollPos = -(scrollPos.Value - ub) / (ub - lb) * (total_height - 40.0f)
        Draw.rect (Rect.create (Render.vwidth - 10.0f) (origin + 10.0f + scrollPos) (Render.vwidth - 5.0f) (origin + 30.0f + scrollPos)) Color.White Sprite.Default
