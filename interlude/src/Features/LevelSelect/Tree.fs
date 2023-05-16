namespace Interlude.Features.LevelSelect

open System
open System.Linq
open OpenTK.Mathematics
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Grading
open Prelude.Data.Scores
open Prelude.Data.Charts
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Sorting
open Prelude.Data.Charts.Collections
open Prelude.Data.Charts.Library
open Interlude.UI
open Interlude.Content
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.Features.Play
open Interlude.Features.Online

[<RequireQualifiedAccess>]
type Navigation =
    | Nothing
    | Backward of string * CachedChart * Collections.LibraryContext
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
    
    /// Group's name = this string => Selected chart is in this group
    let mutable private selectedGroup = ""
    /// Chart's filepath = this string && contextIndex match => It's the selected chart
    let mutable private selectedChart = ""
    /// Group's name = this string => That group is expanded in level select
    /// Only one group can be expanded at a time, and it is independent of the "selected" group
    let mutable private expandedGroup = ""
    
    let private scrollPos = Animation.Fade 300.0f
    let private scroll(amount: float32) = scrollPos.Target <- scrollPos.Target + amount

    let mutable private currently_drag_scrolling = false
    let mutable private drag_scroll_distance = 0.0f
    let mutable private drag_scroll_position = 0.0f
    let DRAG_THRESHOLD = 40.0f
    let DRAG_LEFTCLICK_SCALE = 1.75f

    /// Increment the flag to recalculate cached data on tree items
    /// Tree items use this number + their local copy of it to track if they have refreshed their data yet
    let mutable private cacheFlag = 0

    // future todo: different color settings?
    let private colorFunc : Bests option -> Color = 
        function
        | None -> Colors.grey_2.O2
        | Some b -> Colors.white.O2
    
    /// Set these globals to have them "consumed" in the next frame by a level select item with sufficient knowledge to do so
    let mutable private scrollTo = ScrollTo.Nothing

    let private getPb ({ Best = p1, r1; Fastest = p2, r2 }: PersonalBests<'T>) (colorFunc: 'T -> Color) =
        if r1 < rate.Value then ( p2, r2, if r2 < rate.Value then Color.FromArgb(127, Color.White) else colorFunc p2 )
        else ( p1, r1, colorFunc p1 )
    
    let switchChart(cc, context, groupName) =
        match Library.load cc with
        | Some c ->
            Chart.change(cc, context, c)
            Selection.clear()
            selectedChart <- cc.FilePath
            expandedGroup <- groupName
            selectedGroup <- groupName
            scrollTo <- ScrollTo.Chart
        | None -> Logging.Error("Couldn't load cached file: " + cc.FilePath)
    
    let play() =
        if Network.lobby.IsSome then
            Lobby.select_chart(Chart.cacheInfo.Value, rate.Value)
            Screen.change Screen.Type.Lobby Transitions.Flags.Default
        else
            match Chart.saveData with
            | Some data ->
                data.LastPlayed <- DateTime.Now
                Screen.changeNew
                    ( fun () -> 
                        if autoplay then ReplayScreen.replay_screen(ReplayMode.Auto) :> Screen.T
                        else PlayScreen.play_screen(if enablePacemaker then PacemakerMode.Setting else PacemakerMode.None) )
                    ( if autoplay then Screen.Type.Replay else Screen.Type.Play )
                    Transitions.Flags.Default
            | None -> Logging.Warn "There is no chart selected"

    let challengeScore(rate, replay) =
        match Chart.saveData with
        | Some data ->
            data.LastPlayed <- DateTime.Now
            Screen.changeNew
                ( fun () -> PlayScreen.play_screen(PacemakerMode.Score (rate, replay)) )
                ( Screen.Type.Play )
                Transitions.Flags.Default
        | None -> Logging.Warn "There is no chart selected"

    [<AbstractClass>]
    type private TreeItem() =
        abstract member Bounds: float32 -> Rect
        abstract member Selected: bool
        abstract member Spacing: float32

        member this.CheckBounds(top: float32, origin: float32, originB: float32, if_visible: Rect -> unit) =
            let bounds = this.Bounds (top + this.Spacing * 0.5f)
            if bounds.Bottom > origin && top < originB then if_visible bounds
            top + bounds.Height + this.Spacing

        member this.LeftClick(origin) =
            Mouse.released Mouse.LEFT && drag_scroll_distance <= DRAG_THRESHOLD && Mouse.y() > origin

        member this.RightClick(origin) =
            Mouse.released Mouse.RIGHT && drag_scroll_distance <= DRAG_THRESHOLD && Mouse.y() > origin

    type private ChartItem(groupName: string, cc: CachedChart, context: LibraryContext) =
        inherit TreeItem()

        let hover = Animation.Fade 0.0f
        let mutable localCacheFlag = -1
        let mutable color = Color.Transparent
        let mutable chartData = None
        let mutable pbData: Bests option = None
        let mutable markers = ""

        let updateCachedInfo() =
            localCacheFlag <- cacheFlag
            if chartData.IsNone then chartData <- Scores.getData cc.Hash
            match chartData with
            | Some d when d.Bests.ContainsKey Rulesets.current_hash ->
                pbData <- Some d.Bests.[Rulesets.current_hash]
            | _ -> ()
            color <- colorFunc pbData
            markers <-
                if options.LibraryMode.Value <> LibraryMode.Collections then
                    match Collections.current with
                    | Some (Folder c) -> if c.Contains cc then c.Icon.Value + " " else ""
                    | Some (Playlist p) -> if p.Contains cc then p.Icon.Value + " " else ""
                    | Some (Level lvl) -> if context <> LibraryContext.Table lvl.Name && lvl.Contains cc then Icons.table + " " else ""
                    | None -> ""
                else ""
                +
                match chartData with
                | Some c when c.Comment <> "" -> Icons.comment
                | _ -> ""

        override this.Bounds(top) = Rect.Create(Viewport.vwidth * 0.4f, top, Viewport.vwidth, top + 90.0f)
        override this.Selected = selectedChart = cc.FilePath && (context = LibraryContext.None || Chart.context = context)
        override this.Spacing = 5.0f
        member this.Chart = cc

        member this.Select() = switchChart(cc, context, groupName)

        member private this.OnDraw(bounds: Rect) =
            let { Rect.Left = left; Top = top; Right = right; Bottom = bottom } = bounds

            // draw base
            let accent = Style.color(80 + int (hover.Value * 40.0f), 1.0f, 0.4f)
            Draw.rect bounds (if this.Selected then Style.main 80 () else Colors.shadow_1.O2)
            let stripeLength = (right - left) * (0.4f + 0.6f * hover.Value)
            Draw.quad
                (Quad.create <| new Vector2(left, top) <| new Vector2(left + stripeLength, top) <| new Vector2(left + stripeLength, bottom - 25.0f) <| new Vector2(left, bottom - 25.0f))
                (struct(accent, Color.Transparent, Color.Transparent, accent))
                Sprite.DefaultQuad

            let border = bounds.Expand(5.0f, 0.0f)
            let borderColor = if this.Selected then !*Palette.LIGHT else color
            if borderColor.A > 0uy then
                Draw.rect (border.SliceLeft 5.0f) borderColor

            // draw pbs
            let disp (pb: PersonalBests<'T>) (format: 'T -> string) (colorFunc: 'T -> Color) (pos: float32) =
                let value, rate, color = getPb pb colorFunc
                let formatted = format value
                let rateLabel = sprintf "(%.2fx)" rate
                if color.A > 0uy then
                    Draw.rect( Rect.Create(right - pos - 40.0f, top, right - pos + 40.0f, bottom) ) accent
                    Text.drawJustB(Style.baseFont, formatted, 20.0f, right - pos, top + 8.0f, (color, Color.Black), 0.5f)
                    Text.drawJustB(Style.baseFont, rateLabel, 14.0f, right - pos, top + 35.0f, (color, Color.Black), 0.5f)
        
            match pbData with
            | Some d ->
                disp 
                    d.Grade
                    Rulesets.current.GradeName
                    (fun _ -> let (_, _, c) = getPb d.Grade Rulesets.current.GradeColor in c)
                    415.0f
                disp
                    d.Lamp
                    Rulesets.current.LampName
                    Rulesets.current.LampColor
                    290.0f
                disp
                    d.Clear
                    (fun x -> if x then "CLEAR" else "FAILED")
                    Themes.clearToColor
                    165.0f
            | None -> ()

            // draw text
            Draw.rect (bounds.SliceBottom 25.0f) Colors.shadow_1.O1
            Text.drawB(Style.baseFont, cc.Title, 23.0f, left + 5f, top, Colors.text)
            Text.drawB(Style.baseFont, cc.Artist + "  •  " + cc.Creator, 18.0f, left + 5f, top + 34.0f, Colors.text_subheading)
            Text.drawB(Style.baseFont, cc.DiffName, 15.0f, left + 5f, top + 65.0f, Colors.text_subheading)
            Text.drawJustB(Style.baseFont, markers, 25.0f, right - 65.0f, top + 15.0f, Colors.text, Alignment.CENTER)

            if Comments.fade.Value > 0.01f && chartData.IsSome && chartData.Value.Comment <> "" then
                Draw.rect bounds (Style.color(Comments.fade.Alpha * 2 / 3, 1.0f, 0.0f))
                Text.drawFillB(Style.baseFont, chartData.Value.Comment, bounds.Shrink(30.0f, 15.0f), (Colors.white.O4a Comments.fade.Alpha, Colors.shadow_1.O4a Comments.fade.Alpha), Alignment.CENTER)

        member this.Draw(top, origin, originB) = this.CheckBounds(top, origin, originB, this.OnDraw)

        member private this.OnUpdate(origin, bounds, elapsedTime) =

            if localCacheFlag < cacheFlag then updateCachedInfo()

            if Mouse.hover bounds then
                hover.Target <- 1.0f
                if this.LeftClick(origin) then
                    if this.Selected then play()
                    else this.Select()
                elif this.RightClick(origin) then ChartContextMenu(cc, context).Show()
                elif (!|"delete").Tapped() then ChartContextMenu.ConfirmDelete(cc, false)
            else hover.Target <- 0.0f
            hover.Update(elapsedTime) |> ignore

        member this.Update(top, origin, originB, elapsedTime) =
            if scrollTo = ScrollTo.Chart && groupName = selectedGroup && this.Selected then
                scroll(-top + 500.0f)
                scrollTo <- ScrollTo.Nothing
            this.CheckBounds(top, origin, originB, fun b -> this.OnUpdate(origin, b, elapsedTime))

    type private GroupItem(name: string, items: ChartItem list, context: LibraryGroupContext) =
        inherit TreeItem()

        override this.Bounds(top) = Rect.Create(Viewport.vwidth * 0.5f, top, Viewport.vwidth - 15.0f, top + 65.0f)
        override this.Selected = selectedGroup = name
        override this.Spacing = 20.0f

        member this.Items = items
        member this.Expanded = expandedGroup = name

        member this.SelectFirst() = items.First().Select()
        member this.SelectLast() = items.Last().Select()

        member private this.OnDraw(bounds: Rect) =
            Draw.rect (bounds.Translate(10.0f, 10.0f)) (Style.color(255, 0.2f, 0.0f))
            Background.draw (bounds, (Color.FromArgb(40, 40, 40)), 1.5f)
            Draw.rect bounds (if this.Selected then Style.color(120, 1.0f, 0.2f) else Style.color(100, 0.7f, 0.0f))
            Text.drawFillB(Style.baseFont, name, bounds.Shrink 5.0f, Colors.text, 0.5f)

        member this.Draw(top, origin, originB) =
            let b = this.CheckBounds(top, origin, originB, this.OnDraw)
            if this.Expanded then
                let b2 = List.fold (fun t (i: ChartItem) -> i.Draw(t, origin, originB)) b items
                if b < origin && b2 > origin then Text.drawJustB(Style.baseFont, name, 20.0f, Viewport.vwidth - 20f, origin + 10.0f, Colors.text, 1.0f)
                b2
            else b

        member private this.OnUpdate(origin, bounds, elapsedTime) =
            if Mouse.hover bounds then
                if this.LeftClick(origin) then
                    if this.Expanded then expandedGroup <- "" else (expandedGroup <- name; scrollTo <- ScrollTo.Pack name)
                elif this.RightClick(origin) then GroupContextMenu.Show(name, items |> Seq.map (fun (x: ChartItem) -> x.Chart), context)
                elif (!|"delete").Tapped() then GroupContextMenu.ConfirmDelete(name, items |> Seq.map (fun (x: ChartItem) -> x.Chart), false)

        member this.Update(top, origin, originB, elapsedTime) =
            match scrollTo with
            | ScrollTo.Pack s when s = name ->
                if this.Expanded then 
                     scroll(-top + origin + 185.0f)
                else scroll(-top + origin + 400.0f)
                scrollTo <- ScrollTo.Nothing
            | _ -> ()
            let b = this.CheckBounds(top, origin, originB, fun b -> this.OnUpdate(origin, b, elapsedTime))
            if this.Expanded then
                List.fold (fun t (i: ChartItem) -> i.Update(t, origin, originB, elapsedTime)) b items
            else b
            
    
    let mutable filter: Filter = []
    let mutable private groups: GroupItem list = []
    let mutable private lastItem : ChartItem option = None

    let refresh() =
        // fetch groups
        let library_groups =
            let ctx : GroupContext = { Rate = rate.Value; RulesetId = Rulesets.current_hash; Ruleset = Rulesets.current }
            match options.LibraryMode.Value with
            | LibraryMode.Collections -> getCollectionGroups sortBy.[options.ChartSortMode.Value] filter
            | LibraryMode.Table -> getTableGroups sortBy.[options.ChartSortMode.Value] filter
            | LibraryMode.All -> getGroups ctx groupBy.[options.ChartGroupMode.Value] sortBy.[options.ChartSortMode.Value] filter
        // if exactly 1 result, switch to it
        if library_groups.Count = 1 then
            let g = library_groups.Keys.First()
            if library_groups.[g].Charts.Count = 1 then
                let cc, context = library_groups.[g].Charts.[0]
                if cc.FilePath <> selectedChart then
                    switchChart(cc, context, snd g)
        // build groups ui
        lastItem <- None
        groups <-
            library_groups.Keys
            |> Seq.sort
            |> if options.ChartGroupReverse.Value then Seq.rev else id
            |> Seq.map
                (fun (sortIndex, groupName) ->
                    library_groups.[(sortIndex, groupName)].Charts
                    |> Seq.map
                        ( fun (cc, context) ->
                            match Chart.cacheInfo with
                            | None -> ()
                            | Some c -> if c.FilePath = cc.FilePath && (context = LibraryContext.None || context = Chart.context) then selectedChart <- c.FilePath; selectedGroup <- groupName
                            let i = ChartItem(groupName, cc, context)
                            lastItem <- Some i
                            i
                        )
                    |> if options.ChartSortReverse.Value then Seq.rev else id
                    |> List.ofSeq
                    |> fun l -> GroupItem(groupName, l, library_groups.[(sortIndex, groupName)].Context)
                )
            |> List.ofSeq
        cacheFlag <- 0
        expandedGroup <- selectedGroup
        scrollTo <- ScrollTo.Chart

    do 
        LevelSelect.on_refresh_all.Add refresh
        LevelSelect.on_refresh_details.Add (fun () -> cacheFlag <- cacheFlag + 1)

    let previous() =
        match lastItem with
        | Some l ->
            let mutable searching = true
            let mutable last = l
            for g in groups do
                for c in g.Items do
                    if c.Selected && searching then last.Select(); searching <- false else last <- c
            if searching then l.Select()
        | None -> ()

    let next() =
        match lastItem with
        | Some l ->
            let mutable found = false
            let mutable goNext = l.Selected
            for g in groups do
                for c in g.Items do
                    if goNext then c.Select(); goNext <- false; found <- true
                    elif c.Selected then goNext <- true
            if not found then groups.First().Items.First().Select()
        | None -> ()

    let previousGroup() =
        match lastItem with
        | Some _ ->
            let mutable looping = true
            let mutable last = groups.Last()
            for g in groups do
                if g.Selected && looping then last.SelectFirst(); looping <- false else last <- g
        | None -> ()
    
    let nextGroup() =
        match lastItem with
        | Some _ ->
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

    let begin_dragScroll() =
        currently_drag_scrolling <- true
        drag_scroll_position <- Mouse.y()
        drag_scroll_distance <- 0.0f

    let finish_dragScroll() =
        currently_drag_scrolling <- false

    let dragScroll(origin, total_height, tree_height) =
        let d = Mouse.y() - drag_scroll_position
        drag_scroll_position <- Mouse.y()
        drag_scroll_distance <- drag_scroll_distance + abs d
        if Mouse.held Mouse.RIGHT then
            if drag_scroll_distance > DRAG_THRESHOLD then scrollPos.Target <- -(Mouse.y() - origin) / total_height * tree_height
        elif Mouse.held Mouse.LEFT then
            if drag_scroll_distance > DRAG_THRESHOLD then scrollPos.Target <- scrollPos.Target + d * DRAG_LEFTCLICK_SCALE
        else finish_dragScroll()

    let update(origin: float32, originB: float32, elapsedTime: float) =
        scrollPos.Update(elapsedTime) |> ignore
        
        if Dialog.exists() then () else

        let bottomEdge =
            List.fold
                (fun t (i: GroupItem) -> i.Update(t, origin, originB, elapsedTime))
                scrollPos.Value
                groups

        let total_height = originB - origin
        let tree_height = bottomEdge - scrollPos.Value

        let my = Mouse.y()
        if currently_drag_scrolling then dragScroll(origin, total_height, tree_height)
        elif my < originB && my > origin && (Mouse.leftClick() || Mouse.rightClick()) then begin_dragScroll()

        if (!|"up").Tapped() && expandedGroup <> "" then
            scrollTo <- ScrollTo.Pack expandedGroup
            expandedGroup <- ""
        if (!|"down").Tapped() && expandedGroup = "" && selectedGroup <> "" then
            expandedGroup <- selectedGroup
            scrollTo <- ScrollTo.Pack expandedGroup
        elif (!|"context_menu").Tapped() && Chart.cacheInfo.IsSome then
            ChartContextMenu(Chart.cacheInfo.Value, Chart.context).Show()

        scrollPos.Target <- Math.Min (Math.Max (scrollPos.Target + Mouse.scroll() * 100.0f, total_height - tree_height - origin), 20.0f + origin)

    let draw(origin: float32, originB: float32) =
        Stencil.create false
        Draw.rect (Rect.Create(0.0f, origin, Viewport.vwidth, originB)) Color.Transparent
        Stencil.draw()
        let bottomEdge =
            List.fold 
                (fun t (i: GroupItem) -> i.Draw (t, origin, originB))
                scrollPos.Value
                groups
        Stencil.finish()

        let total_height = originB - origin
        let tree_height = bottomEdge - scrollPos.Value
        let lb = total_height - tree_height - origin
        let ub = 20.0f + origin
        let scrollPos = -(scrollPos.Value - ub) / (ub - lb) * (total_height - 40.0f)
        Draw.rect ( Rect.Create(Viewport.vwidth - 10.0f, origin + 10.0f + scrollPos, Viewport.vwidth - 5.0f, origin + 30.0f + scrollPos) ) Color.White
