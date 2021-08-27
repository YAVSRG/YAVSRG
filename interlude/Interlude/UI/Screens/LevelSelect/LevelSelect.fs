namespace Interlude.UI.Screens.LevelSelect

open System
open System.Drawing
open System.Linq
open OpenTK.Mathematics
open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Prelude.Data.ScoreManager
open Prelude.Data.ChartManager
open Prelude.Data.ChartManager.Sorting
open Prelude.Scoring
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Difficulty
open Interlude
open Interlude.UI
open Interlude.Utils
open Interlude.Graphics
open Interlude.Input
open Interlude.Gameplay
open Interlude.Themes
open Interlude.Options
open Interlude.UI.Animation
open Interlude.UI.Components
open Interlude.UI.Selection

module ScreenLevelSelect =

    //publicly accessible so that other importing can request that the level select is refreshed
    let mutable refresh = false

    open Globals

    [<AutoOpen>]
    module private InfoPanel =

        type ScoreboardSort =
        | Time = 0
        | Performance = 1
        | Accuracy = 2

        type ScoreboardFilter =
        | All = 0
        | CurrentRate = 1
        | CurrentPlaystyle = 2
        | CurrentMods = 3

        type ScoreboardItem(data: ScoreInfoProvider) as this =
            inherit Widget()

            let fade = AnimationFade 0.0f

            do
                data.Physical |> ignore
                data.Lamp |> ignore

                let colfun = fun () -> let a = int (255.0f * fade.Value) in (Color.FromArgb(a, Color.White), Color.FromArgb(a, Color.Black))
                
                TextBox((fun() -> data.Scoring.FormatAccuracy()), colfun, 0.0f)
                |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.5f, 0.0f, 0.6f)
                |> this.Add

                TextBox((fun () -> sprintf "%s  •  %ix  •  %.2f" (data.Lamp.ToString()) data.Scoring.State.BestCombo data.Physical), colfun, 0.0f)
                |> positionWidget(0.0f, 0.0f, 0.0f, 0.6f, 0.0f, 0.5f, 0.0f, 1.0f)
                |> this.Add

                TextBox(K (formatTimeOffset(DateTime.Now - data.ScoreInfo.time)), colfun, 1.0f)
                |> positionWidget(0.0f, 0.5f, 0.0f, 0.6f, 0.0f, 1.0f, 0.0f, 1.0f)
                |> this.Add

                TextBox(K data.Mods, colfun, 1.0f)
                |> positionWidget(0.0f, 0.5f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.6f)
                |> this.Add

                Clickable((fun () -> Globals.newScreen((fun () -> new ScoreScreen(data, (PersonalBestType.None, PersonalBestType.None, PersonalBestType.None)) :> Screen), ScreenType.Score, ScreenTransitionFlag.Default)), ignore)
                |> this.Add

                this.Animation.Add fade
                Animation.Serial(AnimationTimer 150.0, AnimationAction (fun () -> let (l, t, r, b) = this.Anchors in l.Snap(); t.Snap(); r.Snap(); b.Snap(); fade.Target <- 1.0f))
                |> this.Animation.Add

                this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 75.0f, 0.0f)

            override this.Draw() =
                Draw.rect this.Bounds (Globals.accentShade(int (127.0f * fade.Value), 0.8f, 0.0f)) Sprite.Default
                base.Draw()
            member this.Data = data

            override this.Update(elapsedTime, bounds) =
                base.Update(elapsedTime, bounds)
                if Mouse.Hover this.Bounds && options.Hotkeys.Delete.Value.Tapped() then
                    let name = sprintf "%s | %s" (data.Scoring.FormatAccuracy()) (data.Lamp.ToString())
                    Globals.addTooltip(options.Hotkeys.Delete.Value, Localisation.localiseWith [name] "misc.Delete", 2000.0,
                        fun () ->
                            chartSaveData.Value.Scores.Remove data.ScoreInfo |> ignore
                            refresh <- true
                            Globals.addNotification(Localisation.localiseWith [name] "notification.Deleted", NotificationType.Info))

        type Scoreboard() as this =
            inherit Selectable()

            let mutable count = -1
            let filter = Setting.simple ScoreboardFilter.All
            let sort = Setting.map enum int options.ScoreSortMode

            let mutable chart = ""
            let mutable scoring = ""
            let ls = new ListSelectable(true)

            let sorter() : Comparison<Widget> =
                match sort.Value with
                | ScoreboardSort.Accuracy -> Comparison(fun b a -> (a :?> ScoreboardItem).Data.Scoring.Value.CompareTo((b :?> ScoreboardItem).Data.Scoring.Value))
                | ScoreboardSort.Performance -> Comparison(fun b a -> (a :?> ScoreboardItem).Data.Physical.CompareTo((b :?> ScoreboardItem).Data.Physical))
                | ScoreboardSort.Time
                | _ -> Comparison(fun b a -> (a :?> ScoreboardItem).Data.ScoreInfo.time.CompareTo((b :?> ScoreboardItem).Data.ScoreInfo.time))

            let filterer() : Widget -> bool =
                match filter.Value with
                | ScoreboardFilter.CurrentRate -> (fun a -> (a :?> ScoreboardItem).Data.ScoreInfo.rate = rate)
                | ScoreboardFilter.CurrentPlaystyle -> (fun a -> (a :?> ScoreboardItem).Data.ScoreInfo.layout = options.Playstyles.[(a :?> ScoreboardItem).Data.ScoreInfo.keycount - 3])
                | ScoreboardFilter.CurrentMods -> (fun a -> (a :?> ScoreboardItem).Data.ScoreInfo.selectedMods = selectedMods) //nyi
                | _ -> K true

            let flowContainer = new FlowContainer(Sort = sorter(), Filter = filterer())
            let scoreLoader =
                let future = BackgroundTask.futureSeq<ScoreboardItem> "Scoreboard loader" (fun item -> flowContainer.Synchronized(fun () -> flowContainer.Add item))
                fun () ->
                    future
                        (fun () ->
                            flowContainer.Synchronized(flowContainer.Clear)
                            match chartSaveData with
                            | None -> Seq.empty
                            | Some d ->
                                seq { 
                                    for score in d.Scores do
                                        yield ScoreInfoProvider(score, currentChart.Value, fst options.AccSystems.Value, fst options.HPSystems.Value)
                                        |> ScoreboardItem
                                }
                        )

            do
                flowContainer
                |> positionWidgetA(0.0f, 10.0f, 0.0f, -40.0f)
                |> this.Add

                LittleButton.FromEnum("Sort", sort,
                    fun () -> flowContainer.Sort <- sorter())
                |> positionWidget(20.0f, 0.0f, -35.0f, 1.0f, -20.0f, 0.25f, -5.0f, 1.0f)
                |> ls.Add

                LittleButton.FromEnum("Filter", filter, this.Refresh)
                |> positionWidget(20.0f, 0.25f, -35.0f, 1.0f, -20.0f, 0.5f, -5.0f, 1.0f)
                |> ls.Add

                LittleButton((fun () -> scoreSystem), fun () -> Setting.app WatcherSelection.cycleForward options.AccSystems; refresh <- true)
                |> positionWidget(20.0f, 0.5f, -35.0f, 1.0f, -20.0f, 0.75f, -5.0f, 1.0f)
                |> ls.Add

                LittleButton(K <| Localisation.localise "scoreboard.storage.Local", this.Refresh) //nyi
                |> positionWidget(20.0f, 0.75f, -35.0f, 1.0f, -20.0f, 1.0f, -5.0f, 1.0f)
                |> ls.Add

                ls |> this.Add

                let noLocalScores = Localisation.localise "scoreboard.NoLocalScores"
                TextBox((fun () -> if count = 0 then noLocalScores else ""), K (Color.White, Color.Black), 0.5f)
                |> positionWidget(50.0f, 0.0f, 0.0f, 0.3f, -50.0f, 1.0f, 0.0f, 0.5f)
                |> this.Add

            member this.Refresh() =
                let h = match currentCachedChart with Some c -> c.Hash | None -> ""
                if (match chartSaveData with None -> false | Some d -> let v = d.Scores.Count <> count in count <- d.Scores.Count; v) || h <> chart then
                    chart <- h
                    scoreLoader()
                elif scoring <> scoreSystem then
                    let s = fst options.AccSystems.Value
                    for c in flowContainer.Children do (c :?> ScoreboardItem).Data.AccuracyType <- s
                    scoring <- scoreSystem
                flowContainer.Filter <- filterer()

            override this.Update(elapsedTime, bounds) =
                base.Update(elapsedTime, bounds)
                if this.Selected && (ls.Selected <> options.Hotkeys.Scoreboard.Value.Pressed()) then ls.Selected <- not ls.Selected

    type InfoPanel() as this =
        inherit Selectable()

        let scores = Scoreboard()
        let mutable length = ""
        let mutable bpm = ""

        do
            this.Add (new ModSelect())
            this.Add (new CollectionManager())

            scores
            |> positionWidgetA(0.0f, 0.0f, 0.0f, -200.0f)
            |> this.Add

            new TextBox(
                (fun () -> match difficultyRating with None -> "0.00⭐" | Some d -> sprintf "%.2f⭐" d.Physical),
                (fun () -> Color.White, match difficultyRating with None -> Color.Black | Some d -> physicalColor d.Physical), 0.0f)
            |> positionWidget(10.0f, 0.0f, -190.0f, 1.0f, 0.0f, 0.5f, -120.0f, 1.0f)
            |> this.Add

            new TextBox(
                (fun () -> match difficultyRating with None -> "0.00⭐" | Some d -> sprintf "%.2f⭐" d.Technical),
                (fun () -> Color.White, match difficultyRating with None -> Color.Black | Some d -> technicalColor d.Technical), 0.0f)
            |> positionWidget(10.0f, 0.0f, -120.0f, 1.0f, 0.0f, 0.5f, -50.0f, 1.0f)
            |> this.Add

            new TextBox((fun () -> bpm), K (Color.White, Color.Black), 1.0f)
            |> positionWidget(0.0f, 0.5f, -190.0f, 1.0f, -10.0f, 1.0f, -120.0f, 1.0f)
            |> this.Add

            new TextBox((fun () -> length), K (Color.White, Color.Black), 1.0f)
            |> positionWidget(0.0f, 0.5f, -120.0f, 1.0f, -10.0f, 1.0f, -50.0f, 1.0f)
            |> this.Add

            new TextBox((fun () -> getModString(rate, selectedMods)), K (Color.White, Color.Black), 0.0f)
            |> positionWidget(17.0f, 0.0f, -50.0f, 1.0f, -50.0f, 1.0f, -10.0f, 1.0f)
            |> this.Add

            scores.Selected <- true

        member this.Refresh() =
            length <-
                match currentCachedChart with
                | Some cc -> cc.Length
                | None -> 0.0f<ms>
                |> fun x -> x / rate
                |> fun x -> (x / 1000.0f / 60.0f |> int, (x / 1000f |> int) % 60)
                |> fun (x, y) -> sprintf "⌛ %i:%02i" x y
            bpm <-
                match currentCachedChart with
                | Some cc -> cc.BPM
                | None -> (500.0f<ms/beat>, 500.0f<ms/beat>)
                |> fun (b, a) -> (60000.0f<ms> / a * rate |> int, 60000.0f<ms> / b * rate |> int)
                |> fun (a, b) ->
                    if Math.Abs(a - b) < 5 || b > 9000 then sprintf "♬ %i" a
                    elif a > 9000 || b < 0 then sprintf "♬ ∞"
                    else sprintf "♬ %i-%i" a b
            scores.Refresh()

    [<AbstractClass>]
    type LevelSelectItem() =
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

    type LevelSelectChartItem(groupName, cc) =
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
            | Forward b ->
                if b then
                    switchCurrentChart(cc, groupName)
                    navigation <- Navigation.Nothing
                elif groupName = selectedGroup && this.Selected then navigation <- Forward true
            | Backward (groupName2, cc2) ->
                if groupName = selectedGroup && this.Selected then
                    switchCurrentChart(cc2, groupName2)
                    navigation <- Navigation.Nothing
                else navigation <- Backward(groupName, cc)

        override this.OnDraw(bounds, selected) =
            let struct (left, top, right, bottom) = bounds
            let accent = Globals.accentShade(80 + int (hover.Value * 40.0f), 1.0f, 0.2f)
            Draw.rect bounds (Globals.accentShade(80, 1.0f, 0.0f)) Sprite.Default
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

            f accAndGrades (fun (x, _) -> sprintf "%.2f%%" (100.0 * x)) (snd >> ScoreColor.gradeToColor) 450.0f
            f lamp (fun x -> x.ToString()) ScoreColor.lampToColor 300.0f
            f clear (fun x -> if x then "CLEAR" else "FAILED") ScoreColor.clearToColor 150.0f

            Draw.rect(Rect.sliceBottom 25.0f bounds) (Globals.accentShade(70, 0.3f, 0.0f)) Sprite.Default
            Text.drawB(font(), cc.Title, 23.0f, left, top, (Color.White, Color.Black))
            Text.drawB(font(), cc.Artist + "  •  " + cc.Creator, 18.0f, left, top + 34.0f, (Color.White, Color.Black))
            Text.drawB(font(), cc.DiffName, 15.0f, left, top + 65.0f, (Color.White, Color.Black))
            Text.drawB(font(), collectionIcon, 35.0f, right - 95.0f, top + 10.0f, (Color.White, Color.Black))

            let border = Rect.expand(5.0f, 5.0f) bounds
            let border2 = Rect.expand(5.0f, 0.0f) bounds
            let borderColor = if selected then Globals.accentShade(180, 1.0f, 0.5f) else color
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
                | Some d -> pbData <- (f scoreSystem d.Accuracy |> Option.map (PersonalBests.map (fun x -> x, Grade.calculateFromAcc themeConfig.GradeThresholds x)), f scoreSystem d.Lamp, f (scoreSystem + "|" + hpSystem) d.Clear)
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
                    scrollTo <- ScrollToPack groupName
                elif options.Hotkeys.Delete.Value.Tapped() then
                    Globals.addTooltip(options.Hotkeys.Delete.Value, Localisation.localiseWith [cc.Title] "misc.Delete", 2000.0,
                        fun () ->
                            cache.DeleteChart cc
                            refresh <- true
                            Globals.addNotification(Localisation.localiseWith [cc.Title] "notification.Deleted", NotificationType.Info))
            else hover.Target <- 0.0f
            hover.Update(elapsedTime) |> ignore
        override this.Update(top, topEdge, elapsedTime) =
            if scrollTo = ScrollToChart && groupName = selectedGroup && this.Selected then
                scrollBy(-top + 500.0f)
                scrollTo <- ScrollTo.Nothing
            base.Update(top, topEdge, elapsedTime)

    type LevelSelectPackItem(name, items: LevelSelectChartItem list) =
        inherit LevelSelectItem()

        override this.Bounds(top) = Rect.create (Render.vwidth * 0.5f) top (Render.vwidth - 15.0f) (top + 65.0f)
        override this.Selected = selectedGroup = name
        member this.Expanded = expandedGroup = name

        override this.Navigate() = ()

        override this.OnDraw(bounds, selected) =
            Draw.rect bounds (if selected then Globals.accentShade(127, 1.0f, 0.2f) else Globals.accentShade(127, 0.5f, 0.0f)) Sprite.Default
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
                    if this.Expanded then expandedGroup <- "" else (expandedGroup <- name; scrollTo <- ScrollToPack name)
                elif options.Hotkeys.Delete.Value.Tapped() then
                    Globals.addTooltip(options.Hotkeys.Delete.Value, Localisation.localiseWith [name] "misc.Delete", 2000.0,
                        fun () ->
                            items |> Seq.map (fun i -> i.Chart) |> cache.DeleteCharts
                            refresh <- true
                            Globals.addNotification(Localisation.localiseWith [name] "notification.Deleted", NotificationType.Info))

        override this.Update(top, topEdge, elapsedTime) =
            match scrollTo with
            | ScrollToPack s when s = name ->
                if this.Expanded then scrollBy(-top + topEdge + 185.0f) else scrollBy(-top + topEdge + 400.0f)
                scrollTo <- ScrollTo.Nothing
            | _ -> ()
            let b = base.Update(top, topEdge, elapsedTime)
            if this.Expanded then List.fold (fun t (i: LevelSelectChartItem) -> i.Update(t, topEdge, elapsedTime)) b items
            else List.iter (fun (i: LevelSelectChartItem) -> i.Navigate()) items; b

open ScreenLevelSelect
open Globals

type ScreenLevelSelect() as this =
    inherit Screen()

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
                cache.GetGroups groupBy.[options.ChartGroupMode.Value] sortBy.[options.ChartSortMode.Value] filter
            else cache.GetCollectionGroups sortBy.[options.ChartSortMode.Value] filter
        if groups.Count = 1 then
            let g = groups.Keys.First()
            if groups.[g].Count = 1 then
                let cc = groups.[g].[0]
                if cc.FilePath <> selectedChart then
                    match cache.LoadChart(cc) with
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
        scrollTo <- ScrollToChart
        expandedGroup <- selectedGroup

    let changeRate(v) = Interlude.Gameplay.changeRate(v); colorVersionGlobal <- colorVersionGlobal + 1; infoPanel.Refresh()

    do
        Setting.app (fun s -> if sortBy.ContainsKey s then s else "Title") options.ChartSortMode
        Setting.app (fun s -> if groupBy.ContainsKey s then s else "Pack") options.ChartGroupMode
        this.Animation.Add scrollPos
        scrollBy <- fun (amt: float32) -> scrollPos.Target <- scrollPos.Target + amt

        let sorts = sortBy.Keys |> Array.ofSeq
        new Dropdown(sorts, Array.IndexOf(sorts, options.ChartSortMode.Value),
            (fun i -> options.ChartSortMode.Value <- sorts.[i]; refresh()), "Sort by", 50.0f)
        |> positionWidget(-400.0f, 1.0f, 100.0f, 0.0f, -250.0f, 1.0f, 400.0f, 0.0f)
        |> this.Add

        let groups = groupBy.Keys |> Array.ofSeq
        new Dropdown(groups, Array.IndexOf(groups, options.ChartGroupMode.Value),
            (fun i -> options.ChartGroupMode.Value <- groups.[i]; refresh()), "Group by", 50.0f)
        |> positionWidget(-200.0f, 1.0f, 100.0f, 0.0f, -50.0f, 1.0f, 400.0f, 0.0f)
        |> this.Add

        new SearchBox(searchText, fun f -> filter <- f; refresh())
        |> positionWidget(-600.0f, 1.0f, 20.0f, 0.0f, -50.0f, 1.0f, 80.0f, 0.0f)
        |> this.Add

        new TextBox((fun () -> match currentCachedChart with None -> "" | Some c -> c.Title), K (Color.White, Color.Black), 0.5f)
        |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.4f, 100.0f, 0.0f)
        |> this.Add

        new TextBox((fun () -> match currentCachedChart with None -> "" | Some c -> c.DiffName), K (Color.White, Color.Black), 0.5f)
        |> positionWidget(0.0f, 0.0f, 100.0f, 0.0f, 0.0f, 0.4f, 160.0f, 0.0f)
        |> this.Add

        infoPanel
        |> positionWidget(10.0f, 0.0f, 180.0f, 0.0f, -10.0f, 0.4f, 0.0f, 1.0f)
        |> this.Add

        onChartChange <- infoPanel.Refresh

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if ScreenLevelSelect.refresh then refresh(); ScreenLevelSelect.refresh <- false

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

        Draw.rect (Rect.create left top right (top + 170.0f)) (Globals.accentShade (100, 0.6f, 0.0f)) Sprite.Default
        Draw.rect (Rect.create left (top + 170.0f) right (top + 175.0f)) (Globals.accentShade (255, 0.8f, 0.0f)) Sprite.Default
        base.Draw()

    override this.OnEnter prev =
        Audio.trackFinishBehaviour <- Audio.TrackFinishBehaviour.Action (fun () -> Audio.playFrom currentChart.Value.Header.PreviewTime)
        refresh()

    override this.OnExit next =
        Input.removeInputMethod()