namespace Interlude.UI

open System
open System.Drawing
open System.Linq
open OpenTK.Mathematics
open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Prelude.Data.ScoreManager
open Prelude.Data.ChartManager
open Prelude.Data.ChartManager.Sorting
open Prelude.Gameplay.Score
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Difficulty
open Interlude.Gameplay
open Interlude.Themes
open Interlude.Utils
open Interlude.Render
open Interlude.Options
open Interlude.Input
open Interlude.UI.Animation
open Interlude.UI.Components
open Interlude.UI.Selection

module private ScreenLevelSelectVars =

    //functionality wishlist:
    // - hotkeys to navigate by pack/close and open quickly
    // - display of keycount for charts
    // - fix for scoreboard allowing clicking of culled objects
    // - nicer looking pack buttons
    // - "random chart" hotkey
    // - ability to delete charts
    // - cropping of text that is too long
    
    //eventual todo:
    // - goals collections and playlists editor
    // - charts in the current collection/goal/playlist you are editing have a * or something by them

    let mutable selectedGroup = ""
    let mutable selectedChart = "" //filepath
    let mutable expandedGroup = ""
    let mutable scrollBy = ignore
    let mutable colorVersionGlobal = 0
    //future todo: different color settings?
    let mutable colorFunc = fun (_, _, _) -> Color.FromArgb(40, 200, 200, 200)

    //updated whenever screen refreshes
    let mutable scoreSystem = "SC+ (J4)"
    let mutable hpSystem = "VG"

    [<Struct>]
    type Navigation =
    | Nothing
    | Backward of string * CachedChart
    | Forward of bool

    [<Struct>]
    type ScrollTo =
    | Nothing
    | ScrollToChart
    | ScrollToPack of string

    let mutable scrollTo = ScrollTo.Nothing
    let mutable navigation = Navigation.Nothing

    let switchCurrentChart(cc, groupName) =
        match cache.LoadChart(cc) with
        | Some c ->
            changeChart(cc, c)
            selectedChart <- cc.FilePath
            expandedGroup <- groupName
            selectedGroup <- groupName
            scrollTo <- ScrollToChart
        | None -> Logging.Error("Couldn't load cached file: " + cc.FilePath) ""

    let playCurrentChart() =
        if currentChart.IsSome then Screens.newScreen(ScreenPlay >> (fun s -> s :> Screen), ScreenType.Play, ScreenTransitionFlag.Default)
        else Logging.Warn("Tried to play selected chart; There is no chart selected") ""

module ScreenLevelSelect =

    //publicly accessible so that other importing can request that the level select is refreshed
    let mutable refresh = false

    open ScreenLevelSelectVars

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

            do
                TextBox((fun () -> sprintf "%s  •  %i" (data.Accuracy.Format()) (let (_, _, _, _, _, cbs) = data.Accuracy.State in cbs)), K (Color.White, Color.Black), 0.0f)
                |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.5f, 0.0f, 0.6f)
                |> this.Add

                TextBox((fun () -> sprintf "%s  •  %ix  •  %.2f" (data.Lamp.ToString()) (let (_, _, _, _, combo, _) = data.Accuracy.State in combo) data.Physical), K (Color.White, Color.Black), 0.0f)
                |> positionWidget(0.0f, 0.0f, 0.0f, 0.6f, 0.0f, 0.5f, 0.0f, 1.0f)
                |> this.Add

                TextBox(K (formatTimeOffset(DateTime.Now - data.Score.time)), K (Color.White, Color.Black), 1.0f)
                |> positionWidget(0.0f, 0.5f, 0.0f, 0.6f, 0.0f, 1.0f, 0.0f, 1.0f)
                |> this.Add

                TextBox(K data.Mods, K (Color.White, Color.Black), 1.0f)
                |> positionWidget(0.0f, 0.5f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.6f)
                |> this.Add

                Clickable((fun () -> Screens.newScreen((fun () -> new ScreenScore(data, (PersonalBestType.None, PersonalBestType.None, PersonalBestType.None)) :> Screen), ScreenType.Score, ScreenTransitionFlag.Default)), ignore)
                |> this.Add

                this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 75.0f, 0.0f)

            override this.Draw() =
                Draw.rect this.Bounds (Screens.accentShade(127, 0.8f, 0.0f)) Sprite.Default
                base.Draw()
            member this.Data = data

        type Scoreboard() as this =
            inherit Selectable()

            let flowContainer = new FlowContainer()
            let mutable empty = false
            //todo: store these in options
            let filter = Setting(ScoreboardFilter.All)
            let sort = Setting(ScoreboardSort.Performance)

            let mutable chart = ""
            let mutable scoring = ""
            let ls = new ListSelectable(true)

            do
                flowContainer
                |> positionWidgetA(0.0f, 10.0f, 0.0f, -40.0f)
                |> this.Add

                LittleButton.FromEnum("Sort", sort, this.Refresh)
                |> positionWidget(20.0f, 0.0f, -35.0f, 1.0f, -20.0f, 0.25f, -5.0f, 1.0f)
                |> ls.Add

                LittleButton.FromEnum("Filter", filter, this.Refresh)
                |> positionWidget(20.0f, 0.25f, -35.0f, 1.0f, -20.0f, 0.5f, -5.0f, 1.0f)
                |> ls.Add

                LittleButton((fun () -> scoreSystem), fun () -> options.AccSystems.Apply(WatcherSelection.cycleForward); refresh <- true)
                |> positionWidget(20.0f, 0.5f, -35.0f, 1.0f, -20.0f, 0.75f, -5.0f, 1.0f)
                |> ls.Add

                LittleButton(K "Local Scores", this.Refresh) //nyi
                |> positionWidget(20.0f, 0.75f, -35.0f, 1.0f, -20.0f, 1.0f, -5.0f, 1.0f)
                |> ls.Add

                ls |> this.Add

            override this.OnSelect() =
                base.OnSelect()
                let (left, _, right, _) = this.Anchors
                left.Target <- 0.0f
                right.Target <- 0.0f

            override this.OnDeselect() =
                base.OnSelect()
                let (left, _, right, _) = this.Anchors
                left.Target <- -800.0f
                right.Target <- -800.0f

            member this.Refresh() =
                let h = match currentCachedChart with Some c -> c.Hash | None -> ""
                if h <> chart || (match chartSaveData with None -> false | Some d -> d.Scores.Count <> flowContainer.Children.Count) then
                    chart <- h
                    flowContainer.Clear()
                    match chartSaveData with
                    | None -> ()
                    | Some d ->
                        for score in d.Scores do
                            ScoreInfoProvider(score, currentChart.Value, fst options.AccSystems.Value, fst options.HPSystems.Value)
                            |> ScoreboardItem
                            |> flowContainer.Add
                    empty <- flowContainer.Children.Count = 0
                if scoring <> scoreSystem then
                    let s = fst options.AccSystems.Value
                    for c in flowContainer.Children do (c :?> ScoreboardItem).Data.AccuracyType <- s
                    scoring <- scoreSystem

                flowContainer.Sort(
                    match sort.Value with
                    | ScoreboardSort.Accuracy -> Comparison(fun b a -> (a :?> ScoreboardItem).Data.Accuracy.Value.CompareTo((b :?> ScoreboardItem).Data.Accuracy.Value))
                    | ScoreboardSort.Performance -> Comparison(fun b a -> (a :?> ScoreboardItem).Data.Physical.CompareTo((b :?> ScoreboardItem).Data.Physical))
                    | ScoreboardSort.Time
                    | _ -> Comparison(fun b a -> (a :?> ScoreboardItem).Data.Score.time.CompareTo((b :?> ScoreboardItem).Data.Score.time))
                    )
                flowContainer.Filter(
                    match filter.Value with
                    | ScoreboardFilter.CurrentRate -> (fun a -> (a :?> ScoreboardItem).Data.Score.rate = rate)
                    | ScoreboardFilter.CurrentPlaystyle -> (fun a -> (a :?> ScoreboardItem).Data.Score.layout = options.Playstyles.[(a :?> ScoreboardItem).Data.Score.keycount - 3])
                    | ScoreboardFilter.CurrentMods// -> (fun a -> (a :?> ScoreboardItem).Data.Score.selectedMods <> null) //nyi
                    | _ -> K true
                    )

            override this.Update(elapsedTime, bounds) =
                base.Update(elapsedTime, bounds)
                if this.Selected && (ls.Selected <> options.Hotkeys.Scoreboard.Value.Pressed()) then ls.Selected <- not ls.Selected

        type ModSelectItem(name: string) as this =
            inherit Selectable()

            do
                TextBox(ModState.getModName name |> K, K (Color.White, Color.Black), 0.0f)
                |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.6f)
                |> this.Add

                TextBox(ModState.getModDesc name |> K, K (Color.White, Color.Black), 0.0f)
                |> positionWidget(0.0f, 0.0f, 0.0f, 0.6f, 0.0f, 1.0f, 0.0f, 1.0f)
                |> this.Add

                Clickable(
                    (fun () -> if this.SParent.Value.Selected then this.Selected <- true),
                    (fun b -> if b && this.SParent.Value.Selected then this.Hover <- true))
                |> this.Add

            override this.Draw() =
                let hi = Screens.accentShade(255, 1.0f, 0.0f)
                let lo = Color.FromArgb(100, hi)
                let e = selectedMods.ContainsKey(name)
                Draw.quad (Quad.ofRect this.Bounds)
                    (struct((if this.Hover then hi else lo), (if e then hi else lo), (if e then hi else lo), if this.Hover then hi else lo))
                    Sprite.DefaultQuad
                base.Draw()

            override this.OnSelect() =
                base.OnSelect()
                selectedMods <- ModState.cycleState name selectedMods
                updateChart()
                this.Selected <- false

        type ModSelect() as this =
            inherit ListSelectable(false)
            do
                let mutable i = 0.0f
                for k in modList.Keys do
                    this.Add(ModSelectItem(k) |> positionWidget(0.0f, 0.0f, i * 80.0f, 0.0f, 0.0f, 1.0f, 75.0f + i * 80.0f, 0.0f))
                    i <- i + 1.0f

            override this.OnSelect() =
                base.OnSelect()
                let (left, _, right, _) = this.Anchors
                left.Target <- 0.0f
                right.Target <- 0.0f

            override this.OnDeselect() =
                base.OnSelect()
                let (left, _, right, _) = this.Anchors
                left.Target <- -800.0f
                right.Target <- -800.0f

    type InfoPanel() as this =
        inherit Selectable()

        let mods = ModSelect()
        let scores = Scoreboard()
        let mutable length = ""
        let mutable bpm = ""

        do
            mods
            |> positionWidgetA(-800.0f, 0.0f, -800.0f, -200.0f)
            |> this.Add

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

        override this.Update(elapsedTime, bounds) =
            if options.Hotkeys.Mods.Value.Tapped() then
                mods.Selected <- true
            elif options.Hotkeys.Scoreboard.Value.Tapped() then
                scores.Selected <- true
            base.Update(elapsedTime, bounds)

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
                | None -> (120.0f<ms/beat>, 120.0f<ms/beat>)
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
            let bounds = this.Bounds(top)
            let struct (_, _, _, bottom) = bounds
            if bottom > topEdge + 170.0f && top < Render.vheight - topEdge then this.OnDraw(bounds, this.Selected)
            top + Rect.height bounds + 15.0f

        abstract member Update: float32 * float32 * float -> float32
        default this.Update(top: float32, topEdge: float32, elapsedTime) =
            this.Navigate()
            let bounds = this.Bounds(top)
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

        override this.Bounds(top) = Rect.create (Render.vwidth * 0.4f) top Render.vwidth (top + 90.0f)
        override this.Selected = selectedChart = cc.FilePath

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
            let accent = Screens.accentShade(80 + int (hover.Value * 40.0f), 1.0f, 0.2f)
            Draw.rect bounds (Screens.accentShade(80, 1.0f, 0.0f)) Sprite.Default
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

            Draw.rect(Rect.sliceBottom 25.0f bounds) (Screens.accentShade(70, 0.3f, 0.0f)) Sprite.Default
            Text.drawB(font(), cc.Title, 23.0f, left, top, (Color.White, Color.Black))
            Text.drawB(font(), cc.Artist + "  •  " + cc.Creator, 18.0f, left, top + 34.0f, (Color.White, Color.Black))
            Text.drawB(font(), cc.DiffName, 15.0f, left, top + 65.0f, (Color.White, Color.Black))

            let border = Rect.expand(5.0f, 5.0f) bounds
            let border2 = Rect.expand(5.0f, 0.0f) bounds
            let borderColor = if selected then Screens.accentShade(180, 1.0f, 0.5f) else color
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
                | Some d -> pbData <- (f scoreSystem d.Accuracy |> Option.map (PersonalBests.map (fun x -> x, grade x themeConfig.GradeThresholds)), f scoreSystem d.Lamp, f (scoreSystem + "|" + hpSystem) d.Clear)
                | None -> ()
                color <- colorFunc pbData
            if Mouse.Hover(bounds) then
                hover.Target <- 1.0f
                if Mouse.Click(MouseButton.Left) then
                    if selected then playCurrentChart()
                    else switchCurrentChart(cc, groupName)
                elif Mouse.Click(MouseButton.Right) then
                    expandedGroup <- ""
                    scrollTo <- ScrollToPack groupName
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
            Draw.rect bounds (if selected then Screens.accentShade(127, 1.0f, 0.2f) else Screens.accentShade(127, 0.5f, 0.0f)) Sprite.Default
            Text.drawFillB(font(), name, bounds, (Color.White, Color.Black), 0.5f)
        override this.Draw(top, topEdge) =
            let b = base.Draw(top, topEdge)
            if this.Expanded then
                let b2 = List.fold (fun t (i: LevelSelectChartItem) -> i.Draw(t, topEdge)) b items
                if b < topEdge + 170.0f && b2 > topEdge + 170.0f then Text.drawJustB(font(), name, 15.0f, Render.vwidth, topEdge + 180.0f, (Color.White, Color.Black), 1.0f)
                b2
            else b

        override this.OnUpdate(bounds, selected, elapsedTime) =
            if Mouse.Hover(bounds) && Mouse.Click(MouseButton.Left) then
                if this.Expanded then expandedGroup <- "" else (expandedGroup <- name; scrollTo <- ScrollToPack name)
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
open ScreenLevelSelectVars

type ScreenLevelSelect() as this =
    inherit Screen()

    let mutable scrolling = false
    let mutable folderList: LevelSelectPackItem list = []
    let mutable lastItem: (string * CachedChart) option = None
    let mutable filter: Filter = []
    let scrollPos = new AnimationFade(300.0f)
    let searchText = new Setting<string>("")
    let infoPanel = new InfoPanel()

    let refresh() =
        scoreSystem <- (fst options.AccSystems.Value).ToString()
        infoPanel.Refresh()
        let groups = cache.GetGroups groupBy.[options.ChartGroupMode.Value] sortBy.[options.ChartSortMode.Value] filter
        if groups.Count = 1 then
            let g = groups.Keys.First()
            if groups.[g].Count = 1 then
                let cc = groups.[g].[0]
                if cc.FilePath <> selectedChart then
                    match cache.LoadChart(cc) with
                    | Some c -> changeChart(cc, c)
                    | None -> Logging.Error("Couldn't load cached file: " + cc.FilePath) ""
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
        options.ChartSortMode.Apply(fun s -> if sortBy.ContainsKey s then s else "Title")
        options.ChartGroupMode.Apply(fun s -> if groupBy.ContainsKey s then s else "Pack")
        this.Animation.Add scrollPos
        scrollBy <- fun amt -> scrollPos.Target <- scrollPos.Target + amt

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
                navigation <- Navigation.Forward(selectedGroup = g && selectedChart = c.FilePath)
        elif options.Hotkeys.Previous.Value.Tapped() then
            if lastItem.IsSome then navigation <- Navigation.Backward(lastItem.Value)

        let struct (left, top, right, bottom) = this.Bounds
        let bottomEdge =
            folderList
            |> List.fold (fun t (i: LevelSelectPackItem) -> i.Update(t, top, elapsedTime)) scrollPos.Value
        let height = bottomEdge - scrollPos.Value - 320.0f
        if Mouse.Click(MouseButton.Right) then scrolling <- true
        if Mouse.Held(MouseButton.Right) |> not then scrolling <- false
        if scrolling then scrollPos.Target <- -(Mouse.Y() - (top + 250.0f))/(bottom - top - 250.0f) * height
        scrollPos.Target <- Math.Min(Math.Max(scrollPos.Target + Mouse.Scroll() * 100.0f, -height + 600.0f), 300.0f)

    override this.Draw() =
        let struct (left, top, right, bottom) = this.Bounds
        //level select stuff
        Stencil.create(false)
        Draw.rect(Rect.create 0.0f (top + 170.0f) Render.vwidth bottom) Color.Transparent Sprite.Default
        Stencil.draw()
        let bottomEdge = folderList |> List.fold (fun t (i: LevelSelectPackItem) -> i.Draw(t, top)) scrollPos.Value
        Stencil.finish()
        //todo: make this render right, is currently bugged
        let scrollPos = (scrollPos.Value / (scrollPos.Value - bottomEdge)) * (bottom - top - 100.0f)
        Draw.rect(Rect.create (Render.vwidth - 10.0f) (top + 225.0f + scrollPos) (Render.vwidth - 5.0f) (top + 245.0f + scrollPos)) Color.White Sprite.Default

        Draw.rect(Rect.create left top right (top + 170.0f)) (Screens.accentShade(100, 0.6f, 0.0f)) Sprite.Default
        Draw.rect(Rect.create left (top + 170.0f) right (top + 175.0f)) (Screens.accentShade(255, 0.8f, 0.0f)) Sprite.Default
        base.Draw()

    override this.OnEnter(prev) =
        base.OnEnter(prev)
        refresh()

    override this.OnExit(next) =
        base.OnExit(next)
        Input.removeInputMethod()