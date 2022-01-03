namespace Interlude.UI.Screens.LevelSelect

open System
open System.Drawing
open System.Linq
open OpenTK.Mathematics
open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Prelude.Data.Scores
open Prelude.Data.Charts
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Collections
open Prelude.Scoring
open Interlude.UI
open Interlude.Graphics
open Interlude.Input
open Interlude.Content
open Interlude.Options
open Interlude.Gameplay
open Interlude.UI.Animation
open Interlude.UI.Screens.LevelSelect.Globals

[<AbstractClass>]
type private TreeItem() =
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

type private ChartItem(groupName: string, cc: CachedChart, context: LevelSelectContext) =
    inherit TreeItem()

    let hover = new AnimationFade(0.0f)
    let mutable colorVersion = -1
    let mutable color = Color.Transparent
    let mutable chartData = None
    let mutable pbData: Bests option = None
    let mutable collectionIcon = ""
    let collectionIndex = context.Id

    override this.Bounds(top) = Rect.create (Render.vwidth * 0.4f) top Render.vwidth (top + 90.0f)
    override this.Selected = selectedChart = cc.FilePath && collectionIndex = Collections.contextIndex
    member this.Chart = cc

    override this.Navigate() =
        match navigation with
        | Navigation.Nothing -> ()
        | Navigation.Forward b ->
            if b then
                switchCurrentChart(cc, context, groupName)
                navigation <- Navigation.Nothing
            elif groupName = selectedGroup && this.Selected then navigation <- Navigation.Forward true
        | Navigation.Backward (groupName2, cc2, context2) ->
            if groupName = selectedGroup && this.Selected then
                switchCurrentChart(cc2, context2, groupName2)
                navigation <- Navigation.Nothing
            else navigation <- Navigation.Backward(groupName, cc, context)

    override this.OnDraw(bounds, selected) =
        let struct (left, top, right, bottom) = bounds
        let accent = Style.accentShade(80 + int (hover.Value * 40.0f), 1.0f, 0.2f)
        Draw.rect bounds (if this.Selected then Style.main 80 () else Style.black 80 ()) Sprite.Default
        let stripeLength = (right - left) * (0.4f + 0.6f * hover.Value)
        Draw.quad
            (Quad.create <| new Vector2(left, top) <| new Vector2(left + stripeLength, top) <| new Vector2(left + stripeLength * 0.9f, bottom - 25.0f) <| new Vector2(left, bottom - 25.0f))
            (struct(accent, Color.Transparent, Color.Transparent, accent))
            Sprite.DefaultQuad

        let disp (pb: PersonalBests<'T>) (format: 'T -> string) (colorFunc: 'T -> Color) (pos: float32) =
            let value, rate, color = getPb pb colorFunc
            let formatted = format value
            let rateLabel = sprintf "(%.2fx)" rate
            if color.A > 0uy then
                Draw.rect(Rect.create (right - pos - 40.0f) top (right - pos + 40.0f) bottom) accent Sprite.Default
                Text.drawJustB(font(), formatted, 20.0f, right - pos, top + 8.0f, (color, Color.Black), 0.5f)
                Text.drawJustB(font(), rateLabel, 14.0f, right - pos, top + 35.0f, (color, Color.Black), 0.5f)
        
        match pbData with
        | Some d ->
            disp 
                d.Accuracy
                (fun x -> sprintf "%.2f%%" (100.0 * x))
                (fun _ -> let (_, _, c) = getPb d.Grade Themes.gradeToColor in c)
                450.0f
            disp
                d.Lamp
                (fun x -> x.ToString())
                Themes.lampToColor
                300.0f
            disp
                d.Clear
                (fun x -> if x then "CLEAR" else "FAILED")
                Themes.clearToColor
                150.0f
        | None -> ()

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
            colorVersion <- colorVersionGlobal
            if chartData.IsNone then chartData <- Scores.getScoreData cc.Hash
            match chartData with
            | Some d when d.Bests.ContainsKey scoreSystem ->
                pbData <- Some d.Bests.[scoreSystem]
            | _ -> ()
            color <- colorFunc pbData
            collectionIcon <-
                if options.ChartGroupMode.Value <> "Collections" then
                    match snd Collections.selected with
                    | Collection ccs -> if ccs.Contains cc.FilePath then Interlude.Icons.star else ""
                    | Playlist ps -> if ps.Exists(fun (id, _) -> id = cc.FilePath) then "➾" else ""
                    | Goals gs -> if gs.Exists(fun (id, _) -> id = cc.FilePath) then "@" else ""
                else ""
        if Mouse.Hover bounds then
            hover.Target <- 1.0f
            if Mouse.Click MouseButton.Left then
                if selected then playCurrentChart()
                else switchCurrentChart(cc, context, groupName)
            elif Mouse.Click MouseButton.Right then
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

type private GroupItem(name: string, items: ChartItem list) =
    inherit TreeItem()

    override this.Bounds(top) = Rect.create (Render.vwidth * 0.5f) top (Render.vwidth - 15.0f) (top + 65.0f)
    override this.Selected = selectedGroup = name
    member this.Expanded = expandedGroup = name

    override this.Navigate() = ()

    override this.OnDraw(bounds, selected) =
        let borderb = Rect.expand(5.0f, 5.0f) bounds
        let colorb = if selected then Style.accentShade(200, 1.0f, 0.5f) else Style.accentShade(100, 0.7f, 0.0f)
        Draw.rect (Rect.sliceLeft 5.0f borderb) colorb Sprite.Default
        Draw.rect (Rect.sliceRight 5.0f borderb) colorb Sprite.Default
        Draw.rect (Rect.sliceTop 5.0f borderb) colorb Sprite.Default
        Draw.rect (Rect.sliceBottom 5.0f borderb) colorb Sprite.Default
        Draw.rect bounds (if selected then Style.accentShade(127, 1.0f, 0.2f) else Style.accentShade(127, 0.3f, 0.0f)) Sprite.Default
        Text.drawFillB(font(), name, bounds |> Rect.trimBottom 5.0f, (Color.White, Color.Black), 0.5f)

    override this.Draw(top, topEdge) =
        let b = base.Draw(top, topEdge)
        if this.Expanded then
            let b2 = List.fold (fun t (i: ChartItem) -> i.Draw(t, topEdge)) b items
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
        if this.Expanded then List.fold (fun t (i: ChartItem) -> i.Update(t, topEdge, elapsedTime)) b items
        else List.iter (fun (i: ChartItem) -> i.Navigate()) items; b