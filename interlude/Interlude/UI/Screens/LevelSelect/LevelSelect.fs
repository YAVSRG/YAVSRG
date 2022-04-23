namespace Interlude.UI.Screens.LevelSelect

open OpenTK.Mathematics
open Prelude.Common
open Prelude.Scoring
open Prelude.Data.Charts.Sorting
open Prelude.Data.Charts.Caching
open Interlude
open Interlude.UI
open Interlude.Utils
open Interlude.Graphics
open Interlude.Input
open Interlude.Gameplay
open Interlude.Options
open Interlude.UI.Components
open Interlude.UI.Components.Selection.Controls

type LevelSelectDropdown(items: string seq, label: string, setting: Setting<string>, colorFunc: unit -> Color, bind: Hotkey) as this =
    inherit StylishButton(
            ( fun () -> 
                match this.Dropdown with
                | Some d when d.Parent <> None -> d.Destroy()
                | _ ->
                    let d = Dropdown.create_selector items id (fun g -> setting.Set g) ignore
                    this.Dropdown <- Some d
                    d
                    |> positionWidget(5.0f, 0.0f, 60.0f, 0.0f, -5.0f, 1.0f, 60.0f + float32 (Seq.length items) * Dropdown.ITEMSIZE, 0.0f)
                    |> this.Add
            ),
            ( fun () -> sprintf "%s: %s" label setting.Value ),
            colorFunc,
            bind
        )

    member val Dropdown : Dropdown.Container option = None with get, set

type Screen() as this =
    inherit Screen.T()

    let searchText = Setting.simple ""
    let infoPanel = new ChartInfo()

    let refresh() =
        ruleset <- getCurrentRuleset()
        rulesetId <- Ruleset.hash ruleset
        infoPanel.Refresh()
        Tree.refresh()

    let changeRate v = 
        rate.Value <- rate.Value + v
        Tree.updateDisplay()
        infoPanel.Refresh()

    do
        Setting.app (fun s -> if sortBy.ContainsKey s then s else "Title") options.ChartSortMode
        Setting.app (fun s -> if groupBy.ContainsKey s then s else "Pack") options.ChartGroupMode

        new TextBox((fun () -> match Chart.cacheInfo with None -> "" | Some c -> c.Title), K (Color.White, Color.Black), 0.5f)
        |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.4f, 100.0f, 0.0f)
        |> this.Add

        new TextBox((fun () -> match Chart.cacheInfo with None -> "" | Some c -> c.DiffName), K (Color.White, Color.Black), 0.5f)
        |> positionWidget(0.0f, 0.0f, 100.0f, 0.0f, 0.0f, 0.4f, 160.0f, 0.0f)
        |> this.Add

        new SearchBox(searchText, fun f -> Tree.filter <- f; refresh())
        |> TooltipRegion.Create (Localisation.localise "levelselect.search.tooltip")
        |> positionWidget(-600.0f, 1.0f, 30.0f, 0.0f, -50.0f, 1.0f, 90.0f, 0.0f)
        |> this.Add

        new ModSelect()
        |> TooltipRegion.Create (Localisation.localise "levelselect.mods.tooltip")
        |> positionWidget(25.0f, 0.4f, 120.0f, 0.0f, -25.0f, 0.55f, 170.0f, 0.0f)
        |> this.Add

        new CollectionManager()
        |> TooltipRegion.Create (Localisation.localise "levelselect.collections.tooltip")
        |> positionWidget(0.0f, 0.55f, 120.0f, 0.0f, -25.0f, 0.7f, 170.0f, 0.0f)
        |> this.Add

        StylishButton(
            (fun () -> Setting.app not options.ChartSortReverse; LevelSelect.refresh <- true),
            (fun () -> if options.ChartSortReverse.Value then Icons.order_descending else Icons.order_ascending),
            (fun () -> Style.accentShade(150, 0.4f, 0.6f))
        )
        |> positionWidget(0.0f, 0.7f, 120.0f, 0.0f, 35.0f, 0.7f, 170.0f, 0.0f)
        |> this.Add

        LevelSelectDropdown(sortBy.Keys, "Sort",
            options.ChartSortMode |> Setting.trigger (fun _ -> refresh()),
            (fun () -> Style.accentShade(100, 0.4f, 0.6f)),
            Hotkey.SortMode
        )
        |> TooltipRegion.Create (Localisation.localise "levelselect.sortby.tooltip")
        |> positionWidget(60.0f, 0.7f, 120.0f, 0.0f, -25.0f, 0.85f, 170.0f, 0.0f)
        |> this.Add
        
        StylishButton(
            (fun () -> Setting.app not options.ChartGroupReverse; LevelSelect.refresh <- true),
            (fun () -> if options.ChartGroupReverse.Value then Icons.order_descending else Icons.order_ascending),
            (fun () -> Style.accentShade(150, 0.2f, 0.8f))
        )
        |> positionWidget(0.0f, 0.85f, 120.0f, 0.0f, 35.0f, 0.85f, 170.0f, 0.0f)
        |> this.Add

        LevelSelectDropdown(groupBy.Keys, "Group",
            options.ChartGroupMode |> Setting.trigger (fun _ -> refresh()),
            (fun () -> Style.accentShade(100, 0.2f, 0.8f)),
            Hotkey.GroupMode
        )
        |> TooltipRegion.Create (Localisation.localise "levelselect.groupby.tooltip")
        |> positionWidget(60.0f, 0.85f, 120.0f, 0.0f, 0.0f, 1.0f, 170.0f, 0.0f)
        |> this.Add

        infoPanel
        |> positionWidget(10.0f, 0.0f, 180.0f, 0.0f, -10.0f, 0.4f, 0.0f, 1.0f)
        |> this.Add

        Chart.onChange.Add infoPanel.Refresh

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if LevelSelect.refresh then refresh(); LevelSelect.refresh <- false

        if (!|Hotkey.Select).Tapped() then Tree.play()

        elif (!|Hotkey.UpRateSmall).Tapped() then changeRate(0.01f)
        elif (!|Hotkey.UpRateHalf).Tapped() then changeRate(0.05f)
        elif (!|Hotkey.UpRate).Tapped() then changeRate(0.1f)
        elif (!|Hotkey.DownRateSmall).Tapped() then changeRate(-0.01f)
        elif (!|Hotkey.DownRateHalf).Tapped() then changeRate(-0.05f)
        elif (!|Hotkey.DownRate).Tapped() then changeRate(-0.1f)

        elif (!|Hotkey.Next).Tapped() then Tree.next()
        elif (!|Hotkey.Previous).Tapped() then Tree.previous()
        elif (!|Hotkey.NextGroup).Tapped() then Tree.nextGroup()
        elif (!|Hotkey.PreviousGroup).Tapped() then Tree.previousGroup()
        elif (!|Hotkey.Start).Tapped() then Tree.beginGroup()
        elif (!|Hotkey.End).Tapped() then Tree.endGroup()
        
        let struct (left, top, right, bottom) = this.Bounds
        Tree.update(top + 170.0f, bottom, elapsedTime)

    override this.Draw() =
        let struct (left, top, right, bottom) = this.Bounds

        Tree.draw(top + 170.0f, bottom)

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
        Audio.trackFinishBehaviour <- Audio.TrackFinishBehaviour.Action (fun () -> Audio.playFrom Chart.current.Value.Header.PreviewTime)
        refresh()

    override this.OnExit next = Input.removeInputMethod()