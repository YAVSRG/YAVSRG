namespace Interlude.UI.Screens.LevelSelect

open System
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
open Interlude.UI.Animation
open Interlude.UI.Components

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

        let sorts = sortBy.Keys |> Array.ofSeq
        new Dropdown(sorts, Array.IndexOf(sorts, options.ChartSortMode.Value),
            (fun i -> options.ChartSortMode.Value <- sorts.[i]; refresh()), "Sort", 50.0f, fun () -> Style.accentShade(100, 0.4f, 0.6f))
        |> TooltipRegion.Create (Localisation.localise "levelselect.sortby.tooltip")
        |> positionWidget(0.0f, 0.7f, 120.0f, 0.0f, -25.0f, 0.85f, 400.0f, 0.0f)
        |> this.Add

        let groups = groupBy.Keys |> Array.ofSeq
        new Dropdown(groups, Array.IndexOf(groups, options.ChartGroupMode.Value),
            (fun i -> options.ChartGroupMode.Value <- groups.[i]; refresh()), "Group", 50.0f, fun () -> Style.accentShade(100, 0.2f, 0.8f))
        |> TooltipRegion.Create (Localisation.localise "levelselect.groupby.tooltip")
        |> positionWidget(0.0f, 0.85f, 120.0f, 0.0f, 0.0f, 1.0f, 400.0f, 0.0f)
        |> this.Add

        infoPanel
        |> positionWidget(10.0f, 0.0f, 180.0f, 0.0f, -10.0f, 0.4f, 0.0f, 1.0f)
        |> this.Add

        Chart.onChange.Add infoPanel.Refresh

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if LevelSelect.refresh then refresh(); LevelSelect.refresh <- false

        if options.Hotkeys.Select.Value.Tapped() then Tree.play()

        elif options.Hotkeys.UpRateSmall.Value.Tapped() then changeRate(0.01f)
        elif options.Hotkeys.UpRateHalf.Value.Tapped() then changeRate(0.05f)
        elif options.Hotkeys.UpRate.Value.Tapped() then changeRate(0.1f)
        elif options.Hotkeys.DownRateSmall.Value.Tapped() then changeRate(-0.01f)
        elif options.Hotkeys.DownRateHalf.Value.Tapped() then changeRate(-0.05f)
        elif options.Hotkeys.DownRate.Value.Tapped() then changeRate(-0.1f)

        elif options.Hotkeys.Next.Value.Tapped() then Tree.next()
        elif options.Hotkeys.Previous.Value.Tapped() then Tree.previous()
        
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