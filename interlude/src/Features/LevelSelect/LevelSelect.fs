namespace Interlude.Features.LevelSelect

open OpenTK.Mathematics
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Prelude.Scoring
open Prelude.Data.Charts.Sorting
open Prelude.Data.Charts.Caching
open Interlude.Options
open Interlude.Utils
open Interlude.Features.Gameplay
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu

type OrganiseCharts(options: string seq, label: string, setting: Setting<string>, reverse: Setting<bool>, bind: Hotkey) =
    inherit StaticContainer(NodeType.None)

    override this.Init(parent: Widget) =
        this 
        |+ StylishButton(
            ( fun () -> this.ToggleDropdown() ),
            K (label + ":"),
            ( fun () -> Style.color(100, 1.0f, 0.2f) ),
            Hotkey = bind,
            Position = Position.SliceLeft 120.0f)
        |* StylishButton(
            ( fun () -> reverse.Value <- not reverse.Value ),
            ( fun () -> sprintf "%s %s" setting.Value (if reverse.Value then Icons.order_descending else Icons.order_ascending) ),
            ( fun () -> Style.color(100, 0.5f, 0.0f) ),
            // todo: hotkey for this
            Position = Position.TrimLeft 145.0f )
        base.Init parent

    member this.ToggleDropdown() =
        match this.Dropdown with
        | Some d -> this.Dropdown <- None
        | _ ->
            let d = Dropdown.Selector options id (fun g -> setting.Set g) (fun () -> this.Dropdown <- None)
            d.Position <- Position.SliceTop(d.Height + 60.0f).TrimTop(60.0f).Margin(Style.padding, 0.0f)
            d.Init this
            this.Dropdown <- Some d

    member val Dropdown : Dropdown option = None with get, set

    override this.Draw() =
        base.Draw()
        match this.Dropdown with
        | Some d -> d.Draw()
        | None -> ()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        match this.Dropdown with
        | Some d -> d.Update(elapsedTime, moved)
        | None -> ()

type LevelSelectScreen() as this =
    inherit Screen()

    let searchText = Setting.simple ""
    let infoPanel = ChartInfo(Position = { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 175.0f; Right = 0.4f %- 10.0f; Bottom = 1.0f %+ 0.0f })

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

        this
        |+ Text(
            (fun () -> match Chart.cacheInfo with None -> "" | Some c -> c.Title),
            Align = Alignment.CENTER,
            Position = { Left = 0.0f %+ 30.0f; Top = 0.0f %+ 20.0f; Right = 0.4f %- 30.0f; Bottom = 0.0f %+ 100.0f })

        |+ Text(
            (fun () -> match Chart.cacheInfo with None -> "" | Some c -> c.DiffName),
            Align = Alignment.CENTER,
            Position = { Left = 0.0f %+ 30.0f; Top = 0.0f %+ 90.0f; Right = 0.4f %- 30.0f; Bottom = 0.0f %+ 140.0f })

        |+ SearchBox(searchText, (fun f -> Tree.filter <- f; refresh()))
            .Tooltip(L"levelselect.search.tooltip")
            .WithPosition { Left = 1.0f %- 600.0f; Top = 0.0f %+ 30.0f; Right = 1.0f %- 50.0f; Bottom = 0.0f %+ 90.0f }

        |+ CollectionManager()
            .Tooltip(L"levelselect.collections.tooltip")
            .WithPosition { Left = 0.4f %+ 25.0f; Top = 0.0f %+ 120.0f; Right = 0.6f %- 25.0f; Bottom = 0.0f %+ 170.0f }

        |+ OrganiseCharts(sortBy.Keys, "Sort",
            options.ChartSortMode |> Setting.trigger (fun _ -> refresh()),
            options.ChartSortReverse |> Setting.map not not |> Setting.trigger (fun _ -> refresh()),
            "sort_mode")
            .Tooltip(L"levelselect.sortby.tooltip")
            .WithPosition { Left = 0.6f %+ 0.0f; Top = 0.0f %+ 120.0f; Right = 0.8f %- 25.0f; Bottom = 0.0f %+ 170.0f }

        |+ OrganiseCharts(groupBy.Keys, "Group",
            options.ChartGroupMode |> Setting.trigger (fun _ -> refresh()),
            options.ChartGroupReverse |> Setting.trigger (fun _ -> refresh()),
            "group_mode")
            .Tooltip(L"levelselect.groupby.tooltip")
            .WithPosition { Left = 0.8f %+ 0.0f; Top = 0.0f %+ 120.0f; Right = 1.0f %+ 0.0f; Bottom = 0.0f %+ 170.0f }

        |* infoPanel

        Chart.onChange.Add infoPanel.Refresh

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if LevelSelect.refresh then refresh(); LevelSelect.refresh <- false

        if (!|"select").Tapped() then Tree.play()

        elif (!|"uprate_small").Tapped() then changeRate(0.01f)
        elif (!|"uprate_half").Tapped() then changeRate(0.05f)
        elif (!|"uprate").Tapped() then changeRate(0.1f)
        elif (!|"downrate_small").Tapped() then changeRate(-0.01f)
        elif (!|"downrate_half").Tapped() then changeRate(-0.05f)
        elif (!|"downrate").Tapped() then changeRate(-0.1f)

        elif (!|"next").Tapped() then Tree.next()
        elif (!|"previous").Tapped() then Tree.previous()
        elif (!|"next_group").Tapped() then Tree.nextGroup()
        elif (!|"previous_group").Tapped() then Tree.previousGroup()
        elif (!|"start").Tapped() then Tree.beginGroup()
        elif (!|"end").Tapped() then Tree.endGroup()
        
        Tree.update(this.Bounds.Top + 170.0f, this.Bounds.Bottom, elapsedTime)

    override this.Draw() =

        Tree.draw(this.Bounds.Top + 170.0f, this.Bounds.Bottom)

        let w = this.Bounds.Width * 0.4f
        let { Rect.Left = left; Top = top; Right = right } = this.Bounds
        Draw.quad
            ( Quad.create <| Vector2(left, top) <| Vector2(left + w + 85.0f, top) <| Vector2(left + w, top + 170.0f) <| Vector2(left, top + 170.0f) )
            (Quad.colorOf (Style.color (120, 0.6f, 0.0f))) Sprite.DefaultQuad
        Draw.rect (this.Bounds.SliceTop(170.0f).SliceLeft(w).Shrink(20.0f)) (System.Drawing.Color.FromArgb(100, 0, 0, 0))

        Draw.quad
            ( Quad.create <| Vector2(left + w + 85.0f, top) <| Vector2(right, top) <| Vector2(right, top + 170.0f) <| Vector2(left + w, top + 170.0f) )
            (Quad.colorOf (Style.color (120, 0.1f, 0.0f))) Sprite.DefaultQuad
        Draw.rect ( this.Bounds.SliceTop(175.0f).SliceBottom(5.0f) ) (Style.color (255, 0.8f, 0.0f))
        base.Draw()

    override this.OnEnter prev =
        Song.onFinish <- SongFinishAction.Callback (fun () -> Song.playFrom Chart.current.Value.Header.PreviewTime)
        refresh()

    override this.OnExit next = Input.removeInputMethod()