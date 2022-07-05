namespace Interlude.UI.Screens.LevelSelect

open OpenTK.Mathematics
open Percyqaz.Common
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
                    d.Position
                        {
                            Left = 0.0f %+ 5.0f
                            Top = 0.0f %+ 60.0f
                            Right = 1.0f %- 5.0f
                            Bottom = 0.0f %+ (60.0f + float32 (Seq.length items) * Dropdown.ITEMSIZE)
                        }
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

        TextBox((fun () -> match Chart.cacheInfo with None -> "" | Some c -> c.Title), K (Color.White, Color.Black), 0.5f)
            .Position { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 0.0f; Right = 0.4f %+ 0.0f; Bottom = 0.0f %+ 100.0f }
        |> this.Add

        TextBox((fun () -> match Chart.cacheInfo with None -> "" | Some c -> c.DiffName), K (Color.White, Color.Black), 0.5f)
            .Position { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 100.0f; Right = 0.4f %+ 0.0f; Bottom = 0.0f %+ 160.0f }
        |> this.Add

        SearchBox(searchText, fun f -> Tree.filter <- f; refresh())
            .Tooltip(L"levelselect.search.tooltip")
            .Position { Left = 1.0f %- 600.0f; Top = 0.0f %+ 30.0f; Right = 1.0f %- 50.0f; Bottom = 0.0f %+ 90.0f }
        |> this.Add

        ModSelect()
            .Tooltip(L"levelselect.mods.tooltip")
            .Position { Left = 0.4f %+ 25.0f; Top = 0.0f %+ 120.0f; Right = 0.55f %- 25.0f; Bottom = 0.0f %+ 170.0f }
        |> this.Add

        CollectionManager()
            .Tooltip(L"levelselect.collections.tooltip")
            .Position { Left = 0.55f %+ 0.0f; Top = 0.0f %+ 120.0f; Right = 0.7f %- 25.0f; Bottom = 0.0f %+ 170.0f }
        |> this.Add

        StylishButton(
            (fun () -> Setting.app not options.ChartSortReverse; LevelSelect.refresh <- true),
            (fun () -> if options.ChartSortReverse.Value then Icons.order_descending else Icons.order_ascending),
            (fun () -> Style.accentShade(150, 0.4f, 0.6f))
        ).Position { Left = 0.7f %+ 0.0f; Top = 0.0f %+ 120.0f; Right = 0.7f %+ 35.0f; Bottom = 0.0f %+ 170.0f }
        |> this.Add

        LevelSelectDropdown(sortBy.Keys, "Sort",
            options.ChartSortMode |> Setting.trigger (fun _ -> refresh()),
            (fun () -> Style.accentShade(100, 0.4f, 0.6f)),
            Hotkey.SortMode)
            .Tooltip(L"levelselect.sortby.tooltip")
            .Position { Left = 0.7f %+ 60.0f; Top = 0.0f %+ 120.0f; Right = 0.85f %- 25.0f; Bottom = 0.0f %+ 170.0f }
        |> this.Add
        
        StylishButton(
            (fun () -> Setting.app not options.ChartGroupReverse; LevelSelect.refresh <- true),
            (fun () -> if options.ChartGroupReverse.Value then Icons.order_descending else Icons.order_ascending),
            (fun () -> Style.accentShade(150, 0.2f, 0.8f)))
            .Position { Left = 0.85f %+ 0.0f; Top = 0.0f %+ 120.0f; Right = 0.85f %+ 35.0f; Bottom = 0.0f %+ 170.0f }
        |> this.Add

        LevelSelectDropdown(groupBy.Keys, "Group",
            options.ChartGroupMode |> Setting.trigger (fun _ -> refresh()),
            (fun () -> Style.accentShade(100, 0.2f, 0.8f)),
            Hotkey.GroupMode)
            .Tooltip(L"levelselect.groupby.tooltip")
            .Position { Left = 0.85f %+ 60.0f; Top = 0.0f %+ 120.0f; Right = 1.0f %+ 0.0f; Bottom = 0.0f %+ 170.0f }
        |> this.Add

        infoPanel.Position { Left = 0.0f %+ 10.0f; Top = 0.0f %+ 180.0f; Right = 0.4f %- 10.0f; Bottom = 1.0f %+ 0.0f }
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
        
        Tree.update(this.Bounds.Top + 170.0f, this.Bounds.Bottom, elapsedTime)

    override this.Draw() =

        Tree.draw(this.Bounds.Top + 170.0f, this.Bounds.Bottom)

        let w = this.Bounds.Width * 0.4f
        let { Rect.Left = left; Top = top; Right = right } = this.Bounds
        Draw.quad
            ( Quad.create <| Vector2(left, top) <| Vector2(left + w + 85.0f, top) <| Vector2(left + w, top + 170.0f) <| Vector2(left, top + 170.0f) )
            (Quad.colorOf (Style.accentShade (120, 0.6f, 0.0f))) Sprite.DefaultQuad
        Draw.quad
            ( Quad.create <| Vector2(left + w + 85.0f, top) <| Vector2(right, top) <| Vector2(right, top + 170.0f) <| Vector2(left + w, top + 170.0f) )
            (Quad.colorOf (Style.accentShade (120, 0.1f, 0.0f))) Sprite.DefaultQuad
        Draw.rect ( this.Bounds.SliceTop(175.0f).SliceBottom(5.0f) ) (Style.accentShade (255, 0.8f, 0.0f)) Sprite.Default
        base.Draw()

    override this.OnEnter prev =
        Audio.trackFinishBehaviour <- Audio.TrackFinishBehaviour.Action (fun () -> Audio.playFrom Chart.current.Value.Header.PreviewTime)
        refresh()

    override this.OnExit next = Input.removeInputMethod()