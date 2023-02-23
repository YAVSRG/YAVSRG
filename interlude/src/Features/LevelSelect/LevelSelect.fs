namespace Interlude.Features.LevelSelect

open OpenTK.Mathematics
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Prelude.Data.Charts.Sorting
open Prelude.Data.Charts.Caching
open Interlude.Options
open Interlude.Utils
open Interlude.Features.Gameplay
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu
open Interlude.Features.Online

type LevelSelectScreen() =
    inherit Screen()

    let searchText = Setting.simple ""
    let infoPanel = ChartInfo(Position = { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 175.0f; Right = 0.4f %- 10.0f; Bottom = 1.0f %+ 0.0f })

    let refresh() =
        infoPanel.Refresh()
        Tree.refresh()

    let changeRate v = 
        rate.Value <- rate.Value + v
        Tree.updateDisplay()
        infoPanel.Refresh()

    override this.Init(parent: Widget) =
        base.Init parent

        Setting.app (fun s -> if sortBy.ContainsKey s then s else "title") options.ChartSortMode
        Setting.app (fun s -> if groupBy.ContainsKey s then s else "pack") options.ChartGroupMode

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
            .Tooltip(L"levelselect.search.tooltip", "search")
            .WithPosition { Left = 1.0f %- 600.0f; Top = 0.0f %+ 30.0f; Right = 1.0f %- 50.0f; Bottom = 0.0f %+ 90.0f }

        |+ LibraryModeSettings()

        |* infoPanel

        Chart.onChange.Add infoPanel.Refresh
        Comments.init this

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        if LevelSelect.refresh then refresh(); LevelSelect.refresh <- false

        Comments.update(elapsedTime, moved)

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
        Comments.draw()

    override this.OnEnter prev =
        Song.onFinish <- SongFinishAction.Callback (fun () -> Song.playFrom Chart.current.Value.Header.PreviewTime)
        refresh()

    override this.OnExit next = Input.removeInputMethod()

    override this.OnBack() = 
        if Network.lobby.IsSome then Some Screen.Type.Lobby
        else Some Screen.Type.MainMenu