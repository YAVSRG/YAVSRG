namespace Interlude.Features.Skins

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.UI
open Interlude.Content
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.Features.Play

type SkinPreview(position: Position) as this =
    inherit Container(NodeType.None)

    static let mutable instances = []

    let fbo = Render.borrow_fbo ()

    let construct_hud_element (state: PlayState) (elem: HudElement) (playfield: Container) (outside_playfield: Container) =
        if (HudElement.enabled_setting elem).Value then
            let pos = (HudElement.position_setting elem).Value
            let w = HudElement.constructor elem (Content.HUD, state)
            w.Position <-
                {
                    Left = pos.Left
                    Top = pos.Top
                    Right = pos.Right
                    Bottom = pos.Bottom
                }
            if pos.RelativeToPlayfield then
                playfield.Add w
            else
                outside_playfield.Add w

    let create_renderer (info: LoadedChartInfo) =
        let state, recreate_scoring = PlayState.Dummy info
        let noteskin_config = Content.NoteskinConfig
        let playfield =
            Playfield(info.WithColors, state, noteskin_config, false)

        playfield.Add(LanecoverOverReceptors())
        let overlay_items = Container(NodeType.None)

        let elements =
            HudElement.FULL_LIST
            |> Seq.except (seq {
                yield HudElement.SkipButton
                if not options.EnablePacemaker.Value then yield HudElement.Pacemaker
            })
        for elem in elements do
            construct_hud_element state elem playfield overlay_items

        let mutable last_time = -Time.infinity

        playfield.Add({ new StaticWidget(NodeType.None) with
            override this.Draw() = ()
            override this.Update(elapsed_ms, moved) =
                base.Update(elapsed_ms, moved)
                let now = state.CurrentChartTime()
                state.Scoring.Update (now)
                if last_time > now then
                    recreate_scoring()
                last_time <- now
        })

        playfield.Add(OverlayContainer(overlay_items))

        if this.Initialised then
            playfield.Init this

        playfield :> Widget

    let mutable renderer: Widget = Dummy()

    let mutable expand = false

    let bounds_placeholder =
        SlideContainer(NodeType.None)
            .Position(position)

    do
        instances <- this :: instances
        fbo.Unbind()

        SelectedChart.if_loaded <| fun info -> renderer <- create_renderer info

        this
        |* (bounds_placeholder
            |+ Text(Icons.EYE + " " + %"misc.preview")
                .Align(Alignment.LEFT)
                .Position(Position.Shrink(20.0f, 10.0f).SliceT(30.0f)))

    member this.PreviewBounds = bounds_placeholder.Bounds

    member this.Refresh() =
        SelectedChart.recolor ()
        SelectedChart.when_loaded false <| fun info -> renderer <- create_renderer info

    static member RefreshAll() =
        for instance in instances do instance.Refresh()

    override this.Update(elapsed_ms, moved) =
        this.Bounds <- Render.bounds()
        renderer.Update(elapsed_ms, moved)
        base.Update(elapsed_ms, moved)
        if (Mouse.hover bounds_placeholder.Bounds && Mouse.left_clicked()) || (%%"preview").Pressed() then
            expand <- not expand
            bounds_placeholder.Position <- if expand then Position.Expand(0.5f) else position
        elif expand && (%%"exit").Pressed() then
            expand <- false
            bounds_placeholder.Position <- position
        elif expand then
            Input.finish_frame_events()
        bounds_placeholder.Update(elapsed_ms, moved)

    override this.Draw() =
        fbo.Bind true
        let screen_bounds = Render.bounds()
        Background.draw (screen_bounds, Colors.white, 1.0f)
        Render.rect screen_bounds (Color.Black.O4a(options.BackgroundDim.Value * 255.0f |> int))
        renderer.Draw()
        fbo.Unbind()
        Render.rect (bounds_placeholder.Bounds.Translate(10.0f, 10.0f)) Colors.shadow_2.O2
        Render.sprite bounds_placeholder.Bounds Color.White fbo.Sprite
        base.Draw()

    override this.Init(parent: Widget) =
        base.Init parent
        this.Bounds <- Render.bounds()
        renderer.Init this

    interface System.IDisposable with
        member this.Dispose() =
            instances <- instances |> List.except [this]
            (fbo :> System.IDisposable).Dispose()

    static member LEFT_HAND_SIDE (scale: float32) : Position =
        let w = Render.width() * scale
        let h = Render.height() * scale
        {
            Left = 0.0f %+ 50.0f
            Top = 0.5f %- (h * 0.5f)
            Right = 0.0f %+ (50.0f + w)
            Bottom = 0.5f %+ (h * 0.5f)
        }

    static member RIGHT_HAND_SIDE (scale: float32) : Position =
        let w = Render.width() * scale
        let h = Render.height() * scale
        {
            Left = 1.0f %- (50.0f + w)
            Top = 0.5f %- (h * 0.5f)
            Right = 1.0f %- 50.0f
            Bottom = 0.5f %+ (h * 0.5f)
        }