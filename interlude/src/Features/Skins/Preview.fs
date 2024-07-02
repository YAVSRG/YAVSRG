namespace Interlude.Features.Skins

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skinning.HudLayouts
open Interlude.UI
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.Features.Play
open Interlude.Features.HUD.Edit

type NoteskinPreview(position: Position) as this =
    inherit Container(NodeType.None)

    static let mutable instances = []

    let fbo = FBO.create ()

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
        let state = PlayState.Dummy info
        let noteskin_config = Content.NoteskinConfig
        let playfield =
            Playfield(info.WithColors, state, noteskin_config, false)
        
        if noteskin_config.EnableColumnLight then
            playfield.Add(new ColumnLighting(info.WithColors.Keys, noteskin_config, state))

        if noteskin_config.UseExplosions then
            playfield.Add(new Explosions(info.WithColors.Keys, noteskin_config, state))

        playfield.Add(LanecoverOverReceptors())

        for elem in HudElement.FULL_LIST do
            construct_hud_element state elem playfield this

        playfield.Add({ new StaticWidget(NodeType.None) with
            override this.Draw() = ()
            override this.Update(elapsed_ms, moved) =
                base.Update(elapsed_ms, moved)
                state.Scoring.Update (state.CurrentChartTime())
        })

        if this.Initialised then
            playfield.Init this

        playfield :> Widget

    let mutable renderer: Widget = Dummy()

    let mutable expand = false

    let bounds_placeholder =
        SlideContainer(
            NodeType.None,
            Position = position
        )

    do
        instances <- this :: instances
        fbo.Unbind()

        SelectedChart.if_loaded <| fun info -> renderer <- create_renderer info

        this
        |* (bounds_placeholder
            |+ Text(
                Icons.EYE + " " + %"misc.preview",
                Align = Alignment.LEFT,
                Position = Position.Margin(20.0f, 10.0f).SliceTop(30.0f)
            ))

    member this.PreviewBounds = bounds_placeholder.Bounds

    member this.Refresh() =
        SelectedChart.recolor ()
        SelectedChart.when_loaded <| fun info -> renderer <- create_renderer info

    static member RefreshAll() =
        for instance in instances do instance.Refresh()

    override this.Update(elapsed_ms, moved) =
        this.Bounds <- Viewport.bounds
        renderer.Update(elapsed_ms, moved)
        base.Update(elapsed_ms, moved)
        if (Mouse.hover bounds_placeholder.Bounds && Mouse.left_click()) || (%%"preview").Tapped() then
            expand <- not expand
            bounds_placeholder.Position <- if expand then Position.Default else position
        elif expand && (%%"exit").Tapped() then
            expand <- false
            bounds_placeholder.Position <- position
        elif expand then
            Input.finish_frame_events()
        bounds_placeholder.Update(elapsed_ms, moved)

    override this.Draw() =
        fbo.Bind true
        Background.draw (Viewport.bounds, Colors.white, 1.0f)
        Draw.rect Viewport.bounds (Color.Black.O4a(Interlude.Options.options.BackgroundDim.Value * 255.0f |> int))
        renderer.Draw()
        fbo.Unbind()
        Draw.rect (bounds_placeholder.Bounds.Translate(10.0f, 10.0f)) Colors.shadow_2.O2
        Draw.sprite bounds_placeholder.Bounds Color.White fbo.sprite
        base.Draw()

    override this.Init(parent: Widget) =
        base.Init parent
        this.Bounds <- Viewport.bounds
        renderer.Init this

    member this.Destroy() = 
        instances <- instances |> List.except [this]
        fbo.Dispose()

    static member LEFT_HAND_SIDE (scale: float32) : Position =
        let w = Viewport.vwidth * scale
        let h = Viewport.vheight * scale
        {
            Left = 0.0f %+ 50.0f
            Top = 0.5f %- (h * 0.5f)
            Right = 0.0f %+ (50.0f + w)
            Bottom = 0.5f %+ (h * 0.5f)
        }

    static member RIGHT_HAND_SIDE (scale: float32) : Position =
        let w = Viewport.vwidth * scale
        let h = Viewport.vheight * scale
        {
            Left = 1.0f %- (50.0f + w)
            Top = 0.5f %- (h * 0.5f)
            Right = 1.0f %- 50.0f
            Bottom = 0.5f %+ (h * 0.5f)
        }