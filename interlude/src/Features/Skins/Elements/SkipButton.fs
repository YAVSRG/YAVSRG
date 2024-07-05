namespace Interlude.Features.Play.HUD

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.UI
open Interlude.Content
open Interlude.Features.Play

type SkipButton(config: HudConfig, state: PlayState) =
    inherit Container(NodeType.None)

    let text = [ (%%"skip").ToString() ] %> "play.skiphint"
    let mutable active = true

    let first_note = state.WithColors.FirstNote

    override this.Init(parent) =
        let background = config.SkipButtonBackground
        if background.Enable then
            let lo = (1.0f - background.Scale) * 0.5f
            let hi = 1.0f - lo
            this 
            |* Image(
                Content.Texture "skip-button-bg",
                StretchToFill = false,
                Position = 
                    { 
                        Left = (lo - 0.5f + background.AlignmentX) %+ 0.0f
                        Top = (lo - 0.5f + background.AlignmentY) %+ 0.0f
                        Right = (hi - 0.5f + background.AlignmentX) %+ 0.0f
                        Bottom = (hi - 0.5f + background.AlignmentY) %+ 0.0f
                    }
            )
        this
        |* Text(text, Color = K Colors.text, Align = Alignment.CENTER)
        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if active && Screen.current_type <> Screen.Type.Practice then // hack for HUD editor

            if state.CurrentChartTime() < -Song.LEADIN_TIME * 2.5f then
                if (%%"skip").Tapped() then
                    Song.pause ()
                    Song.play_from (first_note - Song.LEADIN_TIME)
                    active <- false
            else
                active <- false

    override this.Draw() =
        if active then
            base.Draw()