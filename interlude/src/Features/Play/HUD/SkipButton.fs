namespace Interlude.Features.Play.HUD

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Content.Noteskins
open Interlude.UI
open Interlude.Features.Play

type SkipButton(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    let text = [ (%%"skip").ToString() ] %> "play.skiphint"
    let mutable active = true

    let first_note = state.WithColors.FirstNote

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if Screen.current_type <> Screen.Type.Practice then // hack for HUD editor

            if active && state.CurrentChartTime() < -Song.LEADIN_TIME * 2.5f then
                if (%%"skip").Tapped() then
                    Song.pause ()
                    Song.play_from (first_note - Song.LEADIN_TIME)
            else
                active <- false

    override this.Draw() =
        if active then
            Text.fill_b (Style.font, text, this.Bounds, Colors.text, Alignment.CENTER)