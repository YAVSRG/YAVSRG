namespace Interlude.Features.Play

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Prelude.Gameplay.Scoring
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Pacemaker
open Interlude.Features.Online
open Interlude.Features.Play

[<AbstractClass>]
type IPlayScreen
    (info: LoadedChartInfo, pacemaker_info: PacemakerState, scoring: ScoreProcessor, hud_ctx_inner: HudContextInner) as this
    =
    inherit Screen()

    let state: PlayState =
        PlayState(info, pacemaker_info, scoring, Song.get_TimeWithOffset)

    let noteskin_config = Content.NoteskinConfig

    let playfield =
        Playfield(info.WithColors, state, noteskin_config, options.VanishingNotes.Value)

    let hud_ctx: HudContext =
        {
            Screen = this
            Playfield = playfield
            State = state
            Config = Content.HUD
            Inner = hud_ctx_inner
        }

    do
        this.Add(playfield)
        playfield.Add(LanecoverOverReceptors())
        hud_ctx.Init()

    member this.Playfield = playfield
    member this.State = state
    member this.HudContext = hud_ctx

    override this.OnEnter(_: ScreenType) : unit =
        Dialog.close()
        Background.dim(float32 options.BackgroundDim.Value)
        Toolbar.hide()
        Song.SetRate(SelectedChart.rate.Value)
        Song.SetGlobalOffset(options.AudioOffset.Value)
        Song.OnFinish(SongFinishAction.Wait)
        Song.PlayLeadin(info.WithColors.FirstNote)
        Input.remove_listener()
        Input.finish_frame_events()
        WindowThread.defer WindowThread.disable_windows_key

    override this.OnExit(next: ScreenType) : unit =
        Background.dim 0.7f

        if next <> ScreenType.Score then
            Toolbar.show()

        if next <> ScreenType.Play then
            WindowThread.defer WindowThread.enable_windows_key

    override this.OnBack() : ScreenType option =
        if Network.lobby.IsSome then Some ScreenType.Lobby else Some ScreenType.LevelSelect
