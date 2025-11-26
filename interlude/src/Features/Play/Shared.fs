namespace Interlude.Features.Play

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Prelude.Gameplay.Scoring
open Prelude.Skins.HudLayouts
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Pacemaker
open Interlude.Features.Online
open Interlude.Features.Play
open Interlude.Features.Play.HUD

type HudContext =
    {
        Screen: IContainer<Widget>
        Playfield: Playfield
        State: PlayState
        Config: HudConfig
    }

    member private this.IsEnabled(element: HudElement) : bool =
        match element with
        | HudElement.Accuracy -> this.Config.AccuracyEnabled
        | HudElement.ErrorBar -> this.Config.TimingDisplayEnabled
        | HudElement.ColumnErrorBars -> this.Config.ColumnErrorBarsEnabled
        | HudElement.Combo -> this.Config.ComboEnabled
        | HudElement.SkipButton -> true
        | HudElement.Judgement -> this.Config.JudgementMeterEnabled
        | HudElement.EarlyLate -> this.Config.EarlyLateMeterEnabled
        | HudElement.ProgressPie -> this.Config.ProgressMeterEnabled
        | HudElement.JudgementCounter -> this.Config.JudgementCounterEnabled
        | HudElement.RateMods -> this.Config.RateModMeterEnabled
        | HudElement.BPM -> this.Config.BPMMeterEnabled
        | HudElement.InputMeter -> this.Config.InputMeterEnabled
        | HudElement.Pacemaker -> not this.State.Pacemaker.IsNone
        | HudElement.KeysPerSecond -> this.Config.KeysPerSecondMeterEnabled
        | HudElement.CustomImage -> this.Config.CustomImageEnabled

    member private this.Position(element: HudElement) : HudPosition =
        match element with
        | HudElement.Accuracy -> this.Config.AccuracyPosition
        | HudElement.ErrorBar -> this.Config.TimingDisplayPosition
        | HudElement.ColumnErrorBars -> this.Config.ColumnErrorBarsPosition
        | HudElement.Combo -> this.Config.ComboPosition
        | HudElement.SkipButton -> this.Config.SkipButtonPosition
        | HudElement.Judgement -> this.Config.JudgementMeterPosition
        | HudElement.EarlyLate -> this.Config.EarlyLateMeterPosition
        | HudElement.ProgressPie -> this.Config.ProgressMeterPosition
        | HudElement.JudgementCounter -> this.Config.JudgementCounterPosition
        | HudElement.RateMods -> this.Config.RateModMeterPosition
        | HudElement.BPM -> this.Config.BPMMeterPosition
        | HudElement.InputMeter -> this.Config.InputMeterPosition
        | HudElement.Pacemaker -> this.Config.PacemakerPosition
        | HudElement.KeysPerSecond -> this.Config.KeysPerSecondMeterPosition
        | HudElement.CustomImage -> this.Config.CustomImagePosition

    member private this.Constructor(element: HudElement) : HudConfig * PlayState -> Widget =
        let inline cast (f: ^T -> ^U) = fun x -> f x :> Widget
        match element with
        | HudElement.Accuracy -> cast Accuracy
        | HudElement.ErrorBar -> cast ErrorBar
        | HudElement.ColumnErrorBars -> cast ColumnErrorBars
        | HudElement.Combo -> cast Combo
        | HudElement.SkipButton -> cast SkipButton
        | HudElement.Judgement -> cast Judgement
        | HudElement.EarlyLate -> cast EarlyLate
        | HudElement.ProgressPie -> cast ProgressPie
        | HudElement.JudgementCounter -> cast JudgementCounter
        | HudElement.RateMods -> cast RateMods
        | HudElement.BPM -> cast BPM
        | HudElement.InputMeter -> cast InputMeter
        | HudElement.Pacemaker -> cast Pacemaker
        | HudElement.KeysPerSecond -> cast KeysPerSecond
        | HudElement.CustomImage -> cast CustomImage

    /// Adds the element in its configured position, if enabled
    member this.TryAdd(element: HudElement) =
        if this.IsEnabled(element) then
            let position = this.Position(element)

            let w = this.Constructor(element)(this.Config, this.State)

            w.Position <-
                {
                    Left = position.Left
                    Top = position.Top
                    Right = position.Right
                    Bottom = position.Bottom
                }

            if position.RelativeToPlayfield then this.Playfield.Add w else this.Screen.Add w

[<AbstractClass>]
type IPlayScreen(info: LoadedChartInfo, pacemaker_info: PacemakerState, scoring: ScoreProcessor) as this
    =
    inherit Screen()

    let state: PlayState = PlayState(info, pacemaker_info, scoring, Song.time_with_offset)

    let noteskin_config = Content.NoteskinConfig

    let playfield =
        Playfield(info.WithColors, state, noteskin_config, options.VanishingNotes.Value)

    let hud_ctx : HudContext = { Screen = this; Playfield = playfield; State = state; Config = Content.HUD }

    do
        this.Add playfield

        playfield.Add(LanecoverOverReceptors())

        this.AddWidgets hud_ctx

    abstract member AddWidgets: HudContext -> unit

    member this.Playfield = playfield
    member this.State = state

    override this.OnEnter(prev) =
        Dialog.close ()
        Background.dim (float32 options.BackgroundDim.Value)
        Toolbar.hide ()
        Song.change_rate SelectedChart.rate.Value
        Song.set_global_offset options.AudioOffset.Value
        Song.on_finish <- SongFinishAction.Wait
        Song.play_leadin info.WithColors.FirstNote
        Input.remove_listener ()
        Input.finish_frame_events ()
        WindowThread.defer WindowThread.disable_windows_key

    override this.OnExit next =
        Background.dim 0.7f

        if next <> ScreenType.Score then
            Toolbar.show ()

        if next <> ScreenType.Play then
            WindowThread.defer WindowThread.enable_windows_key

    override this.OnBack() =
        if Network.lobby.IsSome then
            Some ScreenType.Lobby
        else
            Some ScreenType.LevelSelect