namespace Interlude.Features.Play

open System.Runtime.CompilerServices
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

[<Extension>]
type HudContextExtensions =

    [<Extension>]
    static member private IsEnabled (ctx: HudContext, element: HudElement) : bool =
        match element with
        | HudElement.Accuracy -> ctx.Config.AccuracyEnabled
        | HudElement.ErrorBar -> ctx.Config.TimingDisplayEnabled
        | HudElement.ColumnErrorBars -> ctx.Config.ColumnErrorBarsEnabled
        | HudElement.Combo -> ctx.Config.ComboEnabled
        | HudElement.SkipButton -> true
        | HudElement.Judgement -> ctx.Config.JudgementMeterEnabled
        | HudElement.EarlyLate -> ctx.Config.EarlyLateMeterEnabled
        | HudElement.ProgressPie -> ctx.Config.ProgressMeterEnabled
        | HudElement.JudgementCounter -> ctx.Config.JudgementCounterEnabled
        | HudElement.RateMods -> ctx.Config.RateModMeterEnabled
        | HudElement.BPM -> ctx.Config.BPMMeterEnabled
        | HudElement.InputMeter -> ctx.Config.InputMeterEnabled
        | HudElement.Pacemaker -> not ctx.State.Pacemaker.IsNone
        | HudElement.KeysPerSecond -> ctx.Config.KeysPerSecondMeterEnabled
        | HudElement.CustomImage -> ctx.Config.CustomImageEnabled

    [<Extension>]
    static member private Position (ctx: HudContext, element: HudElement) : HudPosition =
        match element with
        | HudElement.Accuracy -> ctx.Config.AccuracyPosition
        | HudElement.ErrorBar -> ctx.Config.TimingDisplayPosition
        | HudElement.ColumnErrorBars -> ctx.Config.ColumnErrorBarsPosition
        | HudElement.Combo -> ctx.Config.ComboPosition
        | HudElement.SkipButton -> ctx.Config.SkipButtonPosition
        | HudElement.Judgement -> ctx.Config.JudgementMeterPosition
        | HudElement.EarlyLate -> ctx.Config.EarlyLateMeterPosition
        | HudElement.ProgressPie -> ctx.Config.ProgressMeterPosition
        | HudElement.JudgementCounter -> ctx.Config.JudgementCounterPosition
        | HudElement.RateMods -> ctx.Config.RateModMeterPosition
        | HudElement.BPM -> ctx.Config.BPMMeterPosition
        | HudElement.InputMeter -> ctx.Config.InputMeterPosition
        | HudElement.Pacemaker -> ctx.Config.PacemakerPosition
        | HudElement.KeysPerSecond -> ctx.Config.KeysPerSecondMeterPosition
        | HudElement.CustomImage -> ctx.Config.CustomImagePosition

    [<Extension>]
    static member private Constructor (ctx: HudContext, element: HudElement) : HudContext -> Widget =
        let inline cast (f: ^T -> ^U) = fun x -> f x :> Widget
        match element with
        | HudElement.Accuracy -> cast Accuracy
        | HudElement.ErrorBar -> cast ErrorBar // todo: in replay mode, overlay should make this element conditionally visible
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
    [<Extension>]
    static member TryAdd (ctx: HudContext, element: HudElement) =
        if ctx.IsEnabled(element) then
            let position = ctx.Position(element)

            let w = ctx.Constructor element ctx

            w.Position <-
                {
                    Left = position.Left
                    Top = position.Top
                    Right = position.Right
                    Bottom = position.Bottom
                }

            if position.RelativeToPlayfield then ctx.Playfield.Add w else ctx.Screen.Add w

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