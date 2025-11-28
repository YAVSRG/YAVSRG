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
        | HudElement.MultiplayerScoreTracker -> ctx.Inner.IsMultiplayer || ctx.Inner.IsSpectate

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
        | HudElement.MultiplayerScoreTracker -> ctx.Config.MultiplayerScoreTrackerPosition

    [<Extension>]
    static member private Constructor (ctx: HudContext, element: HudElement) : HudContext -> Widget =
        let inline cast (f: ^T -> ^U) = fun x -> f x :> Widget
        match element with
        | HudElement.Accuracy -> cast Accuracy
        | HudElement.ErrorBar -> match ctx.Inner with HudContextInner.Replay (_, overlay_shown) -> cast (fun x -> ErrorBar(x).Conditional(not << overlay_shown)) | _ -> cast ErrorBar
        | HudElement.ColumnErrorBars -> match ctx.Inner with HudContextInner.Replay (_, overlay_shown) -> cast (fun x -> ColumnErrorBars(x).Conditional(not << overlay_shown)) | _ -> cast ColumnErrorBars
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
        | HudElement.MultiplayerScoreTracker -> cast MultiplayerScoreTracker

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

    [<Extension>]
    static member Init (ctx: HudContext) =
        match ctx.Inner with
        | HudContextInner.Play -> HudElement.DRAW_ORDER
        | HudContextInner.Practice -> HudElement.DRAW_ORDER_WITHOUT_SKIP
        | HudContextInner.Replay (true, _) -> HudElement.DRAW_ORDER_WITHOUT_SKIP |> Array.except HudElement.HIDDEN_DURING_AUTO
        | HudContextInner.Replay (false, _) -> HudElement.DRAW_ORDER_WITHOUT_SKIP
        | HudContextInner.Spectate _ -> HudElement.DRAW_ORDER_WITHOUT_SKIP |> Array.except [| HudElement.SkipButton |]
        | HudContextInner.Multiplayer _ -> HudElement.DRAW_ORDER_WITHOUT_SKIP |> Array.except [| HudElement.SkipButton |]
        | HudContextInner.Editor -> [||] // Editor adds elements itself, with positioning controls
        |> Array.iter ctx.TryAdd

[<AbstractClass>]
type IPlayScreen(info: LoadedChartInfo, pacemaker_info: PacemakerState, scoring: ScoreProcessor, hud_ctx_inner: HudContextInner) as this
    =
    inherit Screen()

    let state: PlayState = PlayState(info, pacemaker_info, scoring, Song.time_with_offset)

    let noteskin_config = Content.NoteskinConfig

    let playfield =
        Playfield(info.WithColors, state, noteskin_config, options.VanishingNotes.Value)

    let hud_ctx : HudContext = { Screen = this; Playfield = playfield; State = state; Config = Content.HUD; Inner = hud_ctx_inner }

    do
        this.Add playfield
        playfield.Add(LanecoverOverReceptors())
        hud_ctx.Init()

    member this.Playfield = playfield
    member this.State = state
    member this.HudContext = hud_ctx

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