namespace Interlude.Features.Play

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Charts
open Prelude.Data.Scores
open Prelude.Gameplay
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Utils
open Interlude.Features
open Interlude.Features.Gameplay.Chart
open Interlude.Features.Online
open Interlude.Features.Stats
open Interlude.Features.Play.HUD

module PracticeScreen =

    module Sync =

        [<RequireQualifiedAccess>]
        type Mode =
            | HIT_POSITION
            | SCROLL_SPEED
            | VISUAL_OFFSET
            | AUDIO_OFFSET
            member this.Audio =
                match this with
                | VISUAL_OFFSET -> 0
                | HIT_POSITION
                | SCROLL_SPEED -> 1
                | AUDIO_OFFSET -> 2

        let mutable suggested = false
        let mutable mode = Mode.AUDIO_OFFSET
        let mutable mean = 0.0f<ms>

        let update_mean (scoring: ScoreMetric) =
            let mutable sum = 0.0f<ms>
            let mutable count = 1.0f

            for ev in scoring.HitEvents do
                match ev.Guts with
                | Hit x when not x.Missed ->
                    sum <- sum + x.Delta
                    count <- count + 1.0f
                | _ -> ()

            mean <- sum / count * Gameplay.rate.Value
            suggested <- true

        type Panel(content: Widget) =
            inherit StaticContainer(NodeType.Switch(fun () -> content))

            override this.Init(parent) =
                this.Add content
                base.Init parent

            override this.Draw() =
                Draw.rect this.Bounds Colors.shadow_1.O2
                Draw.rect (this.Bounds.Expand(0.0f, Style.PADDING).SliceBottom(Style.PADDING)) Colors.shadow_2.O3
                base.Draw()

        type ModeButton(label: string, m: Mode, apply_suggestion: unit -> unit) =
            inherit
                StaticContainer(
                    NodeType.Button(fun () ->
                        Style.click.Play()
                        if mode = m then apply_suggestion () else mode <- m
                    )
                )

            let requires_audio = m.Audio = 2

            override this.Init(parent) =
                this
                |+ Text(
                    label,
                    Color =
                        fun () ->
                            if requires_audio <> (options.AudioVolume.Value > 0.0) then
                                Colors.text_greyout
                            elif this.Focused then
                                Colors.text_yellow_2
                            else
                                Colors.text
                )
                |* Clickable.Focus this

                base.Init parent

            override this.OnFocus() =
                Style.hover.Play()
                base.OnFocus()

            override this.Draw() =
                if this.Focused then
                    Draw.rect this.Bounds Colors.yellow_accent.O2
                elif mode = m then
                    Draw.rect this.Bounds Colors.pink_accent.O2

                base.Draw()

            override this.Update(elapsed_ms, moved) =
                base.Update(elapsed_ms, moved)

                if this.Focused && not this.Selected && (%%"right").Tapped() then
                    this.Select()
                elif this.Selected && (%%"left").Tapped() then
                    this.Focus()

        type UI(chart: Chart, save_data: ChartSaveData) =
            inherit StaticContainer(NodeType.None)

            let local_audio_offset = 
                LocalAudioSync.offset_setting chart save_data
                |> Setting.bound -200.0f<ms> 200.0f<ms>

            let mutable local_audio_offset_suggestion = local_audio_offset.Value

            let visual_offset = options.VisualOffset |> Setting.roundf 0
            let mutable visual_offset_suggestion = visual_offset.Value

            let scroll_speed = options.ScrollSpeed |> Setting.roundf 2
            let mutable scroll_speed_suggestion = scroll_speed.Value

            let hit_position = options.HitPosition |> Setting.roundf 0
            let mutable hit_position_suggestion = hit_position.Value

            let accept_suggestion_hint =
                [ (%%"accept_suggestion").ToString() ] %> "practice.suggestions.accepthint"

            let about_right_hint = Icons.CHECK + " " + %"practice.suggestions.aboutright"

            let suggestion_text () =
                Text(fun () ->
                    if not suggested then
                        %"practice.suggestions.hint"
                    elif mean < 5.0f<ms> && mean > -5.0f<ms> then
                        about_right_hint
                    else
                        accept_suggestion_hint
                )

            member this.UpdateSuggestions() =
                hit_position_suggestion <- hit_position.Value - mean * options.ScrollSpeed.Value * 1.0f< / ms>

                let expected_pixels = (1080.0f - options.HitPosition.Value) * 0.6f
                let current_lead_time = expected_pixels / (options.ScrollSpeed.Value * 1.0f< / ms>)
                let desired_lead_time = current_lead_time - mean
                scroll_speed_suggestion <- expected_pixels / float32 desired_lead_time

                if options.AudioVolume.Value = 0.0 then
                    visual_offset_suggestion <- visual_offset.Value + mean / 1.0f<ms>
                else
                    local_audio_offset_suggestion <- local_audio_offset.Value - mean

            member this.AcceptSuggestion() =
                match mode with
                | Mode.HIT_POSITION -> hit_position.Set hit_position_suggestion
                | Mode.SCROLL_SPEED -> scroll_speed.Set scroll_speed_suggestion
                | Mode.VISUAL_OFFSET -> visual_offset.Set visual_offset_suggestion
                | Mode.AUDIO_OFFSET -> local_audio_offset.Set local_audio_offset_suggestion

                hit_position_suggestion <- hit_position.Value
                scroll_speed_suggestion <- scroll_speed.Value
                visual_offset_suggestion <- visual_offset.Value
                local_audio_offset_suggestion <- local_audio_offset.Value

            override this.Init(parent) =
                this
                |+ Panel(
                    FlowContainer.Vertical<ModeButton>(50.0f, Spacing = 10.0f)
                    |+ ModeButton(%"gameplay.hitposition.name", Mode.HIT_POSITION, this.AcceptSuggestion)
                    |+ ModeButton(%"gameplay.scrollspeed.name", Mode.SCROLL_SPEED, this.AcceptSuggestion)
                    |+ ModeButton(%"system.visualoffset.name", Mode.VISUAL_OFFSET, this.AcceptSuggestion)
                    |+ ModeButton(%"practice.localoffset.name", Mode.AUDIO_OFFSET, this.AcceptSuggestion),
                    Position = Position.Box(0.0f, 0.0f, 20.0f, 350.0f, 300.0f, 230.0f)
                )
                |+ Conditional(
                    (fun () -> mode.Audio = 0 && options.AudioVolume.Value > 0.0),
                    Callout.frame
                        (Callout.Small.Icon(Icons.VOLUME_X).Body(%"practice.mute_mandatory_hint"))
                        (fun (w, h) -> Position.Box(0.0f, 0.0f, 340.0f, 450.0f, w, h + 40.0f))
                )
                |+ Conditional(
                    (fun () -> mode.Audio = 1 && options.AudioVolume.Value > 0.0),
                    Callout.frame
                        (Callout.Small.Icon(Icons.VOLUME_X).Body(%"practice.mute_hint"))
                        (fun (w, h) -> Position.Box(0.0f, 0.0f, 340.0f, 450.0f, w, h + 40.0f))
                )
                |+ Conditional(
                    (fun () -> mode.Audio = 2 && options.AudioVolume.Value = 0.0),
                    Callout.frame
                        (Callout.Small.Icon(Icons.VOLUME_2).Body(%"practice.unmute_hint"))
                        (fun (w, h) -> Position.Box(0.0f, 0.0f, 340.0f, 450.0f, w, h + 40.0f))
                )
                |+ Conditional(
                    (fun () -> mode = Mode.HIT_POSITION),
                    Panel(
                        FlowContainer.Vertical<Widget>(50.0f, Spacing = 10.0f)
                        |+ Text(fun () -> sprintf "Current: %.0f" hit_position.Value)
                        |+ Text(fun () -> sprintf "Suggested: %.0f" hit_position_suggestion)
                        |+ suggestion_text (),
                        Position = Position.Box(0.0f, 0.0f, 20.0f, 650.0f, 500.0f, 170.0f)
                    )
                )
                |+ Conditional(
                    (fun () -> mode = Mode.SCROLL_SPEED),
                    Panel(
                        FlowContainer.Vertical<Widget>(50.0f, Spacing = 10.0f)
                        |+ Text(fun () -> sprintf "Current: %.0f%%" (100.0f * scroll_speed.Value))
                        |+ Text(fun () -> sprintf "Suggested: %.0f%%" (100.0f * scroll_speed_suggestion))
                        |+ suggestion_text (),
                        Position = Position.Box(0.0f, 0.0f, 20.0f, 650.0f, 500.0f, 170.0f)
                    )
                )
                |+ Conditional(
                    (fun () -> mode = Mode.VISUAL_OFFSET && options.AudioVolume.Value = 0.0),
                    Panel(
                        FlowContainer.Vertical<Widget>(50.0f, Spacing = 10.0f)
                        |+ Text(fun () -> sprintf "Current: %.0f" visual_offset.Value)
                        |+ Text(fun () -> sprintf "Suggested: %.0f" visual_offset_suggestion)
                        |+ suggestion_text (),
                        Position = Position.Box(0.0f, 0.0f, 20.0f, 650.0f, 500.0f, 170.0f)
                    )
                )
                |+ Conditional(
                    (fun () -> mode = Mode.AUDIO_OFFSET && options.AudioVolume.Value > 0.0),
                    Panel(
                        FlowContainer.Vertical<Widget>(50.0f, Spacing = 10.0f)
                        |+ Text(fun () -> sprintf "Current: %.0f" local_audio_offset.Value)
                        |+ Text(fun () -> sprintf "Suggested: %.0f" local_audio_offset_suggestion)
                        |+ suggestion_text (),
                        Position = Position.Box(0.0f, 0.0f, 20.0f, 650.0f, 500.0f, 170.0f)
                    )
                )
                |* Button(
                    Icons.REFRESH_CCW + " Reset offsets",
                    (fun () ->
                        local_audio_offset.Set 0.0f<ms>
                        visual_offset.Set 0.0f
                    ),
                    Position = Position.Box(0.0f, 0.0f, 20.0f, 900.0f, 300.0f, 50.0f)
                )

                base.Init parent

    let info_callout =
        Callout.Small
            .Icon(Icons.TARGET)
            .Title(%"practice.info.title")
            .Body(%"practice.info.desc")
            .Hotkey(%"practice.info.play", "skip")
            .Hotkey(%"practice.info.restart", "retry")
            .Hotkey(%"practice.info.options", "exit")
            .Body(%"practice.info.sync")
            .Hotkey(%"practice.info.accept_suggestion", "accept_suggestion")

    let rec practice_screen (info: LoadedChartInfo, practice_point: Time) =

        let last_note =
            info.WithMods.LastNote
            - 5.0f<ms>
            - Song.LEADIN_TIME * Gameplay.rate.Value

        let mutable practice_point = min last_note practice_point

        let ignore_notes_before (time: Time, scoring: ScoreMetric) =
            let mutable i = 0

            while i < scoring.HitData.Length
                  && let struct (t, _, _) = scoring.HitData.[i] in
                     t < time do
                let struct (t, deltas, flags) = scoring.HitData.[i]

                for k = 0 to info.WithMods.Keys - 1 do
                    flags.[k] <- HitStatus.NOTHING

                i <- i + 1

        let first_note = info.WithMods.FirstNote
        let mutable liveplay = LiveReplayProvider first_note

        let mutable scoring =
            Metrics.create Rulesets.current info.WithMods.Keys liveplay info.WithMods.Notes Gameplay.rate.Value

        scoring.OnHit.Add(fun h ->
            match h.Guts with
            | Hit d when not d.Missed -> Stats.session.NotesHit <- Stats.session.NotesHit + 1
            | _ -> ()
        )

        do ignore_notes_before (practice_point + Song.LEADIN_TIME * Gameplay.rate.Value, scoring)

        let binds = options.GameplayBinds.[info.WithMods.Keys - 3]
        let mutable input_key_state = 0us

        // options state
        let mutable options_mode = true

        let restart (screen: IPlayScreen) =
            liveplay <- LiveReplayProvider first_note
            scoring <- Metrics.create Rulesets.current info.WithMods.Keys liveplay info.WithMods.Notes Gameplay.rate.Value
            ignore_notes_before (practice_point + Song.LEADIN_TIME * Gameplay.rate.Value, scoring)
            screen.State.ChangeScoring scoring

            scoring.OnHit.Add(fun h ->
                match h.Guts with
                | Hit d when not d.Missed -> Stats.session.NotesHit <- Stats.session.NotesHit + 1
                | _ -> ()
            )

            Song.play_from (practice_point)

        let play (screen: IPlayScreen) =
            restart (screen)
            options_mode <- false

        let sync_ui = Sync.UI (info.Chart, info.SaveData)

        let pause (screen: IPlayScreen) =
            Song.pause ()
            Sync.update_mean scoring
            sync_ui.UpdateSuggestions()
            options_mode <- true

        let options_ui =
            StaticContainer(NodeType.None)
            |+ Timeline(
                info.WithMods,
                fun t ->
                    practice_point <- min last_note t
                    Song.seek t
            )
            |+ Callout.frame (info_callout) (fun (w, h) -> Position.Box(0.0f, 0.0f, 20.0f, 20.0f, w, h + 40.0f))
            |+ sync_ui

        { new IPlayScreen(info.Chart, info.WithColors, PacemakerInfo.None, Rulesets.current, scoring, FirstNote = first_note) with
            override this.AddWidgets() =
                let inline add_widget x =
                    add_widget (this, this.Playfield, this.State) x

                add_widget ComboMeter
                add_widget ProgressMeter
                add_widget AccuracyMeter
                add_widget HitMeter
                add_widget JudgementCounts
                add_widget JudgementMeter
                add_widget EarlyLateMeter
                add_widget RateModMeter
                add_widget BPMMeter

                this.Add(Conditional((fun () -> options_mode), options_ui))

            override this.OnEnter(p) =
                base.OnEnter(p)
                Song.seek (practice_point)
                Song.pause ()
                DiscordRPC.playing ("Practice mode", info.CacheInfo.Title)

            override this.OnBack() =
                if not options_mode then
                    pause (this)
                    input_key_state <- 0us
                    None
                else
                    Song.resume ()
                    base.OnBack()

            override this.Update(elapsed_ms, moved) =
                let now = Song.time_with_offset ()
                let chart_time = now - first_note

                if not options_mode then
                    Stats.session.PracticeTime <- Stats.session.PracticeTime + elapsed_ms

                if (%%"retry").Tapped() then
                    if options_mode then play (this) else restart (this)

                elif (%%"accept_suggestion").Tapped() then
                    if options_mode then
                        sync_ui.AcceptSuggestion()
                    else
                        pause (this)
                        sync_ui.AcceptSuggestion()
                        play (this)

                elif options_mode then
                    if (%%"skip").Tapped() then
                        play (this)
                    elif (%%"uprate_small").Tapped() then
                        Gameplay.rate.Value <- Gameplay.rate.Value + 0.01f
                    elif (%%"uprate_half").Tapped() then
                        Gameplay.rate.Value <- Gameplay.rate.Value + 0.05f
                    elif (%%"uprate").Tapped() then
                        Gameplay.rate.Value <- Gameplay.rate.Value + 0.1f
                    elif (%%"downrate_small").Tapped() then
                        Gameplay.rate.Value <- Gameplay.rate.Value - 0.01f
                    elif (%%"downrate_half").Tapped() then
                        Gameplay.rate.Value <- Gameplay.rate.Value - 0.05f
                    elif (%%"downrate").Tapped() then
                        Gameplay.rate.Value <- Gameplay.rate.Value - 0.1f

                elif not (liveplay :> IReplayProvider).Finished then
                    Input.pop_gameplay (
                        binds,
                        fun column time is_release ->
                            if is_release then
                                input_key_state <- Bitmask.unset_key column input_key_state
                            else
                                input_key_state <- Bitmask.set_key column input_key_state

                            liveplay.Add(time, input_key_state)
                    )

                    this.State.Scoring.Update chart_time

                base.Update(elapsed_ms, moved)

                if this.State.Scoring.Finished && not options_mode then
                    pause (this)
        }
