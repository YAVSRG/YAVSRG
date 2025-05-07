namespace Interlude.Features.Play.HUD

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Gameplay.Scoring
open Prelude.Skins.HudLayouts
open Interlude.Features.Play
open Interlude.Features.Gameplay

type KeysPerSecond(config: HudConfig, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    let UPDATE_INTERVAL = 100.0
    let mutable update_timer = UPDATE_INTERVAL

    let mutable kps = 0.0f
    let mutable max_kps = 0.0f
    let mutable count = 0.0f

    let shown_extras =
        (if config.KeysPerSecondMeterShowAverage then 1 else 0) +
        (if config.KeysPerSecondMeterShowMax then 1 else 0) +
        (if config.KeysPerSecondMeterShowTotal then 1 else 0)

    let extras =
        let positions : (Rect -> Rect) seq =
            match shown_extras with
            | 3 -> [| (fun r -> r.SlicePercentT 0.4f); (fun r -> r.SlicePercentY 0.4f); (fun r -> r.SlicePercentB 0.4f) |]
            | 2 -> [| (fun r -> r.SlicePercentT 0.55f); (fun r -> r.SlicePercentB 0.55f) |]
            | 1 -> [| fun r -> r.SlicePercentY 0.7f |]
            | _ -> [||]
        let texts : (unit -> string) seq =
            seq {
                if config.KeysPerSecondMeterShowAverage then
                    yield fun () -> sprintf "AVG: %.0f" (count / (state.CurrentChartTime() / state.Scoring.Rate / 1000.0f<ms / rate> |> max 0.1f))
                if config.KeysPerSecondMeterShowMax then
                    yield fun () -> sprintf "MAX: %.0f" max_kps
                if config.KeysPerSecondMeterShowTotal then
                    yield fun () -> sprintf "TOTAL: %.0f" count
            }
        Seq.zip positions texts
        |> Array.ofSeq

    let split_a, split_b = if shown_extras = 3 then 0.4f, 0.65f else 0.5f, 0.5f

    let update_kps() =
        let rate = SelectedChart.rate.Value
        let TWO_SECONDS = 2000.0f<ms / rate> * rate

        let recent_events = state.Scoring.EnumerateRecentFrames()
        let now = state.CurrentChartTime()
        kps <- 0.0f
        let mutable previous = 0us
        let mutable previous_time = now
        for struct (timestamp, keystate) in recent_events |> Seq.takeWhile (fun _ -> previous_time >= now - TWO_SECONDS) do
            let keys = Bitmask.count (previous &&& ~~~keystate) |> float32
            kps <- kps + keys * (1f - (now - previous_time) / TWO_SECONDS)
            max_kps <- max max_kps kps
            previous <- keystate
            previous_time <- timestamp

    override this.Init(parent: Widget) =
        state.SubscribeEvents(fun h ->
            match h.Action with
            | Hit h
            | Hold h -> if not h.Missed then count <- count + 1.0f
            | GhostTap _
            | RegrabHold -> count <- count + 1.0f
            | DropHold
            | Release _ -> ()
        )
        state.ScoringChanged.Publish.Add(fun () -> count <- float32 state.Scoring.Events.Count)
        base.Init parent

    override this.Draw() =
        let text_bounds = this.Bounds.SlicePercentT split_a
        let stat_bounds = this.Bounds.SlicePercentB split_b
        Text.fill_b(Style.font, sprintf "%.0f KPS" kps, text_bounds, Colors.text, Alignment.CENTER)
        for p, t in extras do
            Text.fill_b(Style.font, t(), p stat_bounds, Colors.text_subheading, Alignment.CENTER)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        update_timer <- update_timer - elapsed_ms
        if update_timer <= 0.0 then
            update_kps()
            update_timer <- UPDATE_INTERVAL