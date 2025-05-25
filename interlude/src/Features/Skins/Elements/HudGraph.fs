namespace Interlude.Features.Play.HUD

open System
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Mods
open Prelude.Gameplay.Scoring
open Prelude.Skins.HudLayouts
open Interlude.Features.Gameplay
open Interlude.Features.Play
open Interlude.Options

[<Struct>]
type private HudGraphEvent =
    {
        Time: Time
        Position: float32
        IsRelease: bool
        Judgement: int option
    }

type HudGraph (config: HudConfig, state: PlayState) =
    inherit StaticWidget(NodeType.None)
    let hits = ResizeArray<HudGraphEvent>()
    let first_note = state.WithColors.FirstNote
  
    let color = Animation.Color(Color.White)
    let MAX_WINDOW = state.Ruleset.LargestWindow
    let mutable last_seen_time = float32 -Time.infinity
    let ln_mult = 2f

    override this.Init(parent: Widget) =
        state.Subscribe(fun ev ->
             match ev.Action with
             | Hit e ->
                 hits.Add
                    {
                        Time = ev.Time
                        Position = float32 e.Delta 
                        IsRelease = false
                        Judgement = e.Judgement |> Option.map fst
                    }
             | Hold e ->
                 hits.Add
                    {
                        Time = ev.Time
                        Position = float32 e.Delta
                        IsRelease = false
                        Judgement = e.Judgement |> Option.map fst
                    }
             | Release e ->
                 hits.Add
                    {
                        Time = ev.Time
                        Position = float32 e.Delta 
                        IsRelease = true
                        Judgement = e.Judgement |> Option.map fst
                    }
             | GhostTap e ->
                 match e.Judgement with
                 | Some (j, _) ->
                     hits.Add
                         {
                             Time = ev.Time
                             Position = 20f
                             IsRelease = false
                             Judgement = Some j
                         }
                 | None -> ()
             | DropHold
             | RegrabHold -> ()
        )
        |> ignore
        base.Init(parent)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        color.Update elapsed_ms
        let now =
            if not (config.HudGraphSmoothScrolling) then
                 if hits.Count = 0 then
                    0.0f
                 else
                    float32 hits.[hits.Count - 1].Time
            else
                float32 (state.CurrentChartTime())
        let speed = config.HudGraphScrollSpeed * 0.01f
        let x_notes_ago = int config.HudGraphShowXNotes
        if now < last_seen_time && not (config.HudGraphShowWholeChart) then
            hits.Clear()

        last_seen_time <- now
        
        if config.HudGraphScrollType = HudGraphScrollType.ScrollSpeed then
            while hits.Count > 0 && (float32 now - float32 hits.[0].Time) * speed > this.Bounds.Width do
               hits.RemoveAt(0)
               
        if config.HudGraphScrollType = HudGraphScrollType.ShowXNotes then
            let index = hits.Count - x_notes_ago
            let timeAtIndex =
                if index >= 0 then
                    float32 hits.[index].Time
                elif hits.Count > 0 then
                    float32 hits.[0].Time
                else
                    1.0f

            while hits.Count > 0 && float32(now - float32 hits.[0].Time) * this.Bounds.Width / (now - float32 timeAtIndex) > this.Bounds.Width do
               hits.RemoveAt(0)


    member this.DrawWindows(opacity: int) =

        let draw (r: float32<ms/rate> -> float32<ms/rate> -> Rect) =
            let mutable previous_early = 0.0f<ms / rate>
            let mutable previous_late = 0.0f<ms / rate>

            for j in state.Scoring.Ruleset.Judgements do
                match j.TimingWindows with
                | Some (early, late) ->
                    Render.rect
                        (r early previous_early)
                        (j.Color.O4a opacity)
                    previous_early <- early
                    Render.rect
                        (r previous_late late)
                        (j.Color.O4a opacity)
                    previous_late <- late
                | None -> ()

        let center = this.Bounds.CenterY

        let ms_to_y =
            let h = this.Bounds.Height * 0.5f
            fun time -> center - time / MAX_WINDOW * h
        let r time1 time2 = Rect.FromEdges(this.Bounds.Left, ms_to_y time1, this.Bounds.Right, ms_to_y time2)

        draw r
        
    override this.Draw() =

        if config.HudGraphTransparent = false then Render.rect this.Bounds Colors.black

        Render.rect (
            Rect.FromSize(
                this.Bounds.Left,
                this.Bounds.CenterY + 2f,
                this.Bounds.Width,
                -4f
            )
        ) Colors.white.O1
        let size = config.HudGraphHitSize
        //Render.rect (this.Bounds.SliceR (float32 size)) Colors.white.O1 
        let window_opacity = int config.HudGraphWindowOpacity
        this.DrawWindows window_opacity
        let speed = config.HudGraphScrollSpeed * 0.01f
        let smooth_scrolling = config.HudGraphSmoothScrolling

        let now =
            if not (smooth_scrolling) then
                 if hits.Count = 0 then
                    0.0f
                 else
                    float32 hits.[hits.Count - 1].Time
            else
                float32 (state.CurrentChartTime())
        let x_notes_ago = int config.HudGraphShowXNotes
        let r =
            fun p1 p2 ->
                let scaledreverseage =
                    match config.HudGraphScrollType with
                    | HudGraphScrollType.ScrollSpeed ->
                        float32(now - p2) * speed

                    | HudGraphScrollType.ShowWholeChart ->
                        float32(now - p2) * this.Bounds.Width / now

                    | HudGraphScrollType.ShowXNotes ->
                        let index = hits.Count - x_notes_ago - 1
                        let timeAtIndex =
                            if index >= 0 then
                                float32 hits.[index].Time
                            elif hits.Count > 0 then
                                float32 hits.[0].Time
                            else
                                1.0f
                        float32(now - p2) * this.Bounds.Width / (now - float32 timeAtIndex)
                    | _ -> 0f
                
                Rect.FromEdges(
                    System.Math.Clamp(this.Bounds.Right - scaledreverseage - size,
                    this.Bounds.Left,
                    this.Bounds.Right),               
                    System.Math.Clamp(this.Bounds.CenterY + p1 * this.Bounds.Height / float32 MAX_WINDOW / 2f + size * 0.5f,
                    this.Bounds.Top,
                    this.Bounds.Bottom),
                    System.Math.Clamp(this.Bounds.Right - scaledreverseage,
                    this.Bounds.Left,
                    this.Bounds.Right),              
                    System.Math.Clamp(this.Bounds.CenterY + p1 * this.Bounds.Height / float32 MAX_WINDOW / 2f - size * 0.5f,
                    this.Bounds.Top,
                    this.Bounds.Bottom))

        for hit in hits do
            let rect = r (float32 -hit.Position) (float32 hit.Time)
            let baseColor =
                match hit.Judgement with
                    | None -> Color.Silver                                     
                    | Some j -> state.Ruleset.JudgementColor j
        
            let alpha = int config.HudGraphHitOpacity
            let colorWithOpacity = Color.FromArgb(alpha * 255 / 100, baseColor)

            //if (float32 now - float32 hit.Time) * speed < this.Bounds.Width || config.HudGraphShowWholeChart then
            Render.rect
                rect
                colorWithOpacity
                    //Bounds.Width = right - left
        if config.HudGraphShowIncomingNotes then
            let draw_delta (window_time: float32) =
                if window_time < float32 MAX_WINDOW && window_time > - float32 MAX_WINDOW then

                    Rect.FromEdges(
                            this.Bounds.Right - size,
                            System.Math.Clamp(this.Bounds.CenterY - window_time * this.Bounds.Height / float32 MAX_WINDOW / 2f + size / 2f,
                            this.Bounds.Top,
                            this.Bounds.Bottom),
                            this.Bounds.Right,
                            System.Math.Clamp(this.Bounds.CenterY - window_time * this.Bounds.Height / float32 MAX_WINDOW / 2f - size / 2f,
                            this.Bounds.Top,
                            this.Bounds.Bottom)
                        )
                    |> fun a -> Render.rect a Colors.white.O2
            for { Time = time; Data = notes } in state.WithColors.Source.Notes do
                let window_time = float32 (first_note + state.CurrentChartTime() - time)
                draw_delta window_time

        Text.draw_b (
            Style.font,
            sprintf "%s (-%.0fms)" (%"hud.hudgraph.early")  (MAX_WINDOW),
            24.0f,
            this.Bounds.Left + 10.0f,
            this.Bounds.Bottom - 40.0f,
            (Color.White.O4, Color.White.O1)
            )
        Text.draw_b (
            Style.font,
            sprintf "%s (-%.0fms)" (%"hud.hudgraph.late")  (MAX_WINDOW),
            24.0f,
            this.Bounds.Left + 10.0f,
            this.Bounds.Top + 3.0f,
            (Color.White.O4, Color.White.O1)
            )
            

        if config.HudGraphDrawBorder = true then Render.border Style.PADDING this.Bounds Colors.white
                    
        

