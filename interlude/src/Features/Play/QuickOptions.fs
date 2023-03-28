namespace Interlude.Features.Play

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Scoring
open Interlude.Features
open Interlude.Options
open Interlude.Utils
open Interlude.UI.Menu

module QuickOptions =

    type QuickOptionsPage(scoring: IScoreMetric, callback) as this =
        inherit Page()

        do
            let mutable sum = 0.0f<ms>
            let mutable count = 1.0f
            for ev in scoring.HitEvents do
                match ev.Guts with
                | Hit x when not x.Missed ->
                    sum <- sum + x.Delta
                    count <- count + 1.0f
                | _ -> ()
            let mean = sum / count * Gameplay.rate.Value

            let firstNote = Gameplay.Chart.current.Value.FirstNote
            let offset = 
                Setting.make
                    (fun v -> Gameplay.Chart.saveData.Value.Offset <- toTime v + firstNote; Song.changeLocalOffset(toTime v))
                    (fun () -> float (Gameplay.Chart.saveData.Value.Offset - firstNote))
                |> Setting.bound -200.0 200.0
                |> Setting.round 0
            let recommendedOffset = float (Gameplay.Chart.saveData.Value.Offset - firstNote - mean)

            this.Content(
                column()
                |+ PrettySetting("quick.localoffset", Slider(offset, 0.0025f)).Pos(200f)
                |+ Text(
                        sprintf "Suggested: %.0f" recommendedOffset,
                        Align = Alignment.RIGHT,
                        Position = Position.Box(1.0f, 0.0f, -600.0f, 200.0f, 550.0f, PRETTYHEIGHT)
                    )
                |+ PrettyButton("quick.applyoffset", fun () -> offset.Value <- recommendedOffset).Pos(280f)

                |+ PrettySetting("gameplay.scrollspeed", Slider<_>.Percent(options.ScrollSpeed, 0.0025f)).Pos(380.0f)
                |+ PrettySetting("gameplay.hitposition", Slider(options.HitPosition, 0.005f)).Pos(460.0f)
                |+ PrettySetting("gameplay.upscroll", Selector<_>.FromBool options.Upscroll).Pos(540.0f)
            )

        override this.Title = L"options.quick.name"
        override this.OnClose() = callback()

    let show(scoring, callback) = QuickOptionsPage(scoring, callback).Show()