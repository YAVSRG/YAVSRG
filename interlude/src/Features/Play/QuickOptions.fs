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

// todo: replace with offset wizard

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
                    (fun v -> Gameplay.Chart.saveData.Value.Offset <- v * 1.0f<ms> + firstNote; Song.changeLocalOffset(v * 1.0f<ms>))
                    (fun () -> (Gameplay.Chart.saveData.Value.Offset - firstNote) / 1.0f<ms>)
                |> Setting.bound -200.0f 200.0f
                |> Setting.roundf 0
            let recommendedOffset = Gameplay.Chart.saveData.Value.Offset - firstNote - mean

            this.Content(
                column()
                |+ PageSetting("quick.localoffset", Slider(offset, Step = 1f)).Pos(200f)
                |+ Text(
                        sprintf "Suggested: %.0f" recommendedOffset,
                        Align = Alignment.RIGHT,
                        Position = Position.Box(1.0f, 0.0f, -600.0f, 200.0f, 550.0f, PRETTYHEIGHT)
                    )
                |+ PageButton("quick.applyoffset", fun () -> offset.Value <- recommendedOffset / 1.0f<ms>).Pos(280f)

                |+ PageSetting("gameplay.scrollspeed", Slider.Percent(options.ScrollSpeed)).Pos(380.0f)
                |+ PageSetting("gameplay.hitposition", Slider(options.HitPosition, Step = 1f)).Pos(460.0f)
                |+ PageSetting("gameplay.upscroll", Selector<_>.FromBool options.Upscroll).Pos(540.0f)
            )

        override this.Title = L"quick.name"
        override this.OnClose() = callback()

    let show(scoring, callback) = QuickOptionsPage(scoring, callback).Show()