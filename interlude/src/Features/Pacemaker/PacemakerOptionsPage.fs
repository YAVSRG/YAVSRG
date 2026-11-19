namespace Interlude.Features.Pacemaker

open Interlude.Features.Gameplay
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI
open Interlude.Content

type PacemakerOptionsPage() =
    inherit Page()

    let ruleset_info = rulesets.GetCurrent(SelectedChart.CACHE_DATA)

    let existing = options.GetPacemakerOptions()

    let mode = Setting.simple existing.Mode

    let accuracy = existing.Accuracy |> Setting.bounded(0.0, 1.0) |> Setting.round 4

    let lamp = Setting.simple existing.Lamp

    let use_personal_best = Setting.simple existing.PersonalBest

    override this.Content() : Widget =

        this.OnClose(this.Save)

        let lamps =
            seq {
                let lamp_count = ruleset_info.Ruleset.Lamps.Length

                for i in 0 .. lamp_count - 1 do
                    yield i, ruleset_info.Ruleset.LampName(i)

                if lamp_count = 0 then
                    yield 0, ruleset_info.Ruleset.LampName(0)
            }
            |> Array.ofSeq

        page_container()
            .With(
                PageSetting(%"gameplay.pacemaker.enable", Checkbox options.EnablePacemaker).Pos(0),
                PageSetting(%"gameplay.pacemaker.fail_mid_song", Checkbox options.EnablePacemakerFailMidway)
                    .Help(Help.Info("gameplay.pacemaker.fail_mid_song"))
                    .Pos(2),
                PageSetting(%"gameplay.pacemaker.onlysavenewrecords", Checkbox options.OnlySaveNewRecords)
                    .Help(Help.Info("gameplay.pacemaker.onlysavenewrecords"))
                    .Pos(4),
                PageSetting(
                    %"gameplay.pacemaker.type",
                    SelectDropdown(
                        [|
                            PacemakerMode.Accuracy, %"gameplay.pacemaker.accuracy"
                            PacemakerMode.Lamp, %"gameplay.pacemaker.lamp"
                        |],
                        mode
                    )
                )
                    .Pos(7),
                PageSetting(%"gameplay.pacemaker.target", NumberEntry.CreatePercent(accuracy))
                    .Pos(9)
                    .Conditional(fun () -> mode.Value = PacemakerMode.Accuracy),
                PageSetting(%"gameplay.pacemaker.target", SelectDropdown(lamps, lamp))
                    .Pos(9)
                    .Conditional(fun () -> mode.Value = PacemakerMode.Lamp),
                PageSetting(
                    %"gameplay.pacemaker.use_personal_best",
                    SelectDropdown(
                        [|
                            PacemakerPersonalBestMode.Never, %"gameplay.pacemaker.use_personal_best.never"
                            PacemakerPersonalBestMode.IfBetter, %"gameplay.pacemaker.use_personal_best.if_better"
                            PacemakerPersonalBestMode.Always, %"gameplay.pacemaker.use_personal_best.always"
                        |],
                        use_personal_best
                    )
                )
                    .Help(Help.Info("gameplay.pacemaker.use_personal_best"))
                    .Pos(11)
            )

    override this.Title = sprintf "%s %s" Icons.FLAG (%"gameplay.pacemaker")

    member this.Save() : unit =
        options.SavePacemakerOptions(
            {
                Accuracy = accuracy.Value
                Lamp = lamp.Value
                Mode = mode.Value
                PersonalBest = use_personal_best.Value
            }
        )
