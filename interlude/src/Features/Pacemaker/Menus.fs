namespace Interlude.Features.Pacemaker

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI
open Interlude.Content

type PacemakerOptionsPage() =
    inherit Page()

    let ruleset_id = Rulesets.current_hash

    let existing =
        if options.Pacemaker.ContainsKey ruleset_id then
            options.Pacemaker.[ruleset_id]
        else
            PacemakerSettings.Default

    let mode = Setting.simple existing.Mode

    let accuracy =
        existing.Accuracy
        |> Setting.bounded (0.0, 1.0)
        |> Setting.round 3

    let lamp = Setting.simple existing.Lamp

    let use_personal_best = Setting.simple existing.PersonalBest

    override this.Content() =
        let lamps =
            seq {
                let lamp_count = Rulesets.current.Lamps.Length
                for i in 0 .. lamp_count - 1 do
                    yield i, Rulesets.current.LampName i
                if lamp_count = 0 then
                    yield 0, Rulesets.current.LampName 0
            }
            |> Array.ofSeq

        page_container()
        |+ PageSetting(%"gameplay.pacemaker.enable", Checkbox options.EnablePacemaker)
            .Pos(0)
        |+ PageSetting(%"gameplay.pacemaker.fail_mid_song", Checkbox options.EnablePacemakerFailMidway)
            .Help(Help.Info("gameplay.pacemaker.fail_mid_song"))
            .Pos(2)
        |+ PageSetting(%"gameplay.pacemaker.onlysavenewrecords", Checkbox options.OnlySaveNewRecords)
            .Help(Help.Info("gameplay.pacemaker.onlysavenewrecords"))
            .Pos(4)
        |+ PageSetting(%"gameplay.pacemaker.type",
            SelectDropdown([| PacemakerMode.Accuracy, %"gameplay.pacemaker.accuracy"; PacemakerMode.Lamp, %"gameplay.pacemaker.lamp" |], mode)
        )
            .Pos(7)
        |+ PageSetting(%"gameplay.pacemaker.accuracy", Slider.Percent(accuracy |> Setting.f32))
            .Pos(9)
            .Conditional(fun () -> mode.Value = PacemakerMode.Accuracy)
        |+ PageSetting(%"gameplay.pacemaker.lamp", SelectDropdown(lamps, lamp))
            .Pos(9)
            .Conditional(fun () -> mode.Value = PacemakerMode.Lamp)
        |+ PageSetting(%"gameplay.pacemaker.use_personal_best",
            SelectDropdown(
                [|
                    PacemakerPersonalBestMode.Never, %"gameplay.pacemaker.use_personal_best.never"
                    PacemakerPersonalBestMode.IfBetter, %"gameplay.pacemaker.use_personal_best.if_better"
                    PacemakerPersonalBestMode.Always, %"gameplay.pacemaker.use_personal_best.always"
                |],
                use_personal_best)
        )
            .Help(Help.Info("gameplay.pacemaker.use_personal_best"))
            .Pos(11)
        :> Widget

    override this.Title = sprintf "%s %s" Icons.FLAG (%"gameplay.pacemaker")

    override this.OnClose() =
        options.Pacemaker.[ruleset_id] <-
            {
                Accuracy = accuracy.Value
                Lamp = lamp.Value
                Mode = mode.Value
                PersonalBest = use_personal_best.Value
            }