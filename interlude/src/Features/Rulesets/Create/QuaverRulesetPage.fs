namespace Interlude.Features.Rulesets

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.Content
open Interlude.UI

type QuaverRulesetPage() =
    inherit Page()

    let judgement = Setting.simple Quaver.Standard

    let create () =
        Rulesets.install (Quaver.create judgement.Value)
        Menu.Back()
        Menu.Back()

    override this.Content() =
        page_container()
            .With(
                PageSetting(
                    %"ruleset.quaver_judgement",
                    SelectDropdown(
                        Quaver.Judgement.LIST |> Array.map (fun j -> j, j.ToString()),
                        judgement
                    )
                )
                    .Pos(0),
                PageButton.Once(%"rulesets.create", create)
                    .Pos(3)
            )

    override this.Title = %"rulesets.create.quaver"