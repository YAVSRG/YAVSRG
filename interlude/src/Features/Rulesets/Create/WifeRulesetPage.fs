namespace Interlude.Features.Rulesets

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.Content
open Interlude.UI

type WifeRulesetPage() =
    inherit Page()

    let judge = Setting.simple 4

    let create () =
        Rulesets.install (Wife3.create judge.Value)
        Menu.Back()
        Menu.Back()

    override this.Content() =
        page_container()
            .With(
                PageSetting(%"ruleset.wife_judge",
                    SelectDropdown(
                        [|
                            4, "4"
                            5, "5"
                            6, "6"
                            7, "7"
                            8, "8"
                            9, "JUSTICE"
                        |],
                        judge
                    )
                )
                    .Pos(0, 2, PageWidth.Custom (PAGE_LABEL_WIDTH + 200.0f)),
                PageButton.Once(%"rulesets.create", create)
                    .Pos(3)
            )

    override this.Title = %"rulesets.create.wife"