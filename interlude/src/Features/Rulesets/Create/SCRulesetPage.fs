namespace Interlude.Features.Rulesets

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.Content
open Interlude.UI

type SCRulesetPage() =
    inherit Page()

    let judge = Setting.simple 4

    let create () =
        Rulesets.install (SC.create judge.Value)
        Menu.Back()
        Menu.Back()

    override this.Content() =
        page_container()
            .With(
                PageSetting(%"ruleset.sc_judge",
                    SelectDropdown(
                        seq { 2 .. 9 } |> Seq.map (fun j -> j, j.ToString()) |> Array.ofSeq,
                        judge
                    )
                )
                    .Pos(0, 2, PageWidth.Custom (PAGE_LABEL_WIDTH + 200.0f)),
                PageButton.Once(%"rulesets.create", create)
                    .Pos(3)
            )

    override this.Title = %"rulesets.create.sc"