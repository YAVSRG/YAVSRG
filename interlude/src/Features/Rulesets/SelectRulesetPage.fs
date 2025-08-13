namespace Interlude.Features.Rulesets

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.Content
open Interlude.Options
open Interlude.UI
open Interlude.Features.Rulesets.Edit

type SelectRulesetPage() =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PAGE_ITEM_HEIGHT)

    let refresh () =
        container.Clear()

        container
            .With(
                PageButton(
                    %"rulesets.add",
                    fun () -> AddRulesetsPage().Show()
                )
                    .Icon(Icons.DOWNLOAD),
                PageButton(
                    %"rulesets.open_folder",
                    fun () -> open_directory (get_game_folder "Rulesets")
                )
                    .Icon(Icons.FOLDER),
                Dummy()
            )
            .Add(seq {
                for id, ruleset in Rulesets.list () do
                    yield NavigationContainer.Row()
                        .With(
                            PageButton(ruleset.Name, fun () -> options.SelectedRuleset.Set id)
                                .TextColor(fun () -> if options.SelectedRuleset.Value = id then Colors.text_pink_2 else Colors.text)
                                .Position(Position.ShrinkR(PAGE_ITEM_HEIGHT * 4.0f)),
                            Button(Icons.PIE_CHART, fun () -> CompareRulesetsPage(Rulesets.current, ruleset).Show())
                                .Position(Position.SliceR(PAGE_ITEM_HEIGHT * 3.0f, PAGE_ITEM_HEIGHT))
                                .Disabled(fun () -> Rulesets.current = ruleset),
                            Button(Icons.EDIT, fun () -> RulesetEditorPage(id, ruleset).Show())
                                .Position(Position.SliceR(PAGE_ITEM_HEIGHT * 2.0f, PAGE_ITEM_HEIGHT)),
                            Button(Icons.COPY, fun () ->
                                ConfirmPage(
                                    [ruleset.Name] %> "rulesets.confirm_copy",
                                    fun () ->
                                        Rulesets.install { ruleset.Clone with Name = ruleset.Name + " (Copy)" }
                                )
                                    .Show()
                            )
                                .Position(Position.SliceR(PAGE_ITEM_HEIGHT, PAGE_ITEM_HEIGHT)),
                            Button(
                                Icons.TRASH,
                                (fun () ->
                                    ConfirmPage(
                                        [ruleset.Name] %> "rulesets.confirm_delete",
                                        fun () -> Rulesets.delete id |> ignore
                                    )
                                        .Show()
                                )
                            )
                                .Disabled(Rulesets.DEFAULT_ID = id)
                                .TextColor(Colors.red_accent)
                                .Position(Position.SliceR(PAGE_ITEM_HEIGHT))
                        )
                        :> Widget
            })

        if container.Focused then
            container.Focus false

    override this.Content() =
        refresh ()
        ScrollContainer(container)
            .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).SliceL(PAGE_ITEM_WIDTH))

    override this.Update (elapsed_ms: float, moved: bool) =
        base.Update(elapsed_ms, moved)
        if (%%"reload_content").Pressed() then
            Style.notify_system.Play()
            Menu.Back()
            Rulesets.load ()
            SelectRulesetPage().Show()

    override this.Title = sprintf "%s %s" Icons.SLIDERS (%"rulesets")
    override this.OnReturnFromNestedPage() = refresh ()