namespace Interlude.Features.Rulesets

open Interlude.Features.Gameplay
open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Data.Options
open Interlude.Content
open Interlude.Options
open Interlude.UI
open Interlude.Features.Rulesets.Edit

type SelectRulesetPage() =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PAGE_ITEM_HEIGHT)
    let mutable selected = rulesets.Selected
    let update_selected () = selected <- rulesets.Selected

    let listed_ruleset (id: string, ruleset_info: RulesetInfo) : Widget =

        let name_button =
            PageButton(
                ruleset_info.Ruleset.Name,
                fun () ->
                    options.Ruleset.Set(FileId(id))
                    update_selected()
            )
                .TextColor(fun () -> if options.Ruleset.Value = FileId(id) then Colors.text_pink_2 else Colors.text)

        let compare_action =
            Button(
                Icons.PIE_CHART,
                fun () ->
                    CompareRulesetsPage(rulesets.GetCurrent(SelectedChart.CACHE_DATA).Ruleset, ruleset_info.Ruleset)
                        .Show()
            )
                .Disabled(fun () -> selected = FileId(id))
                .Position(Position.SliceR(PAGE_ITEM_HEIGHT * 3.0f, PAGE_ITEM_HEIGHT))

        let edit_action =
            Button(Icons.EDIT, fun () -> RulesetEditorPage(id, ruleset_info.Ruleset).Show())
                .Disabled(not(rulesets.CanUpdate(id)))

        let copy_action =
            Button(
                Icons.COPY,
                fun () ->
                    ConfirmPage(
                        [ ruleset_info.Ruleset.Name ] %> "rulesets.confirm_copy",
                        fun () ->
                            rulesets.Install(
                                { ruleset_info.Ruleset.Clone() with Name = ruleset_info.Ruleset.Name + " (Copy)" }
                            )
                            |> ignore
                    )
                        .Show()
            )

        let delete_action =
            Button(
                Icons.TRASH,
                (fun () ->
                    ConfirmPage(
                        [ ruleset_info.Ruleset.Name ] %> "rulesets.confirm_delete",
                        fun () ->
                            if rulesets.Delete(id) then
                                update_selected()
                    )
                        .Show()
                )
            )
                .Disabled(not(rulesets.CanDelete(id)))
                .TextColor(Colors.red_accent)

        NavigationContainer
            .Row()
            .With(
                name_button.Position(Position.ShrinkR(PAGE_ITEM_HEIGHT * 4.0f)),
                compare_action.Position(Position.SliceR(PAGE_ITEM_HEIGHT * 3.0f, PAGE_ITEM_HEIGHT)),
                edit_action.Position(Position.SliceR(PAGE_ITEM_HEIGHT * 2.0f, PAGE_ITEM_HEIGHT)),
                copy_action.Position(Position.SliceR(PAGE_ITEM_HEIGHT, PAGE_ITEM_HEIGHT)),
                delete_action.Position(Position.SliceR(PAGE_ITEM_HEIGHT))
            )

    let automatic_osu () : Widget =
        PageButton(%"rulesets.osu_automatic", fun () -> OsuAutomaticPage().WithOnClose(update_selected).Show())
            .TextColor(fun () -> if options.Ruleset.Value.IsOsuAutomatic then Colors.text_pink_2 else Colors.text)
            .Help(Help.Info("rulesets.osu_automatic"))

    let refresh () =
        container.Clear()

        container
            .With(
                PageButton(%"rulesets.add", fun () -> AddRulesetsPage().Show()).Icon(Icons.DOWNLOAD),
                PageButton(%"rulesets.open_folder", fun () -> open_directory(get_game_folder "Rulesets"))
                    .Icon(Icons.FOLDER),
                Dummy(),
                automatic_osu()
            )
            .Add(rulesets.ToList() |> Seq.map listed_ruleset)

        if container.Focused then
            container.Focus(false)

    override this.Content() : Widget =
        refresh()
        ScrollContainer(container).Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).SliceL(PAGE_ITEM_WIDTH))

    override this.Update(elapsed_ms: float, moved: bool) : unit =
        base.Update(elapsed_ms, moved)

        if (%%"reload_content").Pressed() then
            Style.notify_system.Play()
            Menu.Back()
            rulesets.Load()
            SelectRulesetPage().Show()

    override this.Title = sprintf "%s %s" Icons.SLIDERS (%"rulesets")
    override this.OnReturnFromNestedPage() : unit = refresh()
