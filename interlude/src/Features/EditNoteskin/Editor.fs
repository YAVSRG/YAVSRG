namespace Interlude.Features.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Content
open Prelude.Content.Noteskins
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu

module Problems =

    let problem_card (msg: ValidationMessage) =
        match msg with
        | ValidationWarning w ->
            CalloutCard(
                Callout
                    .Normal
                    .Title(w.Element)
                    .Body(w.Message)
                //|> fun c -> match w.SuggestedFix with Some fix -> c.Button(fix.Description, fix.Action) | None -> c
                , Colors.yellow_accent, Colors.yellow_accent.O2)
        | ValidationError e ->
            CalloutCard(
                Callout
                    .Normal
                    .Title(e.Element)
                    .Body(e.Message)
                //|> fun c -> match e.SuggestedFix with Some fix -> c.Button(fix.Description, fix.Action) | None -> c
                , Colors.red_accent, Colors.red.O2)

    let problems_loader = 
        { new Async.SwitchServiceSeq<Noteskin * DynamicFlowContainer.Vertical<CalloutCard>, (unit -> unit)>() with 
            override this.Process((noteskin, container)) =
                noteskin.Validate() |> Seq.map (fun msg -> fun () -> container |* problem_card msg)
            override this.Handle(action) = action()
        }

type EditNoteskinPage(from_hotkey: bool) as this =
    inherit Page()

    let noteskin = Content.Noteskin
    let data = noteskin.Config
    let name = Setting.simple data.Name

    let preview = NoteskinPreview(0.35f, true)

    let textures_grid =
        GridFlowContainer<TextureCard>(
            PRETTYWIDTH / 5f,
            5,
            WrapNavigation = false,
            Spacing = (15.0f, 15.0f),
            Position = pretty_pos(3, PAGE_BOTTOM - 3, PageWidth.Normal)
        )

    let problems_list =
        DynamicFlowContainer.Vertical<CalloutCard>(
            Spacing = 15.0f
       )

    let refresh () =
        textures_grid.Clear()

        for texture in noteskin.RequiredTextures do
            textures_grid |* TextureCard(texture, (fun () -> TextureEditPage(texture).Show()))

        problems_list.Clear()
        Problems.problems_loader.Request(noteskin, problems_list)

    do
        refresh ()

        let general_tab =
            NavigationContainer.Column<Widget>(WrapNavigation = false)
            |+ PageTextEntry("noteskins.edit.noteskinname", name)
                .Pos(3)
            |+ PageButton(
                "noteskins.edit.playfield",
                fun () ->
                    { new PlayfieldSettingsPage() with
                        override this.OnClose() =
                            base.OnClose()
                            preview.Refresh()
                    }
                        .Show()
            )
                .Tooltip(Tooltip.Info("noteskins.edit.playfield"))
                .Pos(6)
            |+ PageButton(
                "noteskins.edit.holdnotes",
                fun () ->
                    { new HoldNoteSettingsPage() with
                        override this.OnClose() =
                            base.OnClose()
                            preview.Refresh()
                    }
                        .Show()
            )
                .Tooltip(Tooltip.Info("noteskins.edit.holdnotes"))
                .Pos(8)
            |+ PageButton(
                "noteskins.edit.colors",
                fun () ->
                    { new ColorSettingsPage() with
                        override this.OnClose() =
                            base.OnClose()
                            preview.Refresh()
                    }
                        .Show()
            )
                .Tooltip(Tooltip.Info("noteskins.edit.colors"))
                .Pos(10)
            |+ PageButton(
                "noteskins.edit.rotations",
                fun () ->
                    { new RotationSettingsPage() with
                        override this.OnClose() =
                            base.OnClose()
                            preview.Refresh()
                    }
                        .Show()
            )
                .Tooltip(Tooltip.Info("noteskins.edit.rotations"))
                .Pos(12)
            |+ PageButton(
                "noteskins.animations",
                fun () ->
                    { new AnimationSettingsPage() with
                        override this.OnClose() =
                            base.OnClose()
                            preview.Refresh()
                    }
                        .Show()
            )
                .Tooltip(Tooltip.Info("noteskins.animations"))
                .Pos(14)
            |+ PageButton(
                "hud",
                fun () -> EditHUDPage().Show()
            )
                .Tooltip(Tooltip.Info("hud"))
                .Pos(16)

        let textures_tab =
            textures_grid

        let problems_tab =
            ScrollContainer(
                problems_list,
                Margin = Style.PADDING,
                Position = pretty_pos(3, PAGE_BOTTOM - 3, PageWidth.Normal)
            )

        let tabs = SwapContainer(general_tab, Position = Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))

        let tab_buttons =
            RadioButtons.create_tabs
                {
                    Setting = Setting.make tabs.set_Current tabs.get_Current
                    Options =
                        [|
                            general_tab, %"noteskins.edit.general", K false
                            textures_tab, %"noteskins.edit.textures", K false
                            problems_tab, %"noteskins.edit.problems", K false
                        |]
                    Height = 50.0f
                }

        tab_buttons.Position <- pretty_pos(0, 2, PageWidth.Normal).Translate(PRETTY_MARGIN_X, PRETTY_MARGIN_Y)

        NavigationContainer.Column<Widget>()
        |+ tab_buttons
        |+ tabs
        |>> Container
        |+ preview
        |+ Conditional(
            (fun () -> not from_hotkey),
            Callout.frame
                (Callout.Small
                    .Icon(Icons.INFO)
                    .Title(%"noteskins.edit.hotkey_hint")
                    .Hotkey("edit_noteskin"))
                (fun (w, h) -> Position.SliceTop(h).SliceRight(w).Translate(-20.0f, 20.0f))
        )
        |> this.Content

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        Problems.problems_loader.Join()

    override this.Title = data.Name
    override this.OnDestroy() = preview.Destroy()

    override this.OnReturnTo() =
        refresh ()
        base.OnReturnTo()

    override this.OnClose() =
        Noteskins.save_config
            { Content.NoteskinConfig with
                Name = name.Value
            }
