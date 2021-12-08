namespace Interlude.UI.OptionsMenu

open System.Drawing
open Prelude.Gameplay.NoteColors
open Prelude.Common
open Prelude.Data.Themes
open Interlude
open Interlude.Utils
open Interlude.Graphics
open Interlude.Options
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Controls
open Interlude.UI.Components.Selection.Compound
open Interlude.UI.Components.Selection.Menu

module Themes =

    type NoteskinPreview(scale: float32) as this =
        inherit Widget()

        let fbo = FBO.create()

        let createRenderer() =
            match Gameplay.currentChart with
            | Some chart -> 
                let nr = Screens.Play.NoteRenderer(Prelude.Scoring.Metrics.createDummyMetric chart)
                nr.Add(Screens.Play.GameplayWidgets.ScreenCover())
                nr :> Widget
            | None -> new Widget()

        let mutable renderer = createRenderer()

        do
            fbo.Unbind()

            TextBox(K "PREVIEW", K (Color.White, Color.Black), 0.5f)
            |> positionWidget(0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f, 50.0f, 1.0f)
            |> this.Add

            let w = Render.vwidth * scale
            let h = Render.vheight * scale

            this.Reposition(-50.0f - w, 1.0f, -h * 0.5f, 0.5f, -50.0f, 1.0f, h * 0.5f, 0.5f)

        member this.Refresh() =
            Gameplay.recolorChart()
            renderer.Dispose()
            renderer <- createRenderer()

        override this.Update(elapsedTime, bounds) =
            renderer.Update(elapsedTime, this.Parent.Value.Bounds)
            base.Update(elapsedTime, bounds)

        override this.Draw() =
            fbo.Bind true
            renderer.Draw()
            fbo.Unbind()
            Draw.rect this.Bounds Color.White fbo.sprite
            base.Draw()

        override this.Dispose() =
            base.Dispose()
            fbo.Dispose()

    let editNoteskin refreshNoteskins (noteSkin: NoteSkin) : SelectionPage =

        let name = Setting.simple noteSkin.Config.Name
        let keycount = Setting.simple options.KeymodePreference.Value
        let mutable noteColors = noteSkin.Config.NoteColors
        
        let g keycount i =
            let k = if noteColors.UseGlobalColors then 0 else int keycount - 2
            Setting.make
                (fun v -> noteColors.Colors.[k].[i] <- v)
                (fun () -> noteColors.Colors.[k].[i])

        let colors, refreshColors =
            refreshRow
                (fun () -> ColorScheme.count (int keycount.Value) noteColors.Style)
                (fun i k ->
                    let x = -60.0f * float32 k
                    let n = float32 i
                    ColorPicker(g keycount.Value i)
                    |> positionWidget(x + 120.0f * n, 0.5f, 0.0f, 0.0f, x + 120.0f * n + 120.0f, 0.5f, 0.0f, 1.0f))
        {
            Content = fun add ->
                column [
                    PrettySetting("NoteskinName", TextField name).Position(200.0f)
                    PrettySetting("Keymode",
                        Selector.FromEnum<Keymode>(keycount |> Setting.trigger (ignore >> refreshColors))
                    ).Position(450.0f)
                    PrettySetting("ColorStyle",
                        Selector.FromEnum(
                            Setting.make
                                (fun v -> noteColors <- { noteColors with Style = v })
                                (fun () -> noteColors.Style)
                            |> Setting.trigger (ignore >> refreshColors))
                    ).Position(550.0f)
                    PrettySetting("NoteColors", colors).Position(650.0f, Render.vwidth - 200.0f, 120.0f)
                ]
            Callback = fun () ->
                noteSkin.Config <-
                    { noteSkin.Config with
                        Name = name.Value
                        NoteColors = noteColors
                    }
                Content.Noteskins.currentConfig.Value <- noteSkin.Config
                refreshNoteskins()
        }
        

    let themeChanger refresh : SelectionPage =
        Content.Themes.detect()
        {
            Content = fun add ->
                column [
                    PrettySetting("ChooseTheme",
                        ListOrderedSelect.ListOrderedSelector(
                            Setting.make
                                ( fun v ->
                                    options.EnabledThemes.Clear()
                                    options.EnabledThemes.AddRange v
                                    Content.Themes.load options.EnabledThemes
                                    refresh()
                                )
                                (fun () -> options.EnabledThemes),
                            Content.Themes.detected
                        )
                    ).Position(200.0f, PRETTYWIDTH, 500.0f)
                    Divider().Position(750.0f)
                    PrettyButton("OpenThemeFolder",
                        fun () ->
                            //todo: move this to utils
                            let target = System.Diagnostics.ProcessStartInfo("file://" + System.IO.Path.GetFullPath(getDataPath "Themes"), UseShellExecute = true)
                            System.Diagnostics.Process.Start target |> ignore).Position(800.0f)
                    PrettyButton("NewTheme", fun () -> Dialog.add <| TextInputDialog(Render.bounds, "Enter theme name", Content.Themes.createNew)).Position(900.0f)
                ] :> Selectable
            Callback = refresh
        }

    let icon = "✎"
    let page() : SelectionPage =

        let preview = NoteskinPreview 0.5f

        let noteskins = PrettySetting("Noteskin", Selectable())
        let refreshNoteskins() =
            let ns = Content.Noteskins.list() |> Seq.toArray
            let ids = ns |> Array.map fst
            let names = ns |> Array.map (fun (id, data) -> data.Config.Name)
            options.NoteSkin.Value <- Content.Noteskins.currentId.Value
            Selector.FromArray(names, ids, options.NoteSkin |> Setting.trigger (fun id -> Content.Noteskins.switch id; preview.Refresh()))
            |> noteskins.Refresh
            preview.Refresh()
        refreshNoteskins()

        let tryEditNoteskin add =
            let ns = Content.Noteskins.current()
            match ns.StorageType with
            | Zip (_, Some file) -> 
                ConfirmDialog(
                    sprintf "'%s' cannot be edited because it is zipped. Extract and edit?" ns.Config.Name,
                    fun () -> Content.Noteskins.extractCurrent(); refreshNoteskins()
                ).Show()
            | Zip (_, None) ->
                ConfirmDialog(
                    sprintf "'%s' is an embedded default skin. Copy and edit?" ns.Config.Name,
                    fun () -> Content.Noteskins.extractCurrent(); refreshNoteskins()
                ).Show()
            | Folder _ -> add ( "EditNoteSkin", editNoteskin refreshNoteskins ns )

        {
            Content = fun add ->
                column [
                    PrettyButton("ChangeTheme", fun () -> add ("ChangeTheme", themeChanger ignore)).Position(200.0f)
                    PrettyButton("EditTheme", ignore).Position(300.0f)
                    noteskins.Position(800.0f)
                    PrettyButton("EditNoteskin", fun () -> tryEditNoteskin add).Position(900.0f)
                    preview
                ] :> Selectable
            Callback = ignore
        }
