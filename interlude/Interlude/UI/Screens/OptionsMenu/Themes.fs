namespace Interlude.UI.OptionsMenu

open Prelude.Gameplay.NoteColors
open Prelude.Common
open Prelude.Data.Themes
open Interlude
open Interlude.Content
open Interlude.Utils
open Interlude.Graphics
open Interlude.Options
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Controls
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

    let editNoteskin refreshNoteskins (data: NoteskinConfig) : SelectionPage =

        let name = Setting.simple data.Name
        let keycount = Setting.simple options.KeymodePreference.Value
        let holdNoteTrim = Setting.bounded data.HoldNoteTrim 0.0f 2.0f |> Setting.roundf 2
        let mutable noteColors = data.NoteColors
        
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
                    NoteColorPicker(g keycount.Value i)
                    |> positionWidget(x + 120.0f * n, 0.5f, 0.0f, 0.0f, x + 120.0f * n + 120.0f, 0.5f, 0.0f, 1.0f))
        {
            Content = fun add ->
                column [
                    PrettySetting("NoteskinName", TextField name).Position(200.0f)
                    PrettySetting("HoldNoteTrim", Slider(holdNoteTrim, 0.05f)).Position(300.0f)
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
                Noteskins.Current.changeConfig
                    { data with
                        Name = name.Value
                        HoldNoteTrim = holdNoteTrim.Value
                        NoteColors = noteColors
                    }
                refreshNoteskins()
        }

    let editTheme refreshThemes (data: ThemeConfig) : SelectionPage =

        let name = Setting.simple data.Name

        {
            Content = fun add ->
                column [
                    PrettySetting("ThemeName", TextField name).Position(200.0f)
                ]
            Callback = fun () ->
                Themes.Current.changeConfig
                    { data with
                        Name = name.Value
                    }
                refreshThemes()
        }

    let page() : SelectionPage =

        let preview = NoteskinPreview 0.5f

        let noteskins = PrettySetting("Noteskin", Selectable())
        let refreshNoteskins() =
            options.Noteskin.Value <- Noteskins.Current.id
            let ids, names = Noteskins.list() |> Array.unzip
            Selector.FromArray(names, ids, options.Noteskin |> Setting.trigger (fun id -> Noteskins.Current.switch id; preview.Refresh()))
            |> noteskins.Refresh
            preview.Refresh()
        refreshNoteskins()

        let themes = PrettySetting("Theme", Selectable())
        let refreshThemes() =
            options.Theme.Value <- Themes.Current.id
            let ids, names = Themes.list() |> Array.unzip
            Selector.FromArray(names, ids, options.Theme |> Setting.trigger (fun id -> Themes.Current.switch id; preview.Refresh()))
            |> themes.Refresh
            preview.Refresh()
        refreshThemes()

        let tryEditNoteskin add =
            let ns = Noteskins.Current.instance
            match ns.StorageType with
            | Zip (_, Some file) -> 
                ConfirmDialog(
                    sprintf "'%s' cannot be edited because it is zipped. Extract and edit?" ns.Config.Name,
                    fun () -> Noteskins.extractCurrent(); refreshNoteskins()
                ).Show()
            | Zip (_, None) ->
                ConfirmDialog(
                    sprintf "'%s' is an embedded default skin. Extract a copy and edit?" ns.Config.Name,
                    fun () -> Noteskins.extractCurrent(); refreshNoteskins()
                ).Show()
            | Folder _ -> add ( "EditNoteskin", editNoteskin refreshNoteskins ns.Config )

        let tryEditTheme add =
            let theme = Themes.Current.instance
            match theme.StorageType with
            | Zip (_, None) ->
                ConfirmDialog(
                    sprintf "'%s' is the default theme. Extract a copy and edit?" theme.Config.Name,
                    fun () -> Themes.createNew(System.Guid.NewGuid().ToString()); refreshThemes()
                ).Show()
            | Folder _ -> add ( "EditTheme", editTheme refreshThemes theme.Config )
            | Zip (_, Some file) -> failwith "User themes with zip storage not supported"

        {
            Content = fun add ->
                column [
                    themes.Position(200.0f)
                    PrettyButton("EditTheme", fun () -> tryEditTheme add).Position(300.0f)
                    PrettyButton("OpenThemeFolder", fun () -> openDirectory (getDataPath "Themes")).Position(400.0f)

                    Divider().Position(550.0f)

                    noteskins.Position(600.0f)
                    PrettyButton("EditNoteskin", fun () -> tryEditNoteskin add).Position(700.0f)
                    PrettyButton("OpenNoteskinFolder", fun () -> openDirectory (getDataPath "Noteskins")).Position(800.0f)
                    preview
                ] :> Selectable
            Callback = ignore
        }
