namespace Interlude.UI.OptionsMenu

open Prelude.Gameplay.NoteColors
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Data.Themes
open Interlude
open Interlude.Content
open Interlude.Utils
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
            match Gameplay.Chart.current with
            | Some chart -> 
                let nr = Screens.Play.NoteRenderer(Prelude.Scoring.Metrics.createDummyMetric chart)
                nr.Add(Screens.Play.GameplayWidgets.ScreenCover())
                nr :> Widget
            | None -> new Widget()

        let mutable renderer = createRenderer()

        do
            fbo.Unbind()

            TextBox(K "PREVIEW", K (Color.White, Color.Black), 0.5f)
                .Position { Position.Default with Top = 1.0f %+ 0.0f; Bottom = 1.0f %+ 50.0f }
            |> this.Add

            let w = Viewport.vwidth * scale
            let h = Viewport.vheight * scale

            this.Reposition(-50.0f - w, 1.0f, -h * 0.5f, 0.5f, -50.0f, 1.0f, h * 0.5f, 0.5f)

        member this.Refresh() =
            Gameplay.Chart.recolor()
            renderer.Dispose()
            renderer <- createRenderer()

        override this.Update(elapsedTime, bounds) =
            renderer.Update(elapsedTime, this.Parent.Value.Bounds)
            base.Update(elapsedTime, bounds)

        override this.Draw() =
            fbo.Bind true
            renderer.Draw()
            fbo.Unbind()
            Draw.sprite this.Bounds Color.White fbo.sprite
            base.Draw()

        override this.Dispose() =
            base.Dispose()
            fbo.Dispose()

    let editNoteskin refreshNoteskins (data: NoteskinConfig) : SelectionPage =

        let name = Setting.simple data.Name
        let holdNoteTrim = Setting.bounded data.HoldNoteTrim 0.0f 2.0f |> Setting.roundf 2
        let keycount = Setting.simple options.KeymodePreference.Value
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
                    NoteColorPicker(g keycount.Value i).Position
                        { Position.Default with Left = 0.5f %+ (x + 120.0f * n); Right = 0.5f %+ (x + 120.0f * n + 120.0f) }
                )
        {
            Content = fun add ->
                column [
                    PrettySetting("themes.editnoteskin.noteskinname", TextField name).Position(200.0f)
                    PrettySetting("themes.editnoteskin.holdnotetrim", Slider(holdNoteTrim, 0.05f)).Position(300.0f)
                    PrettySetting("generic.keymode",
                        Selector<Keymode>.FromEnum(keycount |> Setting.trigger (ignore >> refreshColors))
                    ).Position(450.0f)
                    PrettySetting("themes.editnoteskin.globalcolors",
                        Selector<_>.FromBool(
                            Setting.make
                                (fun v -> noteColors <- { noteColors with UseGlobalColors = v })
                                (fun () -> noteColors.UseGlobalColors)
                            |> Setting.trigger (ignore >> refreshColors))
                    ).Position(530.0f)
                    PrettySetting("themes.editnoteskin.colorstyle",
                        Selector.FromEnum(
                            Setting.make
                                (fun v -> noteColors <- { noteColors with Style = v })
                                (fun () -> noteColors.Style)
                            |> Setting.trigger (ignore >> refreshColors))
                    ).Position(610.0f)
                    PrettySetting("themes.editnoteskin.notecolors", colors).Position(690.0f, Viewport.vwidth - 200.0f, 120.0f)
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
                    PrettySetting("themes.edittheme.themename", TextField name).Position(200.0f)
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

        let noteskins = PrettySetting("themes.noteskin", Selectable())
        let refreshNoteskins() =
            options.Noteskin.Value <- Noteskins.Current.id
            Selector(Noteskins.list(), options.Noteskin |> Setting.trigger (fun id -> Noteskins.Current.switch id; preview.Refresh()))
            |> noteskins.Refresh
            preview.Refresh()
        refreshNoteskins()

        let themes = PrettySetting("themes.theme", Selectable())
        let refreshThemes() =
            options.Theme.Value <- Themes.Current.id
            Selector(Themes.list(), options.Theme |> Setting.trigger (fun id -> Themes.Current.switch id; preview.Refresh()))
            |> themes.Refresh
            preview.Refresh()
        refreshThemes()

        let tryEditNoteskin add =
            let ns = Noteskins.Current.instance
            match ns.Source with
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
            | Folder _ -> add ( E ns.Config.Name, editNoteskin refreshNoteskins ns.Config )

        let tryEditTheme add =
            let theme = Themes.Current.instance
            match theme.Source with
            | Zip (_, None) ->
                ConfirmDialog(
                    sprintf "'%s' is the default theme. Extract a copy and edit?" theme.Config.Name,
                    fun () -> Themes.createNew(System.Guid.NewGuid().ToString()); refreshThemes()
                ).Show()
            | Folder _ -> add ( E theme.Config.Name, editTheme refreshThemes theme.Config )
            | Zip (_, Some file) -> failwith "User themes with zip storage not supported"

        {
            Content = fun add ->
                column [
                    themes.Position(200.0f)
                    PrettyButton("themes.edittheme", fun () -> tryEditTheme add).Position(300.0f)
                    PrettyButton("themes.showthemesfolder", fun () -> openDirectory (getDataPath "Themes")).Position(400.0f)

                    Divider().Position(550.0f)

                    noteskins.Position(600.0f)
                    PrettyButton("themes.editnoteskin", fun () -> tryEditNoteskin add).Position(700.0f)
                    PrettyButton("themes.shownoteskinsfolder", fun () -> openDirectory (getDataPath "Noteskins")).Position(800.0f)
                    preview
                ] :> Selectable
            Callback = ignore
        }
