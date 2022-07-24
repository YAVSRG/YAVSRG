namespace Interlude.UI.OptionsMenu

open Prelude.Gameplay.NoteColors
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Themes
open Interlude
open Interlude.Content
open Interlude.Utils
open Interlude.Options
open Interlude.UI
open Interlude.UI.Components.Selection.Controls
open Interlude.UI.Components.Selection.Menu

module Themes =

    type NoteskinPreview(scale: float32) as this =
        inherit StaticContainer(NodeType.None)

        let fbo = FBO.create()

        let createRenderer() =
            match Gameplay.Chart.current with
            | Some chart -> 
                let nr = Screens.Play.NoteRenderer(Prelude.Scoring.Metrics.createDummyMetric chart)
                nr.Add(Screens.Play.GameplayWidgets.ScreenCover())
                nr :> Widget1
            | None -> new Widget1()

        let mutable renderer = createRenderer()

        do
            fbo.Unbind()

            this
            |* Text("PREVIEW", Position = { Position.Default with Top = 1.0f %+ 0.0f; Bottom = 1.0f %+ 50.0f } )

            let w = Viewport.vwidth * scale
            let h = Viewport.vheight * scale

            this.Position <- 
                { 
                    Left = 1.0f %- (50.0f + w)
                    Top = 0.5f %- (h * 0.5f)
                    Right = 1.0f %- 50.0f
                    Bottom = 0.5f %+ (h * 0.5f)
                }

        member this.Refresh() =
            Gameplay.Chart.recolor()
            renderer.Dispose()
            renderer <- createRenderer()

        override this.Update(elapsedTime, moved) =
            renderer.Update(elapsedTime, Viewport.bounds)
            base.Update(elapsedTime, moved)

        override this.Draw() =
            fbo.Bind true
            renderer.Draw()
            fbo.Unbind()
            Draw.sprite this.Bounds Color.White fbo.sprite
            base.Draw()

        member this.Destroy() =
            fbo.Dispose()

    type EditNoteskinPage(refreshNoteskins) as this =
        inherit Page()

        let data = Noteskins.Current.config
        
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
                    NoteColorPicker(g keycount.Value i, Position = { Position.Default with Left = 0.5f %+ (x + 120.0f * n); Right = 0.5f %+ (x + 120.0f * n + 120.0f) })
                )

        do
            this.Content(
                column()
                |+ PrettySetting("themes.editnoteskin.noteskinname", TextEntry(name, "none")).Pos(200.0f)
                |+ PrettySetting("themes.editnoteskin.holdnotetrim", Slider(holdNoteTrim, 0.05f)).Pos(300.0f)
                |+ PrettySetting("generic.keymode",
                        Selector<Keymode>.FromEnum(keycount |> Setting.trigger (ignore >> refreshColors))
                    ).Pos(450.0f)
                |+ PrettySetting("themes.editnoteskin.globalcolors",
                        Selector<_>.FromBool(
                            Setting.make
                                (fun v -> noteColors <- { noteColors with UseGlobalColors = v })
                                (fun () -> noteColors.UseGlobalColors)
                            |> Setting.trigger (ignore >> refreshColors))
                    ).Pos(530.0f)
                |+ PrettySetting("themes.editnoteskin.colorstyle",
                        Selector.FromEnum(
                            Setting.make
                                (fun v -> noteColors <- { noteColors with Style = v })
                                (fun () -> noteColors.Style)
                            |> Setting.trigger (ignore >> refreshColors))
                    ).Pos(610.0f)
                |+ PrettySetting("themes.editnoteskin.notecolors", colors).Pos(690.0f, Viewport.vwidth - 200.0f, 120.0f)
            )

        override this.Title = data.Name
        override this.OnClose() =
            Noteskins.Current.changeConfig
                { data with
                    Name = name.Value
                    HoldNoteTrim = holdNoteTrim.Value
                    NoteColors = noteColors
                }
            refreshNoteskins()

    type EditThemePage(refreshThemes) as this =
        inherit Page()

        let data = Themes.Current.config
        
        let name = Setting.simple data.Name

        do
            this.Content(
                    PrettySetting("themes.edittheme.themename", TextEntry(name, "none")).Pos(200.0f)
                )

        override this.Title = data.Name
        override this.OnClose() =
            Themes.Current.changeConfig
                { data with
                    Name = name.Value
                }
            refreshThemes()

    type ThemesPage() as this =
        inherit Page()

        let preview = NoteskinPreview 0.5f

        let noteskins = PrettySetting("themes.noteskin", Text("")) // todo: better placeholders
        let refreshNoteskins() =
            options.Noteskin.Value <- Noteskins.Current.id
            noteskins.Child <- 
                Selector(Noteskins.list(), options.Noteskin |> Setting.trigger (fun id -> Noteskins.Current.switch id; preview.Refresh()))
            preview.Refresh()

        let themes = PrettySetting("themes.theme", Text(""))
        let refreshThemes() =
            options.Theme.Value <- Themes.Current.id
            themes.Child <-
                Selector(Themes.list(), options.Theme |> Setting.trigger (fun id -> Themes.Current.switch id; preview.Refresh()))
            preview.Refresh()

        let tryEditNoteskin() =
            let ns = Noteskins.Current.instance
            match ns.Source with
            | Zip (_, Some file) -> ()
                //ConfirmDialog(
                //    sprintf "'%s' cannot be edited because it is zipped. Extract and edit?" ns.Config.Name,
                //    fun () -> Noteskins.extractCurrent(); refreshNoteskins()
                //).Show()
            | Zip (_, None) -> ()
                //ConfirmDialog(
                //    sprintf "'%s' is an embedded default skin. Extract a copy and edit?" ns.Config.Name,
                //    fun () -> Noteskins.extractCurrent(); refreshNoteskins()
                //).Show()
            | Folder _ -> Menu.ShowPage( EditNoteskinPage refreshNoteskins )

        let tryEditTheme() =
            let theme = Themes.Current.instance
            match theme.Source with
            | Zip (_, None) -> ()
                //ConfirmDialog(
                //    sprintf "'%s' is the default theme. Extract a copy and edit?" theme.Config.Name,
                //    fun () -> Themes.createNew(System.Guid.NewGuid().ToString()); refreshThemes()
                //).Show()
            | Folder _ -> Menu.ShowPage( EditThemePage refreshThemes )
            | Zip (_, Some file) -> failwith "User themes with zip storage not supported"

        do
            refreshNoteskins()
            refreshThemes()
            
            this.Content(
                column()
                |+ themes.Pos(200.0f)
                |+ PrettyButton("themes.edittheme", tryEditTheme).Pos(300.0f)
                |+ PrettyButton("themes.showthemesfolder", fun () -> openDirectory (getDataPath "Themes")).Pos(400.0f)

                |+ Divider().Pos(550.0f)

                |+ noteskins.Pos(600.0f)
                |+ PrettyButton("themes.editnoteskin", tryEditNoteskin).Pos(700.0f)
                |+ PrettyButton("themes.shownoteskinsfolder", fun () -> openDirectory (getDataPath "Noteskins")).Pos(800.0f)
                |+ preview
            )

        override this.OnClose() = ()
        override this.OnDestroy() = preview.Destroy()
        override this.Title = N"themes"