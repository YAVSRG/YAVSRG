namespace Interlude.Features.OptionsMenu.Themes

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Scoring
open Prelude.Data.Themes
open Interlude.Content
open Interlude.Utils
open Interlude.Options
open Interlude.UI.Menu
open Interlude.Features
open Interlude.Features.Play

type NoteskinPreview(scale: float32) as this =
    inherit StaticContainer(NodeType.None)

    let fbo = FBO.create()

    let createRenderer() =
        match Gameplay.Chart.current with
        | Some chart -> 
            let nr = NoteRenderer(Metrics.createDummyMetric chart)
            nr.Add(GameplayWidgets.ScreenCover())
            if this.Initialised then nr.Init this
            nr :> Widget
        | None -> new Dummy()

    let mutable renderer = createRenderer()


    let w = Viewport.vwidth * scale
    let h = Viewport.vheight * scale
    let bounds_placeholder =
        StaticContainer(NodeType.None,
            Position = { Left = 1.0f %- (50.0f + w); Top = 0.5f %- (h * 0.5f); Right = 1.0f %- 50.0f; Bottom = 0.5f %+ (h * 0.5f) })

    do
        fbo.Unbind()

        this
        |* ( bounds_placeholder |+  Text("PREVIEW", Position = { Position.Default with Top = 1.0f %+ 0.0f; Bottom = 1.0f %+ 50.0f } ) )

    member this.Refresh() =
        Gameplay.Chart.recolor()
        renderer <- createRenderer()

    override this.Update(elapsedTime, moved) =
        renderer.Update(elapsedTime, moved)
        base.Update(elapsedTime, moved)

    override this.Draw() =
        fbo.Bind true
        renderer.Draw()
        fbo.Unbind()
        Draw.sprite bounds_placeholder.Bounds Color.White fbo.sprite
        base.Draw()

    override this.Init(parent: Widget) =
        base.Init parent
        renderer.Init this

    member this.Destroy() =
        fbo.Dispose()

type ThemesPage() as this =
    inherit Page()

    let preview = NoteskinPreview 0.5f

    let noteskins = PrettySetting("themes.noteskin", Dummy())
    let refreshNoteskins() =
        options.Noteskin.Value <- Noteskins.Current.id
        noteskins.Child <- 
            Selector(Noteskins.list(), options.Noteskin |> Setting.trigger (fun id -> Noteskins.Current.switch id; preview.Refresh()))
        preview.Refresh()

    let themes = PrettySetting("themes.theme", Dummy())
    let refreshThemes() =
        options.Theme.Value <- Themes.Current.id
        themes.Child <-
            Selector(Themes.list(), options.Theme |> Setting.trigger (fun id -> Themes.Current.switch id; preview.Refresh()))
        preview.Refresh()

    let tryEditNoteskin() =
        let ns = Noteskins.Current.instance
        match ns.Source with
        | Zip (_, Some file) ->
            Menu.ShowPage ( ConfirmPage(Localisation.localiseWith [ns.Config.Name] "options.themes.confirmextractzip", F Noteskins.extractCurrent refreshNoteskins) )
        | Zip (_, None) ->
            Menu.ShowPage ( ConfirmPage(Localisation.localiseWith [ns.Config.Name] "options.themes.confirmextractdefault", F Noteskins.extractCurrent refreshNoteskins) )
        | Folder _ -> Menu.ShowPage( EditNoteskinPage refreshNoteskins )

    let tryEditTheme() =
        let theme = Themes.Current.instance
        match theme.Source with
        | Zip (_, None) ->
            Menu.ShowPage (
                ConfirmPage(
                    Localisation.localiseWith [theme.Config.Name] "options.themes.confirmextractdefault",
                    (fun () -> Themes.createNew(System.Guid.NewGuid().ToString()); refreshThemes())
                )
            )
        | Folder _ -> Menu.ShowPage( EditThemePage refreshThemes )
        | Zip (_, Some file) -> failwith "impossible as user themes are always folders"

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