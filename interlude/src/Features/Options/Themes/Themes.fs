namespace Interlude.Features.OptionsMenu.Themes

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Content
open Interlude.Content
open Interlude.Utils
open Interlude.Options
open Interlude.UI.Menu

type ThemesPage() as this =
    inherit Page()

    let preview = NoteskinPreview 0.5f

    let noteskins = PageSetting("themes.noteskin", Dummy())
    let themes = PageSetting("themes.theme", Dummy())

    let refresh() =

        options.Noteskin.Value <- Noteskins.Current.id
        noteskins.Child <- 
            Selector(Noteskins.list(), options.Noteskin |> Setting.trigger (fun id -> Noteskins.Current.switch id; preview.Refresh()))

        options.Theme.Value <- Themes.Current.id
        themes.Child <-
            Selector(Themes.list(), options.Theme |> Setting.trigger (fun id -> Themes.Current.switch id; preview.Refresh()))
        preview.Refresh()

    let tryEditNoteskin() =
        let ns = Noteskins.Current.instance
        match ns.Source with
        | Zip (_, Some file) ->
            ConfirmPage(
                Localisation.localiseWith [ns.Config.Name] "themes.confirmextractzip",
                (fun () -> 
                    if Noteskins.extractCurrent() then ()
                    else Logging.Error "Noteskin folder already exists"
                )).Show()
        | Zip (_, None) ->
            ConfirmPage(
                Localisation.localiseWith [ns.Config.Name] "themes.confirmextractdefault", 
                (fun () -> 
                    if Noteskins.extractCurrent() then ()
                    else Logging.Error "Noteskin folder already exists"
                )).Show()
        | Folder _ -> Menu.ShowPage EditNoteskinPage

    let tryEditTheme() =
        let theme = Themes.Current.instance
        match theme.Source with
        | Zip (_, None) ->
            ConfirmPage(
                Localisation.localiseWith [theme.Config.Name] "themes.confirmextractdefault",
                (fun () -> 
                    if Themes.createNew(theme.Config.Name + "_extracted") then () 
                    else Logging.Error "Theme folder already exists"
                )
            ).Show()
        | Folder _ -> EditThemePage().Show()
        | Zip (_, Some file) -> failwith "impossible as user themes are always folders"

    do
        refresh()
            
        this.Content(
            column()
            |+ themes
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("themes.theme"))
            |+ PageButton("themes.edittheme", tryEditTheme)
                .Pos(270.0f)
                .Tooltip(Tooltip.Info("themes.edittheme"))
            |+ PageButton("themes.showthemesfolder", fun () -> openDirectory (getDataPath "Themes"))
                .Pos(340.0f)
                .Tooltip(Tooltip.Info("themes.showthemesfolder"))

            |+ noteskins
                .Pos(500.0f)
                .Tooltip(Tooltip.Info("themes.noteskin"))
            |+ PageButton("themes.editnoteskin", tryEditNoteskin)
                .Pos(570.0f)
                .Tooltip(Tooltip.Info("themes.editnoteskin"))
            |+ PageButton("themes.shownoteskinsfolder", fun () -> openDirectory (getDataPath "Noteskins"))
                .Pos(640.0f)
                .Tooltip(Tooltip.Info("themes.shownoteskinsfolder"))
            |+ preview
        )

    override this.OnClose() = ()
    override this.OnDestroy() = preview.Destroy()
    override this.OnReturnTo() = refresh()
    override this.Title = L"themes.name"