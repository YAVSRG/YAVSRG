namespace Interlude.Features.OptionsMenu.Themes

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Themes
open Interlude.Content
open Interlude.Utils
open Interlude.Options
open Interlude.UI.Menu

type ThemesPage() as this =
    inherit Page()

    let preview = NoteskinPreview 0.5f

    let noteskins = PrettySetting("themes.noteskin", Dummy())
    let themes = PrettySetting("themes.theme", Dummy())

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
            ConfirmPage(Localisation.localiseWith [ns.Config.Name] "options.themes.confirmextractzip", Noteskins.extractCurrent).Show()
        | Zip (_, None) ->
            ConfirmPage(Localisation.localiseWith [ns.Config.Name] "options.themes.confirmextractdefault", Noteskins.extractCurrent).Show()
        | Folder _ -> Menu.ShowPage EditNoteskinPage

    let tryEditTheme() =
        let theme = Themes.Current.instance
        match theme.Source with
        | Zip (_, None) ->
            ConfirmPage(
                Localisation.localiseWith [theme.Config.Name] "options.themes.confirmextractdefault",
                (fun () -> Themes.createNew(System.Guid.NewGuid().ToString()))
            ).Show()
        | Folder _ -> EditThemePage().Show()
        | Zip (_, Some file) -> failwith "impossible as user themes are always folders"

    do
        refresh()
            
        this.Content(
            column()
            |+ themes
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("options.themes.theme"))
            |+ PrettyButton("themes.edittheme", tryEditTheme)
                .Pos(270.0f)
                .Tooltip(Tooltip.Info("options.themes.edittheme"))
            |+ PrettyButton("themes.showthemesfolder", fun () -> openDirectory (getDataPath "Themes"))
                .Pos(340.0f)
                .Tooltip(Tooltip.Info("options.themes.showthemesfolder"))

            |+ noteskins
                .Pos(500.0f)
                .Tooltip(Tooltip.Info("options.themes.noteskin"))
            |+ PrettyButton("themes.editnoteskin", tryEditNoteskin)
                .Pos(570.0f)
                .Tooltip(Tooltip.Info("options.themes.editnoteskin"))
            |+ PrettyButton("themes.shownoteskinsfolder", fun () -> openDirectory (getDataPath "Noteskins"))
                .Pos(640.0f)
                .Tooltip(Tooltip.Info("options.themes.shownoteskinsfolder"))
            |+ preview
        )

    override this.OnClose() = ()
    override this.OnDestroy() = preview.Destroy()
    override this.OnReturnTo() = refresh()
    override this.Title = L"options.themes.name"