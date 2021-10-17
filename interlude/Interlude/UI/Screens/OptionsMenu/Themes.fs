namespace Interlude.UI.OptionsMenu

open Prelude.Gameplay.NoteColors
open Prelude.Common
open Interlude
open Interlude.Graphics
open Interlude.Options
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Controls
open Interlude.UI.Components.Selection.Compound
open Interlude.UI.Components.Selection.Menu

module Themes =

    let themeChanger refresh : SelectionPage =
        Themes.refreshAvailableThemes()
        {
            Content = fun add ->
                column [
                    PrettySetting("ChooseTheme",
                        ListOrderedSelect.ListOrderedSelector(
                            Setting.make
                                ( fun v ->
                                    options.EnabledThemes.Clear()
                                    options.EnabledThemes.AddRange(v)
                                    Themes.loadThemes(options.EnabledThemes)
                                    Themes.changeNoteSkin(options.NoteSkin.Value)
                                    refresh()
                                )
                                (fun () -> options.EnabledThemes),
                            Themes.availableThemes
                        )
                    ).Position(200.0f, PRETTYWIDTH, 500.0f)
                    Divider().Position(750.0f)
                    PrettyButton("OpenThemeFolder",
                        fun () ->
                            //todo: move this to utils
                            let target = System.Diagnostics.ProcessStartInfo("file://" + System.IO.Path.GetFullPath(getDataPath "Themes"), UseShellExecute = true)
                            System.Diagnostics.Process.Start target |> ignore).Position(800.0f)
                    PrettyButton("NewTheme", fun () -> Dialog.add <| TextInputDialog(Render.bounds, "Enter theme name", Themes.createNew)).Position(900.0f)
                ] :> Selectable
            Callback = refresh
        }

    let icon = "✎"
    let page() : SelectionPage =
        let keycount = Setting.simple options.KeymodePreference.Value
        
        let g keycount i =
            let k = if options.ColorStyle.Value.UseGlobalColors then 0 else int keycount - 2
            Setting.make
                (fun v -> options.ColorStyle.Value.Colors.[k].[i] <- v)
                (fun () -> options.ColorStyle.Value.Colors.[k].[i])

        let colors, refreshColors =
            refreshRow
                (fun () -> colorCount (int keycount.Value) options.ColorStyle.Value.Style)
                (fun i k ->
                    let x = -60.0f * float32 k
                    let n = float32 i
                    ColorPicker(g keycount.Value i)
                    |> positionWidget(x + 120.0f * n, 0.5f, 0.0f, 0.0f, x + 120.0f * n + 120.0f, 0.5f, 0.0f, 1.0f))

        let noteskins = PrettySetting("Noteskin", Selectable())
        let refreshNoteskins() =
            let ns = Themes.noteskins() |> Seq.toArray
            let ids = ns |> Array.map fst
            let names = ns |> Array.map (fun (id, data) -> data.Config.Name)
            options.NoteSkin.Value <- Themes.currentNoteSkin
            Selector.FromArray(names, ids, options.NoteSkin |> Setting.trigger (fun id -> Themes.changeNoteSkin id; refreshColors()))
            |> noteskins.Refresh
        refreshNoteskins()

        {
            Content = fun add ->
                column [
                    PrettyButton("ChangeTheme", fun () -> add ("ChangeTheme", themeChanger(fun () -> refreshColors(); refreshNoteskins()))).Position(200.0f)
                    PrettyButton("EditTheme", ignore).Position(300.0f)
                    PrettySetting("Keymode",
                        Selector.FromEnum<Keymode>(keycount |> Setting.trigger (ignore >> refreshColors))
                    ).Position(450.0f)
                    PrettySetting(
                        "ColorStyle",
                        Selector.FromEnum(
                            Setting.make
                                (fun v -> Setting.app (fun x -> { x with Style = v }) options.ColorStyle)
                                (fun () -> options.ColorStyle.Value.Style)
                            |> Setting.trigger (ignore >> refreshColors))
                    ).Position(550.0f)
                    PrettySetting("NoteColors", colors).Position(650.0f, Render.vwidth - 200.0f, 120.0f)
                    noteskins.Position(800.0f)
                    PrettyButton("EditNoteskin", ignore).Position(900.0f)
                ] :> Selectable
            Callback = ignore
        }
