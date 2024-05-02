namespace Interlude.Features.Import.FromOsu

open System.IO
open System.Text.RegularExpressions
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skinning.Noteskins.Conversion
open Prelude.Data.Library
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Options
open Interlude.Content

module ImportSkins =

    type ImportOsuNoteskinPage(ini: SkinIni, source_path: string, target_path: string, existing_folder: string option) =
        inherit Page()

        let keymode: Setting<Keymode> = Setting.simple Keymode.``4K``
        let is_arrows: Setting<bool> = Setting.simple false
        let delete_existing: Setting<bool> = Setting.simple false

        override this.Init(parent: Widget) =
            page_container()
            |+ PageSetting("osu_skin_import.keymode", SelectDropdown.FromEnum(keymode))
                .Pos(0)
            |+ Conditional(
                (fun () -> keymode.Value = Keymode.``4K``),
                PageSetting("osu_skin_import.isarrows", Checkbox is_arrows)
                    .Pos(2)
            )
            |+ 
                match existing_folder with
                | Some folder ->
                    [
                        Text([folder] %> "osu_skin_import.delete_prompt", Align = Alignment.LEFT, Position = pretty_pos(5, 2, PageWidth.Full).Margin(Style.PADDING))
                        PageSetting("osu_skin_import.delete_existing", Checkbox delete_existing).Pos(7)
                    ]
                | None -> []
            |+ PageButton
                .Once(
                    "osu_skin_import.confirm",
                    fun () ->
                        try
                            OsuSkinConverter.convert
                                ini
                                source_path
                                target_path
                                (int keymode.Value)
                                (keymode.Value = Keymode.``4K`` && is_arrows.Value)
                            if delete_existing.Value then 
                                try
                                    Directory.Delete(Path.Combine(get_game_folder "Noteskins", existing_folder.Value), true)
                                with err ->
                                    Logging.Error("Error deleting old noteskin")
                            Noteskins.load ()
                            Noteskins.selected_id.Set (Path.GetFileName(target_path))
                        with err ->
                            Logging.Error("Error while converting to noteskin", err)

                        Menu.Exit()
                )
                .Pos(if existing_folder.IsSome then 10 else 5)
            |+ Callout.frame
                (Callout.Normal
                    .Icon(Icons.INFO)
                    .Title(%"osu_skin_import.disclaimer.title")
                    .Body(%"osu_skin_import.disclaimer.body")
                )
                (fun (w, h) -> pretty_pos(15, PAGE_BOTTOM - 15, PageWidth.Custom w))
            |> this.Content

            base.Init parent

        override this.Title = ini.General.Name
        override this.OnClose() = ()

    let import_osu_noteskin (path: string) =
        let id = Regex("[^a-zA-Z0-9_-]").Replace(Path.GetFileName(path), "")

        match OsuSkinConverter.check_before_convert path with
        | Ok ini ->
            let existing_id = Noteskins.list() |> Seq.map fst |> Seq.tryFind (fun x -> x.StartsWith id)
            ImportOsuNoteskinPage(
                ini,
                path,
                Path.Combine(get_game_folder "Noteskins", id + "-" + System.DateTime.Now.ToString("ddMMyyyyHHmmss")),
                existing_id
            )
                .Show()
        | Error err -> 
            Logging.Error("Error while parsing osu! skin.ini\n" + err)
            Notifications.error(%"notification.skin_ini_parse_failed.title", %"notification.skin_ini_parse_failed.body")

    type OsuSkinsListPage() =
        inherit Page()

        let osu_song_folder =
            match options.OsuMount.Value with
            | Some mount -> mount.SourceFolder
            | None -> Imports.OSU_SONG_FOLDER
        let osu_skin_folder =
            Path.Combine(Path.GetDirectoryName(osu_song_folder), "Skins")

        let osu_skin_paths =
            if Directory.Exists(osu_skin_folder) then
                Directory.EnumerateDirectories(osu_skin_folder)
                |> List.ofSeq
            else []

        override this.Init(parent) =
            if List.isEmpty osu_skin_paths then
                Container(NodeType.Leaf, Position = Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))
                |+ EmptyState(Icons.IMAGE, %"osu_skin_import.no_skins_found")
                :> Widget
            else
                ScrollContainer(
                    FlowContainer.Vertical<_>(PRETTYHEIGHT)
                    |+ seq {
                        for path in osu_skin_paths do
                            yield PageButton(System.String.Empty, 
                                fun () ->
                                    import_osu_noteskin(path)
                                , Text = Path.GetFileName(path)
                            )
                    },
                    Position = Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y)
                ) :> Widget
            |> this.Content
            base.Init(parent)

        override this.OnClose() = ()
        override this.Title = %"osu_skin_import.list_skins"