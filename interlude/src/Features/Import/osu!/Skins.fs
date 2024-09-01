namespace Interlude.Features.Import.osu

open System.IO
open System.Text.RegularExpressions
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.Conversions.osu
open Prelude.Data.Library
open Interlude.UI
open Interlude.Options
open Interlude.Content

module Skins =

    type ImportOsuNoteskinPage(ini: SkinIni, source_path: string, folder_name: string, existing_folder: string option) =
        inherit Page()

        let keymode: Setting<Keymode> = Setting.simple Keymode.``4K``
        let is_arrows: Setting<bool> = Setting.simple false
        let delete_existing: Setting<bool> = Setting.simple false

        override this.Content() =
            page_container()
            |+ PageSetting(%"osu_skin_import.keymode", Selector.FromEnum(keymode))
                .Pos(0)
            |+ PageSetting(%"osu_skin_import.isarrows", Checkbox is_arrows)
                .Pos(2)
                .Conditional(fun () -> keymode.Value = Keymode.``4K``)
            |+ 
                match existing_folder with
                | Some folder ->
                    [
                        Text([folder] %> "osu_skin_import.delete_prompt", Align = Alignment.LEFT, Position = pretty_pos(5, 2, PageWidth.Full).Shrink(Style.PADDING))
                        PageSetting(%"osu_skin_import.delete_existing", Checkbox delete_existing).Pos(7)
                    ]
                | None -> []
            |+ PageButton
                .Once(
                    %"osu_skin_import.confirm",
                    fun () ->
                        try
                            OsuSkinConverter.convert_to_skin
                                ini
                                source_path
                                (Path.Combine(get_game_folder "Skins", folder_name))
                                (int keymode.Value)
                                (keymode.Value = Keymode.``4K`` && is_arrows.Value)

                            if delete_existing.Value then 
                                try
                                    match existing_folder with
                                    | Some old_name ->
                                        let skin_path = Path.Combine(get_game_folder "Skins", old_name)
                                        if Directory.Exists skin_path then
                                            Directory.Delete(skin_path, true)
                                    | None -> failwith "impossible"
                                with err ->
                                    Logging.Error("Error deleting old skin")
                            Skins.load ()
                            Skins.selected_noteskin_id.Set folder_name
                            Skins.selected_hud_id.Set folder_name
                        with err ->
                            Logging.Error("Error while converting to skin", err)

                        Menu.Back()
                        Menu.Back()
                )
                .Pos(if existing_folder.IsSome then 10 else 5)
            |+ Callout.frame
                (Callout.Normal
                    .Icon(Icons.INFO)
                    .Title(%"osu_skin_import.disclaimer.title")
                    .Body(%"osu_skin_import.disclaimer.body")
                )
                (fun (w, h) -> pretty_pos(15, PAGE_BOTTOM - 15, PageWidth.Custom w))
            :> Widget

        override this.Title = ini.General.Name
        override this.OnClose() = ()

    let import_osu_skin (path: string) =
        let id = Regex("[^a-zA-Z0-9_-]").Replace(Path.GetFileName(path), "")
        let timestamp = "-" + System.DateTime.Now.ToString("ddMMyyyyHHmmss")

        match OsuSkinConverter.check_before_convert path with
        | Ok ini ->
            let existing_id = Skins.list_noteskins() |> Seq.map (fun (id, _, _) -> id) |> Seq.tryFind (fun x -> x.StartsWith id)
            ImportOsuNoteskinPage(
                ini,
                path,
                id + timestamp,
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

        override this.Content() =
            if List.isEmpty osu_skin_paths then
                Container(NodeType.Leaf, Position = Position.Shrink(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))
                |+ EmptyState(Icons.IMAGE, %"osu_skin_import.no_skins_found")
                :> Widget
            else
                ScrollContainer(
                    FlowContainer.Vertical<_>(PRETTYHEIGHT)
                    |+ seq {
                        for path in osu_skin_paths do
                            yield PageButton(
                                Path.GetFileName(path), 
                                fun () -> import_osu_skin(path)
                            )
                    },
                    Position = Position.Shrink(PRETTY_MARGIN_X, PRETTY_MARGIN_Y)
                ) :> Widget

        override this.OnClose() = ()
        override this.Title = %"skins.import_from_osu"