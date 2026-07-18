namespace Interlude.Features.Import.Osu

open System.IO
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.Conversions.Osu
open Interlude.UI
open Interlude.Options
open Interlude.Content

type ImportOsuNoteskinPage(osu_skin_conversion: OsuSkinConversion, folder_name: string, existing_folder: string option) =
    inherit Page()

    let keymode: Setting<Keymode> = Setting.simple Keymode.``4K``
    let is_arrows: Setting<bool> = Setting.simple false
    let delete_existing: Setting<bool> = Setting.simple false

    override this.Content() =
        page_container()
        |+ PageSetting(%"osu_skin_import.keymode", SelectDropdown.FromEnum(keymode))
            .Pos(0)
        |+ PageSetting(%"osu_skin_import.isarrows", Checkbox is_arrows)
            .Pos(2)
            .Conditional(fun () -> keymode.Value = Keymode.``4K``)
        |+
            match existing_folder with
            | Some folder ->
                [
                    Text([folder] %> "osu_skin_import.delete_prompt")
                        .Align(Alignment.LEFT)
                        .Position(page_position(5, 2, PageWidth.Full).Shrink(Style.PADDING))
                    PageSetting(%"osu_skin_import.delete_existing", Checkbox delete_existing).Pos(7)
                ]
            | None -> []
        |+ PageButton
            .Once(
                %"osu_skin_import.confirm",
                fun () ->
                    try
                        osu_skin_conversion.PerformConversion(
                            Path.Combine(get_game_folder("Skins"), folder_name),
                            int keymode.Value,
                            keymode.Value = Keymode.``4K`` && is_arrows.Value
                        )

                        if delete_existing.Value then
                            try
                                match existing_folder with
                                | Some old_name ->
                                    let skin_path = Path.Combine(get_game_folder("Skins"), old_name)
                                    if Directory.Exists(skin_path) then
                                        Directory.Delete(skin_path, true)
                                | None -> failwith "impossible"
                            with err ->
                                Logging.Error "Error deleting old skin"
                        Skins.load()
                        Skins.selected_noteskin_id.Set(folder_name)
                        Skins.selected_hud_id.Set(folder_name)
                    with err ->
                        Logging.Error "Error while converting to skin: %O" err

                    Menu.Back()
                    Menu.Back()
            )
            .Pos(if existing_folder.IsSome then 10 else 5)
        |+ CalloutCard(
            Callout.Normal
                .Icon(Icons.INFO)
                .Title(%"osu_skin_import.disclaimer.title")
                .Body(%"osu_skin_import.disclaimer.body")
        )
            .Pos(15)
        :> Widget

    override this.Title = osu_skin_conversion.SkinIni.General.Name