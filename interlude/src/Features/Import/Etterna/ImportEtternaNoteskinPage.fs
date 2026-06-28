namespace Interlude.Features.Import.Etterna

open System.IO
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.Conversions.Stepmania
open Interlude.UI
open Interlude.Content

type ImportEtternaNoteskinPage(source_path: string, folder_name: string, existing_folder: string option) =
    inherit Page()

    let is_arrows: Setting<bool> = Setting.simple false
    let delete_existing: Setting<bool> = Setting.simple false

    override this.Content() =
        page_container()
            .With(
                PageSetting(%"etterna_skin_import.isarrows", Checkbox is_arrows)
                    .Pos(2)
            )
            .WithConditional(
                existing_folder.IsSome,

                Text([existing_folder |> Option.defaultValue ""] %> "etterna_skin_import.delete_prompt")
                    .Align(Alignment.LEFT)
                    .TextPos(5),
                PageSetting(%"etterna_skin_import.delete_existing", Checkbox delete_existing)
                    .Pos(7)
            )
            .With(
                PageButton.Once(%"etterna_skin_import.confirm", fun () ->
                    try
                        StepmaniaSkinConverter.convert_to_skin
                            source_path
                            (Path.Combine(get_game_folder "Skins", folder_name))
                            is_arrows.Value

                        if delete_existing.Value then
                            try
                                match existing_folder with
                                | Some old_name ->
                                    let skin_path = Path.Combine(get_game_folder "Skins", old_name)
                                    if Directory.Exists skin_path then
                                        Directory.Delete(skin_path, true)
                                | None -> failwith "impossible"
                            with err ->
                                Logging.Error "Error deleting old skin"
                        Skins.load ()
                        Skins.selected_noteskin_id.Set folder_name
                        Skins.selected_hud_id.Set folder_name
                    with err ->
                        Logging.Error "Error while converting to skin: %O" err

                    Menu.Back()
                )
                    .Pos(if existing_folder.IsSome then 10 else 5),

                CalloutCard(
                    Callout.Normal
                        .Icon(Icons.INFO)
                        .Title(%"etterna_skin_import.disclaimer.title")
                        .Body(%"etterna_skin_import.disclaimer.body")
                )
                    .Pos(15)
            )

    override this.Title = Path.GetFileName source_path