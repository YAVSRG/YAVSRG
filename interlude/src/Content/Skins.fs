namespace Interlude.Content

open System
open System.IO
open System.IO.Compression
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skins
open Prelude.Skins.Noteskins
open Prelude.Skins.HudLayouts
open Interlude

type private LoadedSkin =
    {
        Skin: Skin
        Icon: Sprite option
    }

    static member Create(skin: Skin) : LoadedSkin =
        let sprite =
            match skin.GetIcon() with
            | TextureOk(img, columns, rows) ->
                Sprite.upload_one false true
                    {
                        Label = skin.Metadata.Name + "_icon"
                        Rows = rows
                        Columns = columns
                        Image = img
                        DisposeImageAfter = true
                    }
                |> Some
            | TextureNotRequired
            | TextureError _ -> None

        {
            Skin = skin
            Icon = sprite
        }

    interface IDisposable with
        member this.Dispose() =
            match this.Icon with
            | Some s -> Sprite.destroy s |> ignore
            | None -> ()

/// Once a noteskin's textures have been loaded once, they stay in memory
///  Allows swapping skin textures in the next time fast instead of slowly reading it all again
/// Useful because presets can change skins when a chart is selected, this hugely reduces a lag spike when that happens
type private LoadedNoteskin =
    {
        mutable Metadata: SkinMetadata
        Noteskin: Noteskin
        mutable Textures: (Texture * (string * Sprite) array) option
    }

    member this.ReloadAtlas() : Texture * (string * Sprite) array =
        match this.Textures with
        | Some (existing_atlas, existing_sprites) ->
            Texture.unclaim_texture_unit existing_atlas
            for id, s in existing_sprites do
                Sprite.destroy s |> ignore
        | None -> ()

        let missing_textures = ResizeArray()
        let available_textures = ResizeArray()

        let required_textures = Set.ofSeq this.Noteskin.RequiredTextures

        for texture_id in NoteskinTextureRules.list () do
            match this.Noteskin.GetTexture texture_id with
            | TextureOk(img, columns, rows) ->
                available_textures.Add
                    {
                        Label = texture_id
                        Image = img
                        Rows = rows
                        Columns = columns
                        DisposeImageAfter = true
                    }
            | TextureError reason ->
                if required_textures.Contains texture_id then
                    Logging.Error
                        "Problem with noteskin texture '%s' in '%s': %s\nIt will appear as a white square ingame."
                        texture_id
                        this.Metadata.Name
                        reason

                missing_textures.Add texture_id
            | TextureNotRequired -> missing_textures.Add texture_id

        let atlas, sprites =
            Sprite.upload_many
                (this.Metadata.Name + "::Noteskin")
                false
                this.Noteskin.Config.LinearSampling
                (available_textures.ToArray())

        let sprites =
            Array.concat
                [
                    sprites
                    missing_textures
                    |> Seq.map (fun id -> (id, Texture.create_default_sprite atlas))
                    |> Array.ofSeq
                ]

        this.Textures <- Some (atlas, sprites)
        (atlas, sprites)

    /// `force_reload` false: Load textures from disk if not loaded, otherwise fast-swap the existing data in
    /// `force_reload` true: Always reload textures from disk
    member this.Active(force_reload: bool) =
        let atlas, sprites =
            match this.Textures with
            | Some (atlas, sprites) when not force_reload -> (atlas, sprites)
            | _ -> this.ReloadAtlas()
        Texture.claim_texture_unit atlas |> ignore
        for (texture_id, sprite) in sprites do
            Sprites.add false texture_id sprite

    member this.Inactive() =
        match this.Textures with
        | None -> ()
        | Some (atlas, _) -> Texture.unclaim_texture_unit atlas

    interface IDisposable with
        member this.Dispose() =
            if not this.Noteskin.IsEmbedded then
                (this.Noteskin :> IDisposable).Dispose()
                match this.Textures with
                | Some (atlas, sprites) ->
                    Texture.unclaim_texture_unit atlas
                    sprites |> Array.iter (snd >> Sprite.destroy >> ignore)
                | None -> ()

/// Once a HUD's textures have been loaded once, they stay in memory
///  Allows swapping skin textures in the next time fast instead of slowly reading it all again
/// Useful because presets can change skins when a chart is selected, this hugely reduces a lag spike when that happens
type private LoadedHUD =
    {
        mutable Metadata: SkinMetadata
        HUD: HudLayout
        mutable Textures: (Texture * (string * Sprite) array) option
    }

    member this.ReloadAtlas() : Texture * (string * Sprite) array =
        match this.Textures with
        | Some (existing_atlas, existing_sprites) ->
            Texture.unclaim_texture_unit existing_atlas
            for id, s in existing_sprites do
                Sprite.destroy s |> ignore
        | None -> ()

        let missing_textures = ResizeArray()
        let available_textures = ResizeArray()

        let required_textures = Set.ofSeq this.HUD.RequiredTextures

        for texture_id in HudTextureRules.list () do
            match this.HUD.GetTexture texture_id with
            | TextureOk(img, columns, rows) ->
                available_textures.Add
                    {
                        Label = texture_id
                        Image = img
                        Rows = rows
                        Columns = columns
                        DisposeImageAfter = true
                    }
            | TextureError reason ->
                if required_textures.Contains texture_id then
                    Logging.Error
                        "Problem with HUD texture '%s' in '%s': %s\nIt will appear as a white square ingame."
                        texture_id
                        this.Metadata.Name
                        reason

                missing_textures.Add texture_id
            | TextureNotRequired -> missing_textures.Add texture_id

        let atlas, sprites =
            Sprite.upload_many
                (this.Metadata.Name + "::HUD")
                false
                true
                (available_textures.ToArray())

        let sprites =
            Array.concat
                [
                    sprites
                    missing_textures
                    |> Seq.map (fun id -> (id, Texture.create_default_sprite atlas))
                    |> Array.ofSeq
                ]

        this.Textures <- Some (atlas, sprites)
        (atlas, sprites)

    /// `force_reload` false: Load textures from disk if not loaded, otherwise fast-swap the existing data in
    /// `force_reload` true: Always reload textures from disk
    member this.Active(force_reload: bool) =
        let atlas, sprites =
            match this.Textures with
            | Some (atlas, sprites) when not force_reload -> (atlas, sprites)
            | _ -> this.ReloadAtlas()
        Texture.claim_texture_unit atlas |> ignore
        for (texture_id, sprite) in sprites do
            Sprites.add false texture_id sprite

    member this.Inactive() =
        match this.Textures with
        | None -> ()
        | Some (atlas, _) -> Texture.unclaim_texture_unit atlas

    interface IDisposable with
        member this.Dispose() =
            (this.HUD :> IDisposable).Dispose()
            match this.Textures with
            | Some (atlas, sprites) ->
                Texture.unclaim_texture_unit atlas
                sprites |> Array.iter (snd >> Sprite.destroy >> ignore)
            | None -> ()

module Skins =

    let mutable private initialised = false

    let private DEFAULT_NOTESKIN_META =
        {
            Name = "Chocolate"
            Author = "Percyqaz"
            Editor = None
        }

    /// Skin IDs are: *chocolate_default (represents embedded skin/noteskin), or the name of a folder under Skins
    /// Skins consist of a noteskin (optional) and a HUD (optional)
    /// Skin IDs can interchangeably refer to the Skin container folder, the Noteskin within it, or the HUD within it
    let private DEFAULT_NOTESKIN_ID = "*chocolate_default"
    let private DEFAULT_NOTESKIN =
        "chocolate.zip"
        |> Utils.get_resource_stream
        |> Noteskin.FromZipStream
    let mutable private DEFAULT_SKIN_ICON = None

    let private loaded_noteskins = new Dictionary<string, LoadedNoteskin>()
    /// Skin ID representing which skin's noteskin is selected
    let private _selected_noteskin_id = Setting.simple DEFAULT_NOTESKIN_ID
    let mutable current_noteskin = DEFAULT_NOTESKIN
    let mutable current_noteskin_meta = DEFAULT_NOTESKIN_META

    let private DEFAULT_HUD_META =
        {
            Name = "User"
            Author = "Interlude"
            Editor = None
        }
    let private DEFAULT_HUD_FOLDER = "User"

    let private loaded_huds = new Dictionary<string, LoadedHUD>()
    /// Skin ID representing which skin's HUD is selected
    let private _selected_hud_id = Setting.simple DEFAULT_HUD_FOLDER
    let mutable current_hud = Unchecked.defaultof<HudLayout>
    let mutable current_hud_meta = DEFAULT_HUD_META

    let private loaded_skins = new Dictionary<string, LoadedSkin>()

    let load () : unit =

        for noteskin in loaded_noteskins.Values do (noteskin :> IDisposable).Dispose()
        for hud in loaded_huds.Values do (hud :> IDisposable).Dispose()
        for skin in loaded_skins.Values do (skin :> IDisposable).Dispose()
        loaded_noteskins.Clear()
        loaded_huds.Clear()
        loaded_skins.Clear()

        loaded_noteskins.Add(
            DEFAULT_NOTESKIN_ID,
            {
                Metadata = DEFAULT_NOTESKIN_META
                Noteskin = DEFAULT_NOTESKIN
                Textures = None
            }
        )

        if DEFAULT_SKIN_ICON.IsNone then
            DEFAULT_SKIN_ICON <-
                match DEFAULT_NOTESKIN.GetTexture("note") with
                | TextureOk(img, columns, rows) ->
                    Sprite.upload_one false true
                        {
                            Label = "DEFAULT_SKIN_ICON"
                            Rows = rows
                            Columns = columns
                            Image = img
                            DisposeImageAfter = true
                        }
                    |> Some
                | TextureNotRequired
                | TextureError _ as x -> failwithf "%A" x

        // migrate old noteskins folder
        let would_be_skins_folder = Path.Combine(Directory.GetCurrentDirectory(), "Skins")
        let would_be_noteskins_folder = Path.Combine(Directory.GetCurrentDirectory(), "Noteskins")
        if not (Directory.Exists would_be_skins_folder) && Directory.Exists would_be_noteskins_folder then
            Logging.Info("Relocating old Noteskins folder to 'Skins'")
            Directory.Move(would_be_noteskins_folder, would_be_skins_folder)

        // extract any .isk zips
        for zip in
            Directory.EnumerateFiles(get_game_folder "Skins")
            |> Seq.filter (fun p -> Path.GetExtension(p).ToLower() = ".isk") do
            let target = Path.ChangeExtension(zip, null)

            let target =
                if Directory.Exists(target) && Directory.EnumerateFileSystemEntries(target) |> Seq.isEmpty |> not then
                    sprintf "%s_%i" target (Timestamp.now())
                else
                    target

            try
                try Directory.Delete target with _ -> ()
                Logging.Info "Extracting %s to a folder" (Path.GetFileName zip)
                ZipFile.ExtractToDirectory(zip, target)
                File.Delete zip
            with err ->
                Logging.Error "Error extracting noteskin '%s': %s" zip err.Message

        // migrate old noteskins
        for source in Directory.EnumerateDirectories(get_game_folder "Skins") do
            if NoteskinToSkinMigration.folder_should_migrate source then
                match NoteskinToSkinMigration.migrate_folder source with
                | Ok () -> Logging.Info "Migrating noteskin '%s' to new skin format" (Path.GetFileName source)
                | Error err -> Logging.Error "Error migrating noteskin '%s' to new skin format: %O" (Path.GetFileName source) err

        // load all skins and their parts
        for source in Directory.EnumerateDirectories(get_game_folder "Skins") |> Seq.where Skin.Exists do
            let id = Path.GetFileName source

            if id = DEFAULT_NOTESKIN_ID then failwith "How did we get here?"

            match Skin.FromPath source with
            | Ok skin ->

                match skin.NoteskinFolder() with
                | Some noteskin_path ->
                    match Noteskin.FromPath noteskin_path with
                    | Error err -> Logging.Error "  Failed to load noteskin in '%s': %O" id err
                    | Ok noteskin ->
                        loaded_noteskins.Add(
                            id,
                            {
                                Metadata = skin.Metadata
                                Noteskin = noteskin
                                Textures = None
                            }
                        )
                | None -> ()

                match skin.HudFolder() with
                | Some hud_path ->
                    match HudLayout.FromPath hud_path with
                    | Error err -> Logging.Error "  Failed to load HUD in '%s': %O" id err
                    | Ok hud ->
                        loaded_huds.Add(
                            id,
                            {
                                Metadata = skin.Metadata
                                HUD = hud
                                Textures = None
                            }
                        )
                | None -> ()

                loaded_skins.Add (id, LoadedSkin.Create skin)

            | Error err ->
                Logging.Error "  Failed to load skin '%s': %O" id err

        // fallback HUD
        if not (loaded_skins.ContainsKey DEFAULT_HUD_FOLDER) then

            let hud, skin = Skin.CreateDefault DEFAULT_HUD_META (Path.Combine(get_game_folder "Skins", "User"))
            loaded_skins.Add(DEFAULT_HUD_FOLDER, LoadedSkin.Create skin)
            loaded_huds.Add(DEFAULT_HUD_FOLDER, { Metadata = skin.Metadata; HUD = hud; Textures = None })

        elif not (loaded_huds.ContainsKey DEFAULT_HUD_FOLDER) then

            let skin = loaded_skins.[DEFAULT_HUD_FOLDER]
            let hud, _ = Skin.CreateDefault DEFAULT_HUD_META (Path.Combine(get_game_folder "Skins", "User"))
            loaded_skins.Add(DEFAULT_HUD_FOLDER, skin)
            loaded_huds.Add(DEFAULT_HUD_FOLDER, { Metadata = skin.Skin.Metadata; HUD = hud; Textures = None })

        // ensure selected IDs exist
        if not (loaded_noteskins.ContainsKey _selected_noteskin_id.Value) then
            Logging.Warn "Noteskin '%s' not found, switching to default" _selected_noteskin_id.Value
            _selected_noteskin_id.Value <- DEFAULT_NOTESKIN_ID
        current_noteskin <- loaded_noteskins.[_selected_noteskin_id.Value].Noteskin
        current_noteskin_meta <- loaded_noteskins.[_selected_noteskin_id.Value].Metadata
        loaded_noteskins.[_selected_noteskin_id.Value].Active(false)

        if not (loaded_huds.ContainsKey _selected_hud_id.Value) then
            Logging.Warn "HUD '%s' not found, switching to default" _selected_hud_id.Value
            _selected_hud_id.Value <- DEFAULT_HUD_FOLDER
        current_hud <- loaded_huds.[_selected_hud_id.Value].HUD
        current_hud_meta <- loaded_huds.[_selected_hud_id.Value].Metadata
        loaded_huds.[_selected_hud_id.Value].Active(false)

    let reload_current_noteskin () : unit =
        current_noteskin <- loaded_noteskins.[_selected_noteskin_id.Value].Noteskin
        current_noteskin_meta <- loaded_noteskins.[_selected_noteskin_id.Value].Metadata
        current_noteskin.ReloadFromDisk()
        loaded_noteskins.[_selected_noteskin_id.Value].Active(true)

    let reload_current_hud () : unit =
        current_hud <- loaded_huds.[_selected_hud_id.Value].HUD
        current_hud_meta <- loaded_huds.[_selected_hud_id.Value].Metadata
        current_hud.ReloadFromDisk()
        loaded_huds.[_selected_hud_id.Value].Active(true)

    let selected_hud_id : Setting<string> =
        Setting.make
            (fun new_id ->
                if initialised then
                    let old_id = _selected_hud_id.Value

                    if not (loaded_huds.ContainsKey new_id) then
                        Logging.Warn "HUD '%s' not found, switching to default" new_id
                        _selected_hud_id.Value <- DEFAULT_HUD_FOLDER
                    else
                        _selected_hud_id.Value <- new_id

                    if _selected_hud_id.Value <> old_id then

                        if loaded_huds.ContainsKey old_id then
                            loaded_huds.[old_id].Inactive()

                        loaded_huds.[_selected_hud_id.Value].Active(false)

                    current_hud <- loaded_huds.[_selected_hud_id.Value].HUD
                    current_hud_meta <- loaded_huds.[_selected_hud_id.Value].Metadata
                else
                    _selected_hud_id.Value <- new_id
            )
            (fun () -> _selected_hud_id.Value)

    let selected_noteskin_id : Setting<string> =
        Setting.make
            (fun new_id ->
                if initialised then
                    let old_id = _selected_noteskin_id.Value

                    if not (loaded_noteskins.ContainsKey new_id) then
                        Logging.Warn "Noteskin '%s' not found, switching to default" new_id
                        _selected_noteskin_id.Value <- DEFAULT_NOTESKIN_ID
                    else
                        _selected_noteskin_id.Value <- new_id

                    if _selected_noteskin_id.Value <> old_id then

                        if loaded_noteskins.ContainsKey old_id then
                            loaded_noteskins.[old_id].Inactive()

                        loaded_noteskins.[_selected_noteskin_id.Value].Active(false)

                    current_noteskin <- loaded_noteskins.[_selected_noteskin_id.Value].Noteskin
                    current_noteskin_meta <- loaded_noteskins.[_selected_noteskin_id.Value].Metadata
                else
                    _selected_noteskin_id.Value <- new_id
            )
            (fun () -> _selected_noteskin_id.Value)

    let init () =
        load ()
        Logging.Debug "Loaded %i skins" loaded_skins.Count
        initialised <- true

    let get_icon (skin_id: string) : Sprite option =
        if skin_id = DEFAULT_NOTESKIN_ID then
            DEFAULT_SKIN_ICON
        elif loaded_skins.ContainsKey skin_id then
            loaded_skins.[skin_id].Icon
        else None

    let save_noteskin_config (new_config: NoteskinConfig) : unit =
        if not current_noteskin.IsEmbedded then
            current_noteskin.Config <- new_config

    let save_hud_config (new_config: HudConfig) : unit =
        current_hud.Config <- new_config

    let save_skin_meta (id: string) (new_meta: SkinMetadata) : unit =
        if loaded_skins.ContainsKey id then
            loaded_skins.[id].Skin.Metadata <- new_meta

            if loaded_noteskins.ContainsKey id then
                loaded_noteskins.[id].Metadata <- new_meta
                if id = selected_noteskin_id.Value then
                    current_noteskin_meta <- new_meta

            if loaded_huds.ContainsKey id then
                loaded_huds.[id].Metadata <- new_meta
                if id = selected_hud_id.Value then
                    current_hud_meta <- new_meta

    let create_user_noteskin_from_default (username: string option) : bool =

        let noteskin_name =
            match username with
            | Some u -> sprintf "%s's skin" u
            | None -> "Your skin"

        let can_go_in_user_folder = loaded_skins.[DEFAULT_HUD_FOLDER].Skin.NoteskinFolder().IsNone
        let id =
            if can_go_in_user_folder then
                DEFAULT_HUD_FOLDER
            else
                Text.RegularExpressions.Regex(@"[^a-zA-Z0-9_\-'\s]").Replace(noteskin_name, "")
                + "-" + System.DateTime.Now.ToString("ddMMyyyyHHmmss")

        let target_skin_config = Path.Combine(get_game_folder "Skins", id, "skin.json")
        let target_noteskin_folder = Path.Combine(get_game_folder "Skins", id, "Noteskin")

        DEFAULT_NOTESKIN.ExtractToFolder(target_noteskin_folder) |> ignore
        JSON.ToFile(target_skin_config, true) { DEFAULT_NOTESKIN_META with Editor = Option.defaultValue "Unknown" username |> Some }
        load ()
        selected_noteskin_id.Value <- id
        true

    let open_noteskin_folder (id: string) : bool =
        if loaded_noteskins.ContainsKey id then
            match loaded_noteskins.[id].Noteskin.Source with
            | Embedded _ -> false
            | Folder f -> open_directory f; true
        else false

    let open_hud_folder (id: string) : bool =
        if loaded_huds.ContainsKey id then
            match loaded_huds.[id].HUD.Source with
            | Embedded _ -> false
            | Folder f -> open_directory f; true
        else false

    let open_skin_folder (id: string) : bool =
        if loaded_skins.ContainsKey id then
            match loaded_skins.[id].Skin.Source with
            | Embedded _ -> false
            | Folder f -> open_directory f; true
        else false

    let delete_noteskin (id: string) : bool =
        if loaded_noteskins.ContainsKey id then
            match loaded_noteskins.[id].Noteskin.Source with
            | Embedded _ -> false
            | Folder f ->
                if selected_noteskin_id.Value = id then selected_noteskin_id.Value <- DEFAULT_NOTESKIN_ID
                try
                    Directory.Delete(f, true)
                    if not (loaded_huds.ContainsKey id) then
                        Directory.Delete(Path.Combine(get_game_folder "Skins", id), true)
                    load()
                    true
                with
                | :? IOException as err ->
                    Logging.Error "IO error while deleting noteskin: %O" err
                    false
                | _ -> reraise()
        else false

    let delete_hud (id: string) : bool =
        if loaded_huds.ContainsKey id then
            match loaded_huds.[id].HUD.Source with
            | Embedded _ -> false
            | Folder f ->
                if selected_hud_id.Value = id then selected_hud_id.Value <- DEFAULT_NOTESKIN_ID
                try
                    Directory.Delete(f, true)
                    if not (loaded_noteskins.ContainsKey id) then
                        Directory.Delete(Path.Combine(get_game_folder "Skins", id), true)
                    load()
                    true
                with
                | :? IOException as err ->
                    Logging.Error "IO error while deleting hud: %O" err
                    false
                | _ -> reraise()
        else false

    let export_skin (id: string) : bool =
        if loaded_skins.ContainsKey id then
            let file_name =
                let original_name = loaded_skins.[id].Skin.Metadata.Name
                let file_safe_name = Text.RegularExpressions.Regex(@"[^a-zA-Z0-9_\-'\s]").Replace(original_name, "")
                if file_safe_name = "" then id + ".isk" else file_safe_name + ".isk"
            match loaded_skins.[id].Skin.Source with
            | Embedded _ -> false
            | Folder f ->
                let target = Path.Combine(get_game_folder "Exports", file_name)

                if loaded_skins.[id].Skin.CompressToZip target then
                    open_directory (get_game_folder "Exports")
                    true
                else
                    false
        else false

    /// Each skin ID (where that skin has a noteskin), its noteskin and the metadata of the containing skin, in no particular order
    let list_noteskins () : (string * Noteskin * SkinMetadata) seq =
        seq {
            for noteskin in loaded_noteskins do
                yield noteskin.Key, noteskin.Value.Noteskin, noteskin.Value.Metadata
        }
    let noteskin_exists = loaded_noteskins.ContainsKey

    /// Each skin ID (where that skin has a HUD), its HUD and the metadata of the containing skin, in no particular order
    let list_huds () : (string * HudLayout * SkinMetadata) seq =
        seq {
            for hud in loaded_huds do
                yield hud.Key, hud.Value.HUD, hud.Value.Metadata
        }

    let hud_exists (id: string) : bool = loaded_huds.ContainsKey id