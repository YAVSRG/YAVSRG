namespace Interlude.Content

open System
open System.IO
open System.IO.Compression
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skinning
open Prelude.Skinning.HudLayouts
open Interlude

module HUDs =

    type private LoadedHUD =
        {
            Name: string
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
                | TextureOk(img, config) ->
                    available_textures.Add
                        {
                            Label = texture_id
                            Image = img
                            Rows = config.Rows
                            Columns = config.Columns
                            DisposeImageAfter = true
                        }
                | TextureError reason ->
                    if required_textures.Contains texture_id then
                        Logging.Error(
                            sprintf
                                "Problem with HUD texture '%s' in '%s': %s\nIt will appear as a white square ingame."
                                texture_id
                                this.Name
                                reason
                        )

                    missing_textures.Add texture_id
                | TextureNotRequired -> missing_textures.Add texture_id

            let atlas, sprites =
                Sprite.upload_many
                    ("HUD[" + this.Name + "]")
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
            | Some (atlas, sprites) -> Texture.unclaim_texture_unit atlas

        interface IDisposable with
            member this.Dispose() =
                (this.HUD :> IDisposable).Dispose()
                match this.Textures with
                | Some (atlas, sprites) -> 
                    Texture.unclaim_texture_unit atlas
                    sprites |> Array.iter (snd >> Sprite.destroy >> ignore)
                | None -> ()

    let private DEFAULT_FOLDER = "User"

    let mutable private initialised = false
    let private loaded = new Dictionary<string, LoadedHUD>()
    let private _selected_id = Setting.simple <| DEFAULT_FOLDER
    let mutable current = Unchecked.defaultof<HudLayout>

    let save_config (new_config: HudConfig) =
        current.Config <- new_config

    let reload_current () =
        current <- loaded.[_selected_id.Value].HUD
        current.ReloadFromDisk()
        loaded.[_selected_id.Value].Active(true)

    let selected_id =
        Setting.make
            (fun new_id ->
                if initialised then
                    let old_id = _selected_id.Value

                    if not (loaded.ContainsKey new_id) then
                        Logging.Warn("HUD '" + new_id + "' not found, switching to default")
                        _selected_id.Value <- DEFAULT_FOLDER
                    else
                        _selected_id.Value <- new_id

                    if _selected_id.Value <> old_id then

                        if loaded.ContainsKey old_id then
                            loaded.[old_id].Inactive()

                        loaded.[_selected_id.Value].Active(false)

                    current <- loaded.[_selected_id.Value].HUD
                else
                    _selected_id.Value <- new_id
            )
            (fun () -> _selected_id.Value)

    let load () =
        
        for l in loaded.Values do (l :> IDisposable).Dispose()
        loaded.Clear()

        for zip in
            Directory.EnumerateFiles(get_game_folder "HUDs")
            |> Seq.filter (fun p -> Path.GetExtension(p).ToLower() = ".ihl") do
            let target =
                Path.Combine(Path.GetDirectoryName zip, Path.GetFileNameWithoutExtension zip)

            if Directory.Exists(target) then
                Logging.Info(sprintf "%s has already been extracted, deleting" (Path.GetFileName zip))
                File.Delete zip
            else
                Logging.Info(sprintf "Extracting %s to a folder" (Path.GetFileName zip))
                ZipFile.ExtractToDirectory(zip, Path.ChangeExtension(zip, null))
                File.Delete zip

        for source in Directory.EnumerateDirectories(get_game_folder "HUDs") do
            let id = Path.GetFileName source

            try
                let ns = HudLayout.FromPath source

                loaded.Add(
                    id,
                    {
                        Name = id
                        HUD = ns
                        Textures = None
                    }
                )
            with err ->
                Logging.Error("  Failed to load HUD '" + id + "'", err)

        if not (loaded.ContainsKey DEFAULT_FOLDER) then
            loaded.Add(
                DEFAULT_FOLDER,
                {
                    Name = DEFAULT_FOLDER
                    HUD = HudLayout.CreateDefault(Path.Combine(get_game_folder "HUDs", DEFAULT_FOLDER))
                    Textures = None
                }
            )
        
        if not (loaded.ContainsKey _selected_id.Value) then
            Logging.Warn("HUD '" + _selected_id.Value + "' not found, switching to default")
            _selected_id.Value <- DEFAULT_FOLDER

        current <- loaded.[_selected_id.Value].HUD
        loaded.[_selected_id.Value].Active(false)

    let init_window () =
        load ()
        Logging.Info(sprintf "Loaded %i HUD layouts" loaded.Count)
        initialised <- true

    let list () =
        loaded |> Seq.map (fun kvp -> (kvp.Key, kvp.Value.HUD)) |> Array.ofSeq

    let exists = loaded.ContainsKey

    let open_current_folder () =
        match current.Source with
        | Embedded _ -> false
        | Folder f -> open_directory f; true

    let delete_current () =
        match current.Source with
        | Embedded _ -> false
        | Folder f ->
            selected_id.Value <- DEFAULT_FOLDER
            try
                Directory.Delete(f, true)
                load()
                true
            with
            | :? IOException as err ->
                Logging.Error("IO error while deleting HUD", err)
                false
            | _ -> reraise()

    let export_current () =
        match current.Source with
        | Embedded _ -> false
        | Folder f ->
            let name = Path.GetFileName f
            let target = Path.Combine(get_game_folder "Exports", name + ".ihl")

            if current.CompressToZip target then
                open_directory (get_game_folder "Exports")
                true
            else
                false
