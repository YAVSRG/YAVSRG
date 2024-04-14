namespace Interlude.Content

open System
open System.IO
open System.IO.Compression
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Content
open Prelude.Content.Noteskins
open Interlude

module Noteskins =

    type private LoadedNoteskin =
        {
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
                                "Problem with noteskin texture '%s' in '%s': %s\nIt will appear as a white square ingame."
                                texture_id
                                this.Noteskin.Config.Name
                                reason
                        )

                    missing_textures.Add texture_id
                | TextureNotRequired -> missing_textures.Add texture_id

            let atlas, sprites =
                Sprite.upload_many
                    ("NOTESKIN[" + this.Noteskin.Config.Name + "]")
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
                if not this.Noteskin.IsEmbedded then (this.Noteskin :> IDisposable).Dispose()
                match this.Textures with
                | Some (atlas, sprites) -> 
                    Texture.unclaim_texture_unit atlas
                    sprites |> Array.iter (snd >> Sprite.destroy >> ignore)
                | None -> ()

    let private DEFAULTS =
        let skins = [ "defaultBar.isk"; "defaultArrow.isk"; "defaultOrb.isk" ]

        skins
        |> List.map Utils.get_resource_stream
        |> List.map Noteskin.FromZipStream
        |> List.zip (List.map (fun s -> "*" + s) skins)

    let mutable private initialised = false
    let private loaded = new Dictionary<string, LoadedNoteskin>()
    let private _selected_id = Setting.simple <| fst DEFAULTS.[0]
    let mutable current = snd DEFAULTS.[0]

    let save_config (new_config: NoteskinConfig) =
        current.Config <- new_config

    let save_hud_config (new_hud: HUDNoteskinOptions) =
        current.Config <- { current.Config with HUD = new_hud }

    let reload_current () =
        current <- loaded.[_selected_id.Value].Noteskin
        current.ReloadFromDisk()
        loaded.[_selected_id.Value].Active(true)

    let selected_id =
        Setting.make
            (fun new_id ->
                if initialised then
                    let old_id = _selected_id.Value

                    if not (loaded.ContainsKey new_id) then
                        Logging.Warn("Noteskin '" + new_id + "' not found, switching to default")
                        _selected_id.Value <- fst DEFAULTS.[0]
                    else
                        _selected_id.Value <- new_id

                    if _selected_id.Value <> old_id then

                        if loaded.ContainsKey old_id then
                            loaded.[old_id].Inactive()

                        loaded.[_selected_id.Value].Active(false)

                    current <- loaded.[_selected_id.Value].Noteskin
                else
                    _selected_id.Value <- new_id
            )
            (fun () -> _selected_id.Value)

    let load () =
        
        for l in loaded.Values do (l :> IDisposable).Dispose()
        loaded.Clear()

        for (id, ns) in DEFAULTS do
            loaded.Add(
                id,
                {
                    Noteskin = ns
                    Textures = None
                }
            )

        for zip in
            Directory.EnumerateFiles(get_game_folder "Noteskins")
            |> Seq.filter (fun p -> Path.GetExtension(p).ToLower() = ".isk") do
            let target =
                Path.Combine(Path.GetDirectoryName zip, Path.GetFileNameWithoutExtension zip)

            if Directory.Exists(target) then
                Logging.Info(sprintf "%s has already been extracted, deleting" (Path.GetFileName zip))
                File.Delete zip
            else
                Logging.Info(sprintf "Extracting %s to a folder" (Path.GetFileName zip))
                ZipFile.ExtractToDirectory(zip, Path.ChangeExtension(zip, null))
                File.Delete zip

        for source in Directory.EnumerateDirectories(get_game_folder "Noteskins") do
            let id = Path.GetFileName source

            try
                let ns = Noteskin.FromPath source

                loaded.Add(
                    id,
                    {
                        Noteskin = ns
                        Textures = None
                    }
                )
            with err ->
                Logging.Error("  Failed to load noteskin '" + id + "'", err)
        
        if not (loaded.ContainsKey _selected_id.Value) then
            Logging.Warn("Noteskin '" + _selected_id.Value + "' not found, switching to default")
            _selected_id.Value <- fst DEFAULTS.[0]

        current <- loaded.[_selected_id.Value].Noteskin
        loaded.[_selected_id.Value].Active(false)

    let init_window () =
        load ()
        Logging.Info(sprintf "Loaded %i noteskins. (%i by default)" loaded.Count DEFAULTS.Length)
        initialised <- true

    let list () =
        loaded |> Seq.map (fun kvp -> (kvp.Key, kvp.Value.Noteskin)) |> Array.ofSeq

    let exists = loaded.ContainsKey

    let create_from_embedded (username: string option) : bool =
        if not current.IsEmbedded then
            failwith "Current skin must be an embedded default"

        let skin_name =
            match username with
            | Some u -> sprintf "%s's skin" u
            | None -> "Your skin"

        let id = Text.RegularExpressions.Regex("[^a-zA-Z0-9_-]").Replace(skin_name, "")
        let target = Path.Combine(get_game_folder "Noteskins", id)

        if id <> "" && not (Directory.Exists target) then
            current.ExtractToFolder(target) |> ignore
            load ()
            selected_id.Value <- id

            save_config
                { current.Config with
                    Name = skin_name
                    Author = Option.defaultValue "Unknown" username
                }

            true
        else
            false

    let export_current () =
        match current.Source with
        | Embedded _ -> failwith "Current skin must not be an embedded default"
        | Folder f ->
            let name = Path.GetFileName f
            let target = Path.Combine(get_game_folder "Exports", name + ".isk")

            if current.CompressToZip target then
                open_directory (get_game_folder "Exports")
                true
            else
                false

    let note_rotation keys =
        let rotations =
            if current.Config.UseRotation then
                current.Config.Rotations.[keys - 3]
            else
                Array.zeroCreate keys

        fun k -> Quad.rotate (rotations.[k])

    let preview_loader =
        { new Async.Service<Noteskin, (Bitmap * TextureConfig) option>() with
            override this.Handle(ns) =
                async {
                    return
                        match ns.GetTexture "note" with
                        | TextureOk(bmp, config) -> Some(bmp, config)
                        | _ -> None
                }
        }
