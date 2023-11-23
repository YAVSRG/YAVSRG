namespace Interlude

open System
open System.IO
open System.IO.Compression
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Prelude
open Prelude.Gameplay
open Prelude.Data.Content

module Content =

    let private DEFAULT_THEME =
        Theme.FromZipStream <| Utils.get_resource_stream "default.zip"

    let mutable accent_color = ThemeConfig.Default.DefaultAccentColor

    let mutable first_init = true

    module Sprites =

        let private cache = new Dictionary<string, Sprite>()

        let get (id: string) =
            if cache.ContainsKey id then
                cache.[id]
            else
                failwithf "No such loaded sprite: %s" id

        let add (id: string) (s: Sprite) =
            if cache.ContainsKey id then
                Sprite.destroy cache.[id]
                cache.Remove id |> ignore

            cache.Add(id, s)

    module Sounds =

        let private cache = new Dictionary<string, SoundEffect>()

        let get (id: string) =
            if cache.ContainsKey id then
                cache.[id]
            else
                failwithf "No such loaded sound: %s" id

        let add (id: string) (sound: SoundEffect) =
            if cache.ContainsKey id then
                cache.[id].Free()
                cache.Remove id |> ignore

            cache.Add(id, sound)

    module Rulesets =

        let DEFAULT_ID = "sc-j4"
        let private DEFAULT_RULESET = PrefabRulesets.SC.create 4

        let private loaded = Dictionary<string, Ruleset>()
        let mutable private id = DEFAULT_ID
        let mutable current = DEFAULT_RULESET
        let mutable current_hash = Ruleset.hash current

        let get_by_id (id) = loaded.[id]

        let try_get_by_hash (hash) =
            loaded.Values |> Seq.tryFind (fun rs -> Ruleset.hash rs = hash)

        let list () =
            seq {
                for k in loaded.Keys do
                    yield (k, loaded.[k])
            }

        let switch (new_id: string) =
            let new_id =
                if loaded.ContainsKey new_id then
                    new_id
                else
                    Logging.Warn("Ruleset '" + new_id + "' not found, switching to default")
                    DEFAULT_ID

            if new_id <> id || first_init then
                id <- new_id
                current <- loaded.[id]
                current_hash <- Ruleset.hash current

        let install (new_id, ruleset) =
            loaded.Remove new_id |> ignore
            loaded.Add(new_id, ruleset)

            if id = new_id then
                current <- ruleset
                current_hash <- Ruleset.hash current

            JSON.ToFile (Path.Combine(get_game_folder "Rulesets", new_id + ".ruleset"), true) ruleset

        let load () =
            loaded.Clear()

            let path = get_game_folder "Rulesets"
            let default_path = Path.Combine(path, DEFAULT_ID + ".ruleset")

            if not (File.Exists default_path) then
                JSON.ToFile (default_path, true) DEFAULT_RULESET

            for f in Directory.GetFiles(path) do
                if Path.GetExtension(f).ToLower() = ".ruleset" then
                    let id = Path.GetFileNameWithoutExtension(f)

                    match JSON.FromFile<Ruleset>(f) with
                    | Ok rs -> loaded.Add(id, rs.Validate)
                    | Error e -> Logging.Error(sprintf "Error loading ruleset '%s'" id, e)

            if not (loaded.ContainsKey DEFAULT_ID) then
                loaded.Add(DEFAULT_ID, DEFAULT_RULESET)

        let exists = loaded.ContainsKey

    module Themes =

        let private loaded = Dictionary<string, Theme>()

        module Current =

            let mutable id = "*default"
            let mutable instance = DEFAULT_THEME
            let mutable config = instance.Config

            let save_config (new_config: ThemeConfig) =
                instance.Config <- new_config
                config <- instance.Config

            let reload () =
                for id in Storage.THEME_TEXTURES do
                    match instance.GetTexture id with
                    | Some(img, config) ->
                        Sprite.upload (img, config.Rows, config.Columns, config.Sampling = Linear)
                        |> Sprite.cache id (id = "rain" || id = "logo" || id = "background")
                        |> Sprites.add id
                    | None ->
                        match loaded.["*default"].GetTexture id with
                        | Some(img, config) ->
                            Sprite.upload (img, config.Rows, config.Columns, config.Sampling = Linear)
                            |> Sprite.cache id (id = "rain" || id = "logo" || id = "background")
                            |> Sprites.add id
                        | None -> failwithf "Failed to load texture '%s' from *default" id

                for id in Storage.THEME_SOUNDS do
                    match instance.GetSound id with
                    | Some stream -> SoundEffect.FromStream(id, stream) |> Sounds.add id
                    | None ->
                        match loaded.["*default"].GetSound id with
                        | Some stream -> SoundEffect.FromStream(id, stream) |> Sounds.add id
                        | None -> failwithf "Failed to load sound '%s' from *default" id

                if config.OverrideAccentColor then
                    accent_color <- config.DefaultAccentColor

                for font in DEFAULT_THEME.GetFonts() do
                    Fonts.add font

                for font in instance.GetFonts() do
                    Fonts.add font

                if Style.font <> null then
                    Style.font.Dispose()

                Style.font <- Fonts.create config.Font

                Style.click <- Sounds.get "click"
                Style.hover <- Sounds.get "hover"
                Style.text_close <- Sounds.get "text-close"
                Style.text_open <- Sounds.get "text-open"
                Style.key <- Sounds.get "key"

                Style.notify_error <- Sounds.get "notify-error"
                Style.notify_info <- Sounds.get "notify-info"
                Style.notify_system <- Sounds.get "notify-system"
                Style.notify_task <- Sounds.get "notify-task"

            let switch (new_id: string) =
                let new_id =
                    if loaded.ContainsKey new_id then
                        new_id
                    else
                        Logging.Warn("Theme '" + new_id + "' not found, switching to default")
                        "*default"

                if new_id <> id || first_init then
                    id <- new_id
                    instance <- loaded.[id]
                    config <- loaded.[id].Config
                    reload ()

        // Loading into memory

        let load () =
            loaded.Clear()
            loaded.Add("*default", DEFAULT_THEME)

            for source in Directory.EnumerateDirectories(get_game_folder "Themes") do
                let id = Path.GetFileName source

                try
                    let theme = Theme.FromFolderName id
                    Logging.Debug(sprintf "  Loaded theme '%s' (%s)" theme.Config.Name id)
                    loaded.Add(id, theme)
                with err ->
                    Logging.Error("  Failed to load theme '" + id + "'", err)

            Logging.Info(sprintf "Loaded %i themes. (Including default)" loaded.Count)

            Current.switch Current.id

        let list () =
            loaded |> Seq.map (fun kvp -> (kvp.Key, kvp.Value.Config.Name)) |> Array.ofSeq

        let create_new (id: string) : bool =
            let id = Text.RegularExpressions.Regex("[^a-zA-Z0-9_-]").Replace(id, "")
            let target = Path.Combine(get_game_folder "Themes", id)

            if id <> "" && not (Directory.Exists target) then
                DEFAULT_THEME.ExtractToFolder(target)
                load ()
                Current.switch id

                Current.save_config
                    { Current.config with
                        Name = Current.config.Name + " (Extracted)"
                    }

                true
            else
                false

    module Noteskins =

        let loaded: Dictionary<string, Noteskin> = new Dictionary<string, Noteskin>()

        let private DEFAULTS =
            let skins = [ "defaultBar.isk"; "defaultArrow.isk"; "defaultOrb.isk" ]

            skins
            |> List.map Utils.get_resource_stream
            |> List.map Noteskin.FromZipStream
            |> List.zip (List.map (fun s -> "*" + s) skins)

        module Current =

            let mutable id = fst DEFAULTS.[0]
            let mutable instance = snd DEFAULTS.[0]
            let mutable config = instance.Config

            let save_config (new_config: NoteskinConfig) =
                instance.Config <- new_config
                config <- instance.Config

            let reload () =
                for id in Storage.NOTESKIN_TEXTURES do
                    match instance.GetTexture id with
                    | Some(img, config) ->
                        Sprite.upload (img, config.Rows, config.Columns, config.Sampling = Linear)
                        |> Sprite.cache id false
                        |> Sprites.add id
                    | None ->
                        Logging.Warn(
                            sprintf
                                "Noteskin texture '%s' didn't load properly, so it will appear as a white square ingame."
                                id
                        )

                        Sprite.DEFAULT |> Sprites.add id

            let switch (new_id: string) =
                let new_id =
                    if loaded.ContainsKey id then
                        new_id
                    else
                        Logging.Warn("Noteskin '" + new_id + "' not found, switching to default")
                        "*defaultBar.isk"

                if new_id <> id || first_init then
                    id <- new_id
                    instance <- loaded.[id]
                    config <- instance.Config

                reload ()

            let reload_texture (id: string) =
                match instance.GetTexture id with
                | Some(img, config) ->
                    Sprite.upload (img, config.Rows, config.Columns, config.Sampling = Linear)
                    |> Sprite.cache id false
                    |> Sprites.add id
                | None ->
                    Logging.Warn(
                        sprintf
                            "Noteskin texture '%s' didn't load properly, so it will appear as a white square ingame."
                            id
                    )

                    Sprite.DEFAULT |> Sprites.add id

        // Loading into memory

        let load () =

            loaded.Clear()

            for (id, ns) in DEFAULTS do
                loaded.Add(id, ns)

            for zip in
                Directory.EnumerateFiles(get_game_folder "Noteskins")
                |> Seq.filter (fun p -> Path.GetExtension(p).ToLower() = ".isk") do
                let target = Path.GetFileNameWithoutExtension zip

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
                    loaded.Add(id, ns)
                    Logging.Debug(sprintf "  Loaded noteskin '%s' (%s)" ns.Config.Name id)
                with err ->
                    Logging.Error("  Failed to load noteskin '" + id + "'", err)

            Logging.Info(sprintf "Loaded %i noteskins. (%i by default)" loaded.Count DEFAULTS.Length)

            Current.switch Current.id

        /// Returns id * Noteskin pairs
        let list () =
            loaded |> Seq.map (fun kvp -> (kvp.Key, kvp.Value)) |> Array.ofSeq

        let create_from_embedded (username: string option) : bool =
            if not Current.instance.IsEmbedded then
                failwith "Current skin must be an embedded default"

            let skin_name =
                match username with
                | Some u -> sprintf "%s's skin" u
                | None -> "Your skin"

            let id = Text.RegularExpressions.Regex("[^a-zA-Z0-9_-]").Replace(skin_name, "")
            let target = Path.Combine(get_game_folder "Noteskins", id)

            if id <> "" && not (Directory.Exists target) then
                Current.instance.ExtractToFolder(target)
                load ()
                Current.switch id

                Current.save_config
                    { Current.config with
                        Name = skin_name
                        Author = Option.defaultValue "Unknown" username
                    }

                true
            else
                false

        let export_current () =
            match Current.instance.Source with
            | Embedded _ -> failwith "Current skin must not be an embedded default"
            | Folder f ->
                let name = Path.GetFileName f
                let target = Path.Combine(get_game_folder "Exports", name + ".isk")

                if Current.instance.CompressToZip target then
                    Utils.open_directory (get_game_folder "Exports")
                    true
                else
                    false

        let note_rotation keys =
            let rotations =
                if Current.config.UseRotation then
                    Current.config.Rotations.[keys - 3]
                else
                    Array.zeroCreate keys

            fun k -> Quad.rotate (rotations.[k])

        let preview_loader =
            { new Async.Service<Noteskin, (Bitmap * TextureConfig) option>() with
                override this.Handle(ns) = async { return ns.GetTexture "note" }
            }

    let init (theme_id: string) (noteskin_id: string) =
        Themes.Current.id <- theme_id
        Noteskins.Current.id <- noteskin_id
        Logging.Info "===== Loading game content ====="
        Noteskins.load ()
        Themes.load ()
        Rulesets.load ()
        first_init <- false

    let inline get_texture (id: string) = Sprites.get id
    let inline noteskin_config () = Noteskins.Current.config
    let inline theme_config () = Themes.Current.config
