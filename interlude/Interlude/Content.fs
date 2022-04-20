namespace Interlude

open System
open System.IO
open System.Collections.Generic
open Prelude.Common
open Prelude.Scoring
open Prelude.Data.Themes
open Prelude.Data
open Prelude.Data.Noteskin
open Interlude.Graphics

module Content =

    let private defaultTheme = Theme.FromZipStream <| Utils.getResourceStream "default.zip"

    let mutable accentColor = ThemeConfig.Default.DefaultAccentColor
    let mutable font : Fonts.SpriteFont = null

    let mutable first_init = true

    module Sprites =
        
        let private cache = new Dictionary<string, Sprite>()
        
        let getTexture (id: string) =
            if cache.ContainsKey id then cache.[id]
            else failwithf "should have already loaded %s" id

        let add (id: string) (s: Sprite) = 
            if cache.ContainsKey id then
                Sprite.destroy cache.[id]
                cache.Remove id |> ignore
            cache.Add(id, s)

    module Sounds =
        "not yet implemented"
        |> ignore

    module Rulesets =

        let DEFAULT = "*sc-j4"
       
        let private loaded = let x = Dictionary<string, Ruleset>() in (for name, rs in defaultTheme.GetRulesets() do x.Add("*" + name, rs)); x
        let mutable private _theme : Theme = Unchecked.defaultof<_>
        let mutable private id = DEFAULT
        let mutable current = loaded.[DEFAULT]

        let list() = 
            seq {
                for k in loaded.Keys do
                    yield (k, loaded.[k])
            }

        let reload() =
            let sourceTheme = if id.StartsWith('*') then defaultTheme else _theme
            for id in Storage.rulesetTextures do
                let fileid = current.TextureNamePrefix + id 
                let img, config = sourceTheme.GetRulesetTexture fileid
                Sprite.upload(img, config.Rows, config.Columns, true)
                |> Sprite.cache id |> Sprites.add id

        let switch (new_id: string) (themeChanged: bool) =
            let new_id = if loaded.ContainsKey new_id then new_id else Logging.Warn("Ruleset '" + new_id + "' not found, switching to default"); DEFAULT
            if Object.ReferenceEquals(_theme, null) |> not && (new_id <> id || themeChanged) then
                id <- new_id
                current <- loaded.[id]
                reload()

        let load_from_theme (theme: Theme) =
            _theme <- theme
            loaded.Clear()
            for name, rs in defaultTheme.GetRulesets() do loaded.Add("*" + name, rs)
            if _theme <> defaultTheme then
                for name, rs in _theme.GetRulesets() do loaded.Add(name, rs)
            switch id true

        let exists = loaded.ContainsKey

    module Themes =

        let private loaded = Dictionary<string, Theme>()

        module Current =

            let mutable id = "*default"
            let mutable instance = defaultTheme
            let mutable config = instance.Config

            module GameplayConfig =

                open WidgetConfig

                let private cache = Dictionary<string, obj>()

                let private add<'T>() =
                    let id = typeof<'T>.Name
                    cache.Remove(id) |> ignore
                    cache.Add(id, instance.GetGameplayConfig<'T> id)

                let reload() =
                    add<AccuracyMeter>()
                    add<HitMeter>()
                    add<LifeMeter>()
                    add<Combo>()
                    add<SkipButton>()
                    add<ProgressMeter>()
                    add<JudgementMeter>()
            
                let get<'T>() = 
                    let id = typeof<'T>.Name
                    if cache.ContainsKey id then
                        cache.[id] :?> 'T
                    else failwithf "config not loaded: %s" id

            let changeConfig(new_config: ThemeConfig) =
                instance.Config <- new_config
                config <- instance.Config

            let reload() =
                if config.OverrideAccentColor then accentColor <- config.DefaultAccentColor
                for font in defaultTheme.GetFonts() do
                    Fonts.add font
                for font in instance.GetFonts() do
                    Fonts.add font
                if font <> null then font.Dispose()
                font <- Fonts.create config.Font

                for id in Storage.themeTextures do
                    match instance.GetTexture id with
                    | Some (img, config) -> Sprite.upload(img, config.Rows, config.Columns, false) |> Sprite.cache id |> Sprites.add id
                    | None ->
                        match loaded.["*default"].GetTexture id with
                        | Some (img, config) -> Sprite.upload(img, config.Rows, config.Columns, false) |> Sprite.cache id |> Sprites.add id
                        | None -> failwith "default doesnt have this texture!!"

                GameplayConfig.reload()
                Rulesets.load_from_theme(instance)

            let switch (new_id: string) =
                let new_id = if loaded.ContainsKey new_id then new_id else Logging.Warn("Theme '" + new_id + "' not found, switching to default"); "*default"
                if new_id <> id || first_init then
                    id <- new_id
                    instance <- loaded.[id]
                    config <- loaded.[id].Config
                    reload()

        // Loading into memory

        let load() =
            loaded.Clear()
            loaded.Add ("*default", defaultTheme)

            for source in Directory.EnumerateDirectories(getDataPath "Themes") do
                let id = Path.GetFileName source
                try
                    let theme = Theme.FromFolderName id
                    Logging.Debug(sprintf "  Loaded theme '%s' (%s)" theme.Config.Name id)
                    loaded.Add(id, theme)
                with err -> Logging.Error("  Failed to load theme '" + id + "'", err)

            Logging.Info(sprintf "Loaded %i themes. (Including default)" loaded.Count)

            Current.switch Current.id

        let list() = loaded |> Seq.map (fun kvp -> (kvp.Key, kvp.Value.Config.Name)) |> Array.ofSeq

        let createNew (id: string) =
             let id = Text.RegularExpressions.Regex("[^a-zA-Z0-9_-]").Replace(id, "")
             let target = Path.Combine(getDataPath "Themes", id)
             if id <> "" && not (Directory.Exists target) then defaultTheme.ExtractToFolder(Path.Combine(getDataPath "Themes", id))
             load()
             Current.switch id
             Current.changeConfig { Current.config with Name = Current.config.Name + " (Extracted)" }

        let clearToColor (cleared: bool) = Current.config.ClearColors |> (if cleared then fst else snd)

    module Noteskins =
        
        let loaded : Dictionary<string, Noteskin> = new Dictionary<string, Noteskin>()

        let private defaults =
            let skins = ["defaultBar.isk"; "defaultArrow.isk"; "defaultOrb.isk"]
            skins
            |> List.map Utils.getResourceStream
            |> List.map Noteskin.FromZipStream
            |> List.zip (List.map (fun s -> "*" + s) skins)

        module Current =
            
            let mutable id = fst defaults.[0]
            let mutable instance = snd defaults.[0]
            let mutable config = instance.Config
            
            let changeConfig(new_config: NoteskinConfig) =
                instance.Config <- new_config
                config <- instance.Config

            let reload() =
                for id in Storage.noteskinTextures do
                    match instance.GetTexture id with
                    | Some (img, config) -> Sprite.upload(img, config.Rows, config.Columns, false) |> Sprite.cache id |> Sprites.add id
                    | None ->
                        match loaded.["*defaultBar.isk"].GetTexture id with
                        | Some (img, config) -> Sprite.upload(img, config.Rows, config.Columns, false) |> Sprite.cache id |> Sprites.add id
                        | None -> failwith "defaultBar doesnt have this texture!!"

            let switch (new_id: string) =
                let new_id = if loaded.ContainsKey id then new_id else Logging.Warn("Noteskin '" + new_id + "' not found, switching to default"); "*defaultBar.isk"
                if new_id <> id || first_init then
                    id <- new_id
                    instance <- loaded.[id]
                    config <- instance.Config
                reload()

        // Loading into memory

        let load () =

            loaded.Clear()

            for (id, ns) in defaults do
                loaded.Add(id, ns)

            let add (source: string) (isZip: bool) =
                let id = Path.GetFileName source
                try 
                    let ns = if isZip then Noteskin.FromZipFile source else Noteskin.FromPath source
                    loaded.Add(id, ns)
                    Logging.Debug(sprintf "  Loaded noteskin '%s' (%s)" ns.Config.Name id)
                with err -> Logging.Error("  Failed to load noteskin '" + id + "'", err)

            for source in Directory.EnumerateDirectories(getDataPath "Noteskins") do add source false
            for source in 
                Directory.EnumerateFiles(getDataPath "Noteskins")
                |> Seq.filter (fun p -> Path.GetExtension(p).ToLower() = ".isk")
                do add source true

            Logging.Info(sprintf "Loaded %i noteskins. (%i by default)" loaded.Count defaults.Length)
            
            Current.switch Current.id

        let list () = loaded |> Seq.map (fun kvp -> (kvp.Key, kvp.Value.Config.Name)) |> Array.ofSeq

        let extractCurrent() =
            let id = Guid.NewGuid().ToString()
            Current.instance.ExtractToFolder(Path.Combine(getDataPath "Noteskins", id))
            load()
            Current.switch id
            Current.changeConfig { Current.config with Name = Current.config.Name + " (Extracted)" }

        let tryImport(path: string) : bool =
            match path with
            | OsuSkinFolder ->
                let id = Guid.NewGuid().ToString()
                try
                    OsuSkin.Converter.convert path (Path.Combine(getDataPath "Noteskins", id)) [4; 7]
                    load()
                    true
                with err -> Logging.Error("Something went wrong converting this skin!", err); true
            | InterludeSkinArchive ->
                try 
                    File.Copy(path, Path.Combine(getDataPath "Noteskins", Path.GetFileName path))
                    load()
                    true
                with err -> Logging.Error("Something went wrong when moving this skin!", err); true
            | OsuSkinArchive -> Logging.Info("Can't directly drop .osks yet, sorry :( You'll have to extract it first"); true
            | Unknown -> false

    let init (themeId: string) (noteskinId: string) =
        Themes.Current.id <- themeId
        Noteskins.Current.id <- noteskinId
        Logging.Info "===== Loading game content ====="
        Noteskins.load()
        Themes.load()
        first_init <- false

    let inline getGameplayConfig<'T>() = Themes.Current.GameplayConfig.get<'T>()
    let inline getTexture (id: string) = Sprites.getTexture id
    let inline noteskinConfig() = Noteskins.Current.config
    let inline themeConfig() = Themes.Current.config