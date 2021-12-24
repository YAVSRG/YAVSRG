namespace Interlude

open System
open System.IO
open System.Collections.Generic
open System.Drawing
open Prelude.Common
open Prelude.Data.Themes
open Interlude.Graphics

module rec Content =

    let accentColor = ref ThemeConfig.Default.DefaultAccentColor
    let _font : Lazy<Text.SpriteFont> ref = ref null
    let font () = _font.Value.Value

    let inline themeConfig () = Themes.config.Value
    let inline noteskinConfig () = Noteskins.currentConfig.Value

    module Themes =
        
        let private _default = Theme.FromZipStream <| Utils.getResourceStream "default.zip"

        let currentId = ref "*default"
        let current () = loaded.[currentId.Value]
        
        let config : ThemeConfig ref = ref _default.Config

        // Detection from file system

        let detected = new List<string>()
        let detect () = 
            detected.Clear()
            for t in Directory.EnumerateDirectories(getDataPath "Themes") do
                detected.Add(Path.GetFileName t)

        // Loading into memory

        let loaded : Dictionary<string, Theme> = new Dictionary<string, Theme>()
        let load() =

            loaded.Clear()

            loaded.Add ("*default", _default)
            config.Value <- _default.Config
            for source in detected do
                let id = Path.GetFileName source
                try
                    let theme = Theme.FromFolderName id
                    Logging.Info(sprintf "  Loaded theme '%s' (%s)" theme.Config.Name id)
                    loaded.Add(id, theme)
                with err -> Logging.Error("  Failed to load theme '" + id + "'", err)

            Logging.Info(sprintf "Loaded %i themes. (Including default)" loaded.Count)

            switch currentId.Value

        // The other stuff

        let switch (id: string) =
            let id = if loaded.ContainsKey id then id else Logging.Warn("Theme '" + id + "' not found, switching to default"); "*default"
            if id <> currentId.Value || _font.Value = null then
                currentId.Value <- id
                config.Value <- loaded.[id].Config

                if config.Value.OverrideAccentColor then accentColor.Value <- config.Value.DefaultAccentColor
                if _font.Value <> null then if _font.Value.IsValueCreated then _font.Value.Value.Dispose()
                _font.Value <- lazy (Text.createFont config.Value.Font)

                GameplayConfig.clearCache()
                Sprites.clearCache()

        let list () = loaded |> Seq.map (fun kvp -> (kvp.Key, kvp.Value.Config.Name)) |> Array.ofSeq

        let pick f =
            match f (current ()) with
            | Some x -> x
            | None ->
                match f loaded.["*default"] with
                | Some x -> x
                | None -> failwith "f should give some value for default theme!!"

        let createNew (id: string) =
             let id = Text.RegularExpressions.Regex("[^a-zA-Z0-9_-]").Replace(id, "")
             let target = Path.Combine(getDataPath "Themes", id)
             if id <> "" && not (Directory.Exists target) then _default.CopyTo(Path.Combine(getDataPath "Themes", id))
             detect()
             load()
             switch id
             current().Config <- { current().Config with Name = current().Config.Name + " (Extracted)" }

        let lampToColor (lampAchieved: Prelude.Scoring.Lamp) = config.Value.LampColors.[lampAchieved |> int]
        let gradeToColor (gradeAchieved: int) = config.Value.Grades.[gradeAchieved].Color
        let clearToColor (cleared: bool) = config.Value.ClearColors |> (if cleared then fst else snd)

    module Noteskins =
        
        open Prelude.Data.SkinConversions

        let private defaults =
            let skins = ["defaultBar.isk"; "defaultArrow.isk"; "defaultOrb.isk"]
            skins
            |> List.map Utils.getResourceStream
            |> List.map Noteskin.FromZipStream
            |> List.zip (List.map (fun s -> "*" + s) skins)

        let currentId = ref "*defaultBar.isk"
        let current () = loaded.[currentId.Value]
        let currentConfig : NoteskinConfig ref = ref NoteskinConfig.Default
        
        // Detection from file system

        let detected = new List<string * bool>()
        let detect () = 

            detected.Clear()

            for t in Directory.EnumerateDirectories(getDataPath "Noteskins") do
                detected.Add(t, false)

            for t in Directory.EnumerateDirectories(getDataPath "Noteskins")
                |> Seq.filter (fun p -> let ext = Path.GetExtension(p).ToLower() in ext = ".isk" || ext = ".zip") do
                detected.Add(t, true)

        // Loading into memory

        let loaded : Dictionary<string, Noteskin> = new Dictionary<string, Noteskin>()
        let load () =

            loaded.Clear()

            for (id, ns) in defaults do
                loaded.Add(id, ns)

            for source, isZip in detected do
                let id = Path.GetFileName source
                try 
                    let ns = if isZip then Noteskin.FromZipFile source else Noteskin.FromFolder source
                    loaded.Add(id, ns)
                    Logging.Info(sprintf "  Loaded noteskin '%s' (%s)" ns.Config.Name id)
                with err -> Logging.Error("  Failed to load noteskin '" + id + "'", err)

            Logging.Info(sprintf "Loaded %i noteskins. (%i by default)" loaded.Count defaults.Length)
            
            switch currentId.Value

        // The other stuff

        let switch (id: string) =
            let id = if loaded.ContainsKey id then id else Logging.Warn("Noteskin '" + id + "' not found, switching to default"); "*defaultBar.isk"
            if id <> currentId.Value then
                currentId.Value <- id
                Sprites.clearNoteskinTextures()
            currentConfig.Value <- loaded.[id].Config

        let list () = loaded |> Seq.map (fun kvp -> (kvp.Key, kvp.Value.Config.Name)) |> Array.ofSeq

        let extractCurrent() =
            let id = Guid.NewGuid().ToString()
            current().CopyTo(Path.Combine(getDataPath "Noteskins", id))
            detect()
            load()
            switch id
            current().Config <- { current().Config with Name = current().Config.Name + " (Extracted)" }

        let tryImport(path: string) : bool =
            match path with
            | OsuSkinFolder ->
                let id = Guid.NewGuid().ToString()
                try
                    OsuSkin.Converter(path).ToNoteskin(Path.Combine(getDataPath "Noteskins", id)) 4
                    detect()
                    load()
                    true
                with err -> Logging.Error("Something went wrong converting this skin!", err); true
            | InterludeSkinArchive ->
                try 
                    File.Copy(path, Path.Combine(getDataPath "Noteskins", Path.GetFileName path))
                    detect()
                    load()
                    true
                with err -> Logging.Error("Something went wrong when moving this skin!", err); true
            | OsuSkinArchive -> Logging.Info("Can't directly drop .osks yet, sorry :( You'll have to extract it first"); true
            | Unknown -> false

    module Sprites =
        
        let private cache = new Dictionary<string, Sprite>()

        let getTexture (name: string) =
            if not <| cache.ContainsKey name then
                if Array.contains name noteskinTextures then
                    match Noteskins.current().GetTexture name with
                    | Some (bmp, config) -> Sprite.upload(bmp, config.Rows, config.Columns, false) |> Sprite.cache name
                    | None ->
                        match Noteskins.loaded.["*defaultBar.isk"].GetTexture name with
                        | Some (bmp, config) -> Sprite.upload(bmp, config.Rows, config.Columns, false)
                        | None -> failwith "defaultBar doesnt have this texture!!"
                    |> fun x -> cache.Add(name, x)
                else
                    let (bmp, config) =
                        Themes.pick (fun (t: Theme) ->
                            try t.GetTexture name
                            with err -> Logging.Error("Failed to load texture '" + name + "'", err); None)
                    cache.Add(name, Sprite.upload (bmp, config.Rows, config.Columns, false) |> Sprite.cache name)
            cache.[name]

        let clearCache() =
            Seq.iter Sprite.destroy cache.Values
            cache.Clear()

        let clearNoteskinTextures() =
            Array.iter
                ( fun t -> 
                    if cache.ContainsKey t then
                        Sprite.destroy cache.[t]
                        cache.Remove t |> ignore
                ) noteskinTextures
    
    module Sounds =
        "not yet implemented"
        |> ignore

    module GameplayConfig =

        let private cache = new Dictionary<string, obj>()

        let get<'T> (name: string) = 
            if cache.ContainsKey name then
                cache.[name] :?> 'T
            else
                let o =
                    (fun (t: Theme) ->
                        let (x, success) = t.GetGameplayConfig name
                        if success then Some x else None)
                    |> Themes.pick
                cache.Add(name, o :> obj)
                o

        let clearCache() = cache.Clear()

    let detect () =
        Themes.detect()
        Noteskins.detect()

    let load () =
        Logging.Info "===== Loading Themes/Noteskins ====="
        Themes.load()
        Noteskins.load()

    let inline getTexture (id: string) = Sprites.getTexture id