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
    let font () = _font.Value.Force()

    let inline themeConfig () = Themes.config.Value
    let inline noteskinConfig () = Noteskins.currentConfig.Value

    module Themes =
        
        let private defaultTheme = Theme.FromZipStream <| Utils.getResourceStream "default.zip"
        
        let config : ThemeConfig ref = ref defaultTheme.Config

        // Detection from file system

        let detected = new List<string>()
        let detect () = 
            detected.Clear()
            for t in Directory.EnumerateDirectories(getDataPath "Themes") do
                detected.Add(Path.GetFileName t)

        // Loading into memory

        let loaded = new List<Theme>()
        let load (ids: List<string>) =

            loaded.Clear()

            loaded.Add defaultTheme
            config.Value <- defaultTheme.Config
            Seq.choose 
                ( fun t ->
                    try
                        let theme = Theme.FromFolderName t
                        Logging.Info(sprintf "  Loaded theme '%s' (%s)" theme.Config.Name t)
                        Some theme
                    with err -> Logging.Error("  Failed to load theme '" + t + "'", err); None )
                ids
            |> Seq.iter (fun t -> loaded.Add t; config.Value <- t.Config)
            Logging.Info(sprintf "Loaded %i themes. (%i available) " <| loaded.Count - 1 <| detected.Count)

            if config.Value.OverrideAccentColor then accentColor.Value <- config.Value.DefaultAccentColor
            if _font.Value <> null then _font.Value.Force().Dispose()
            _font.Value <- lazy (Text.createFont config.Value.Font)
            
            GameplayConfig.clearCache()
            Sprites.clearCache()

        // The other stuff

        let pick f =
            let rec g i =
                if i < 0 then failwith "f should give some value for default theme!!"
                match f loaded.[i] with
                | Some v -> v
                | None -> g (i - 1)
            g (loaded.Count - 1)

        let createNew (id: string) =
             let id = Text.RegularExpressions.Regex("[^a-zA-Z0-9_-]").Replace(id, "")
             let target = Path.Combine(getDataPath "Themes", id)
             if id <> "" && not (Directory.Exists target) then defaultTheme.CopyTo(Path.Combine(getDataPath "Themes", id))
             detect()

    module Noteskins =

        let private defaults =
            let skins = ["defaultBar.isk"; "defaultArrow.isk"; "defaultOrb.isk"]
            skins
            |> List.map Utils.getResourceStream
            |> List.map NoteSkin.FromZipStream
            |> List.zip (List.map (fun s -> "*" + s) skins)

        let currentId = ref "*defaultBar.isk"
        let current () = loaded.[currentId.Value]
        let currentConfig : NoteSkinConfig ref = ref NoteSkinConfig.Default
        
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

        let loaded : Dictionary<string, NoteSkin> = new Dictionary<string, NoteSkin>()
        let load () =

            loaded.Clear()

            for (id, ns) in defaults do
                loaded.Add(id, ns)

            for source, isZip in detected do
                let id = Path.GetFileName source
                try 
                    let ns = if isZip then NoteSkin.FromZipFile source else NoteSkin.FromFolder source
                    loaded.Add(id, ns)
                    Logging.Info(sprintf "  Loaded noteskin '%s' (%s)" ns.Config.Name id)
                with err -> Logging.Error("  Failed to load noteskin '" + id + "'", err)

            Logging.Info(sprintf "Loaded %i noteskins. (%i by default)" loaded.Count defaults.Length)
            
            switch currentId.Value

        // The other stuff

        let switch (id: string) =
            let id = if loaded.ContainsKey id then id else Logging.Warn("Noteskin '" + id + "' not found, switching to default"); "*defaultBar.isk"
            currentId.Value <- id
            currentConfig.Value <- loaded.[id].Config
            Sprites.clearNoteskinTextures()

        let list () = loaded |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))

    module Sprites =
        
        let private cache = new Dictionary<string, Sprite>()

        let getTexture (name: string) =
            if not <| cache.ContainsKey name then
                if Array.contains name noteskinTextures then
                    match Noteskins.current().GetTexture name with
                    | Some (bmp, config) -> Sprite.upload(bmp, config.Rows, config.Columns, false)
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
                    cache.Add(name, Sprite.upload (bmp, config.Rows, config.Columns, false))
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

    let load (themes: List<string>) =
        Logging.Info "===== Loading Themes/Noteskins ====="
        Themes.load themes
        Noteskins.load()

    let inline getTexture (id: string) = Sprites.getTexture id