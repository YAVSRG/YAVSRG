namespace Interlude

open System
open System.IO
open System.Collections.Generic
open System.Drawing
open Prelude.Common
open Prelude.Data.Themes

module Themes =
    open Interlude.Graphics

    let private defaultTheme = Theme.FromZipStream <| Interlude.Utils.getResourceStream "default.zip"
    let private defaultNoteskins =
        let skins = ["defaultBar.isk"; "defaultArrow.isk"; "defaultOrb.isk"]
        skins
        |> List.map Interlude.Utils.getResourceStream
        |> List.map NoteSkin.FromZipStream
        |> List.zip (List.map (fun s -> "*" + s) skins)
    let private loadedNoteskins = new Dictionary<string, NoteSkin>()
    let private loadedThemes = new List<Theme>()
    let private gameplayConfig = new Dictionary<string, obj>()

    let mutable themeConfig = ThemeConfig.Default
    let mutable noteskinConfig = NoteSkinConfig.Default
    let mutable currentNoteSkin = "*defaultBar.isk"

    let mutable internal accentColor = themeConfig.DefaultAccentColor
    let mutable private fontBuilder : Lazy<Text.SpriteFont> option = None
    let font() : Text.SpriteFont = fontBuilder.Value.Force()

    let noteskins() = loadedNoteskins |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))

    let availableThemes = new List<string>()
    let refreshAvailableThemes() = 
        availableThemes.Clear()
        for t in Directory.EnumerateDirectories(getDataPath "Themes") do
            availableThemes.Add(Path.GetFileName t)

    let private sprites = new Dictionary<string, Sprite>()
    let private sounds = "nyi"
    
    // id is the folder name of the noteskin, NOT the name given in the noteskin metadata
    let changeNoteSkin(id: string) =
        let id = if loadedNoteskins.ContainsKey(id) then id else Logging.Warn("Noteskin '" + id + "' not found, switching to default"); "*defaultBar.isk"
        currentNoteSkin <- id
        noteskinConfig <- loadedNoteskins.[id].Config
        Array.iter
            (fun t -> 
                if sprites.ContainsKey t then
                    Sprite.destroy sprites.[t]
                    sprites.Remove t |> ignore
            ) noteskinTextures

    let createNew(id: string) =
        let id = System.Text.RegularExpressions.Regex("[^a-zA-Z0-9_-]").Replace(id, "")
        if id <> "" && availableThemes.Contains id |> not then defaultTheme.CopyTo(Path.Combine(getDataPath "Themes", id))
        refreshAvailableThemes()

    let private getInherited f =
        let rec g i =
            if i < 0 then failwith "f should give some value for default theme"
            match f loadedThemes.[i] with
            | Some v -> v
            | None -> g (i - 1)
        g (loadedThemes.Count - 1)

    // gets texture and handles caching it
    let getTexture(name: string) =
        if not <| sprites.ContainsKey(name) then
            if Array.contains name noteskinTextures then
                match loadedNoteskins.[currentNoteSkin].GetTexture name with
                | Some (bmp, config) -> Sprite.upload(bmp, config.Rows, config.Columns, false)
                | None -> Sprite.Default
                |> fun x -> sprites.Add(name, x)
            else
                let (bmp, config) =
                    getInherited (fun (t: Theme) ->
                        try t.GetTexture name
                        with err -> Logging.Error("Failed to load texture '" + name + "'", err); None)
                sprites.Add(name, Sprite.upload (bmp, config.Rows, config.Columns, false))
        sprites.[name]

    // gets gameplay config and handles caching it
    // todo: support for tracking where the file came from so we can modify it from ingame
    let getGameplayConfig<'T>(name: string) = 
        if gameplayConfig.ContainsKey name then
            gameplayConfig.[name] :?> 'T
        else
            let o =
                (fun (t: Theme) ->
                    let (x, success) = t.GetGameplayConfig name
                    if success then Some x else None)
                |> getInherited
            gameplayConfig.Add(name, o :> obj)
            o
    
    let loadThemes(themes: List<string>) =
        Logging.Info("===== Loading Themes/Noteskins =====")
        loadedNoteskins.Clear()
        loadedThemes.Clear()
        loadedThemes.Add defaultTheme
        themeConfig <- defaultTheme.Config
        Seq.choose (fun t ->
            try
                let theme = Theme.FromFolderName t
                Logging.Info(sprintf "  Loaded theme '%s' (%s)" theme.Config.Name t)
                Some theme
            with err -> Logging.Error("  Failed to load theme '" + t + "'", err); None)
            themes
        |> Seq.iter (fun t -> loadedThemes.Add t; themeConfig <- t.Config)
        Logging.Info(sprintf "Loaded %i themes. (%i available) " <| loadedThemes.Count - 1 <| availableThemes.Count)
    
        loadedNoteskins.Clear()
        // load embedded noteskins
        for (id, ns) in defaultNoteskins do
            loadedNoteskins.Add(id, ns)
        // load folder noteskins
        for t in Directory.EnumerateDirectories(getDataPath "Noteskins") do
            let ns = NoteSkin.FromFolder t
            let id = Path.GetFileName t
            loadedNoteskins.Add(id, ns)
            Logging.Info(sprintf "  Loaded noteskin '%s' (%s)" ns.Config.Name id)
        // load zipped noteskins
        for t in
            Directory.EnumerateFiles(getDataPath "Noteskins")
            |> Seq.filter (fun p -> let ext = Path.GetExtension(p).ToLower() in ext = ".isk" || ext = ".zip" ) do
            let ns = NoteSkin.FromZipFile t
            let id = Path.GetFileName t
            loadedNoteskins.Add(id, ns)
            Logging.Info(sprintf "  Loaded noteskin '%s' (%s)" ns.Config.Name id)

        Logging.Info(sprintf "Loaded %i noteskins. (%i by default)" loadedNoteskins.Count defaultNoteskins.Length)

        Seq.iter Sprite.destroy sprites.Values
        sprites.Clear()
        gameplayConfig.Clear()
        changeNoteSkin currentNoteSkin
        if themeConfig.OverrideAccentColor then accentColor <- themeConfig.DefaultAccentColor
        if fontBuilder.IsSome then font().Dispose(); 
        fontBuilder <- Some (lazy (Text.createFont themeConfig.Font))