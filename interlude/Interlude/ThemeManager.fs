namespace Interlude

open System
open System.IO
open System.Collections.Generic
open System.Drawing
open Prelude.Common
open Prelude.Data.Themes

module Themes =
    open Interlude.Graphics

    let private noteskinTextures = [|"note"; "receptor"; "mine"; "holdhead"; "holdbody"; "holdtail"|]

    let private defaultTheme = Theme.FromZipStream <| Interlude.Utils.getResourceStream("default.zip")
    let private loadedNoteskins = new Dictionary<string, NoteSkinConfig * int>()
    let private loadedThemes = new List<Theme>()
    let private gameplayConfig = new Dictionary<string, obj>()

    let mutable themeConfig = ThemeConfig.Default
    let mutable noteskinConfig = NoteSkinConfig.Default
    let mutable currentNoteSkin = "default"

    let mutable internal accentColor = themeConfig.DefaultAccentColor
    let mutable private fontBuilder: Lazy<Text.SpriteFont> option = None
    let font(): Text.SpriteFont = fontBuilder.Value.Force()

    let noteskins() =
        loadedNoteskins |> Seq.map (fun kvp -> (kvp.Key, let (data, i) = kvp.Value in data))

    let availableThemes = new List<string>()
    let refreshAvailableThemes() = 
        availableThemes.Clear()
        for t in Directory.EnumerateDirectories(getDataPath("Themes")) do
            availableThemes.Add(Path.GetFileName(t))

    let private sprites = new Dictionary<string, Sprite>()
    let private sounds = "nyi"
    
    // id is the folder name of the noteskin, NOT the name given in the noteskin metadata
    let changeNoteSkin(id: string) =
        let id = if loadedNoteskins.ContainsKey(id) then id else Logging.Warn("Noteskin '" + id + "' not found, switching to default"); "default"
        currentNoteSkin <- id
        noteskinConfig <- fst loadedNoteskins.[id]
        Array.iter
            (fun t -> 
                if sprites.ContainsKey(t) then
                    Sprite.destroy sprites.[t]
                    sprites.Remove(t) |> ignore
            ) noteskinTextures

    let createNew(id: string) =
        let id = System.Text.RegularExpressions.Regex("[^a-zA-Z0-9_-]").Replace(id, "")
        if id <> "" && availableThemes.Contains id |> not then defaultTheme.CopyTo(Path.Combine(getDataPath "themes", id))
        refreshAvailableThemes()

    let private getInherited f =
        let rec g i =
            if i < 0 then failwith "f should give some value for default theme"
            match f loadedThemes.[i] with
            | Some v -> v
            | None -> g (i - 1)
        g (loadedThemes.Count - 1)

    let getTexture(name: string) =
        if not <| sprites.ContainsKey(name) then
            if Array.contains name noteskinTextures then
                let (ns, i) = loadedNoteskins.[currentNoteSkin]
                match loadedThemes.[i].GetTexture(Some currentNoteSkin, name) with
                | Some (bmp, config) -> Sprite.upload(bmp, config.Rows, config.Columns, false)
                | None -> Sprite.Default
                |> fun x -> sprites.Add(name, x)
            else
                let (bmp, config) =
                    getInherited (fun (t: Theme) ->
                        try t.GetTexture(None, name)
                        with err -> Logging.Error("Failed to load texture '" + name + "'", err); None)
                sprites.Add(name, Sprite.upload(bmp, config.Rows, config.Columns, false))
        sprites.[name]

    let getGameplayConfig<'T>(name: string) = 
        if gameplayConfig.ContainsKey(name) then
            gameplayConfig.[name] :?> 'T
        else
            let o =
                (fun (t: Theme) ->
                    let (x, success) = t.GetJson<'T>(true, "Interface", "Gameplay", name + ".json")
                    if success then Some x else None)
                |> getInherited
            gameplayConfig.Add(name, o :> obj)
            o
    
    let loadThemes(themes: List<string>) =
        Logging.Info("===== Loading Themes =====")
        loadedNoteskins.Clear()
        loadedThemes.Clear()
        loadedThemes.Add defaultTheme
        themeConfig <- ThemeConfig.Default
        Seq.choose (fun t ->
            let theme = Theme.FromThemeFolder t
            try
                let config: ThemeConfig = theme.GetJson(false, "theme.json") |> fst
                Some (theme, config)
            with err -> Logging.Error("Failed to load theme '" + t + "'", err); None)
            themes
        |> Seq.iter (fun (t, conf) -> loadedThemes.Add t; themeConfig <- conf)
    
        loadedThemes
        |> Seq.iteri(fun i t ->
            // this is where we load other stuff like scripting in future
            t.GetNoteSkins()
            |> Seq.iter (fun (ns, c) ->
                loadedNoteskins.Remove ns |> ignore // overwrite existing skin with same name
                loadedNoteskins.Add(ns, (c, i))
                Logging.Info(sprintf "Loaded noteskin %s" ns)))
        Seq.iter Sprite.destroy sprites.Values
        sprites.Clear()
        gameplayConfig.Clear()
        changeNoteSkin currentNoteSkin
        if themeConfig.OverrideAccentColor then accentColor <- themeConfig.DefaultAccentColor
        if fontBuilder.IsSome then font().Dispose(); 
        fontBuilder <- Some (lazy (Text.createFont themeConfig.Font))
        Logging.Info(sprintf "===== Loaded %i themes (%i available) =====" <| loadedThemes.Count - 1 <| availableThemes.Count)