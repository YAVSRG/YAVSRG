namespace Interlude

open System
open System.IO
open System.IO.Compression
open System.Collections.Generic
open System.Drawing
open Prelude.Common
open Prelude.Data.Themes

type Theme(storage) =
    inherit Prelude.Data.Themes.Theme(storage)

    member this.GetTexture(noteskin: string option, name: string) =
        let folder = 
            match noteskin with
            | None -> "Textures"
            | Some n ->
                match storage with
                | Folder _ -> Path.Combine("Noteskins", n)
                | Zip _ -> "Noteskins/" + n
        let bmp = 
            match base.TryReadFile(folder, name + ".png") with
            | Some s -> use stream = s in new Bitmap(stream)
            | None -> new Bitmap(1, 1)
        let info: TextureConfig =
            this.GetJson<TextureConfig>(false, folder, name + ".json") |> fst
        (bmp, info)

    member this.GetNoteSkins() =
        Seq.choose
            (fun ns ->
                let (config: NoteSkinConfig, success: bool) = this.GetJson(false, "Noteskins", ns, "noteskin.json")
                if success then Some (ns, config) else None)
            (this.GetFolders("Noteskins"))

    static member FromZipStream(stream: Stream) = Theme(Zip <| new ZipArchive(stream))
    static member FromThemeFolder(name: string) = Theme(Folder <| getDataPath(Path.Combine("Themes", name)))

module Themes =
    open Interlude.Render

    let private noteskinTextures = [|"note"; "receptor"; "mine"; "holdhead"; "holdbody"; "holdtail"|]

    let private defaultTheme = Theme.FromZipStream <| Interlude.Utils.getResourceStream("default.zip")
    let private loadedNoteskins = new Dictionary<string, NoteSkinConfig * int>()
    let private loadedThemes = new List<Theme>()
    let private gameplayConfig = new Dictionary<string, obj>()

    let mutable themeConfig = ThemeConfig.Default
    let mutable noteskinConfig = NoteSkinConfig.Default
    let mutable currentNoteSkin = "default"
    let mutable background = Sprite.Default
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
        let id = if loadedNoteskins.ContainsKey(id) then id else Logging.Warn("Noteskin '" + id + "' not found, switching to default") ""; "default"
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
            let (bmp, config) =
                if Array.contains name noteskinTextures then
                    let (ns, i) = loadedNoteskins.[currentNoteSkin]
                    loadedThemes.[i].GetTexture(Some currentNoteSkin, name)
                else
                    getInherited (fun (t: Theme) ->
                        try Some <| t.GetTexture(None, name)
                        with err -> Logging.Error("Failed to load texture '" + name + "'") (err.ToString()); None)
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

    let loadBackground(file: string) =
        if background.ID <> 0 && background.ID <> getTexture("background").ID then Sprite.destroy background
        background <- 
        match Path.GetExtension(file).ToLower() with
        | ".png" | ".bmp" | ".jpg" | ".jpeg" ->
            try
                use bmp = new Bitmap(file)
                accentColor <-
                    if themeConfig.OverrideAccentColor then themeConfig.DefaultAccentColor else
                        let vibrance (c:Color) = Math.Abs(int c.R - int c.B) + Math.Abs(int c.B - int c.G) + Math.Abs(int c.G - int c.R)
                        seq {
                            let w = bmp.Width / 50
                            let h = bmp.Height / 50
                            for x in 0 .. 49 do
                                for y in 0 .. 49 do
                                    yield bmp.GetPixel(w * x, h * x) }
                        |> Seq.maxBy vibrance
                        |> fun c -> if vibrance c > 127 then Color.FromArgb(255, c) else themeConfig.DefaultAccentColor
                Sprite.upload(bmp, 1, 1, true)
            with err ->
                Logging.Warn("Failed to load background image: " + file) (err.ToString())
                accentColor <- themeConfig.DefaultAccentColor
                getTexture("background")
        | ext ->
            //if ext <> "" then Logging.Error("Unsupported file type for background: " + ext) "" else Logging.Debug("Chart has no background image") ""
            accentColor <- themeConfig.DefaultAccentColor
            getTexture("background")
    
    let loadThemes(themes: List<string>) =
        Logging.Debug("===== Loading Themes =====")""
        loadedNoteskins.Clear()
        loadedThemes.Clear()
        loadedThemes.Add(defaultTheme)
        themeConfig <- ThemeConfig.Default
        Seq.choose (fun t ->
            let theme = Theme.FromThemeFolder(t)
            try
                let config: ThemeConfig = theme.GetJson(false, "theme.json") |> fst
                Some (theme, config)
            with err -> Logging.Error("Failed to load theme '" + t + "'") (err.ToString()); None)
            themes
        |> Seq.iter (fun (t, conf) -> loadedThemes.Add(t); themeConfig <- conf)
    
        loadedThemes
        |> Seq.iteri(fun i t ->
            // this is where we load other stuff like scripting in future
            t.GetNoteSkins()
            |> Seq.iter (fun (ns, c) ->
                loadedNoteskins.Remove(ns) |> ignore // overwrite existing skin with same name
                loadedNoteskins.Add(ns, (c, i))
                Logging.Debug(sprintf "Loaded noteskin %s" ns) ""))
        Seq.iter Sprite.destroy sprites.Values
        sprites.Clear()
        gameplayConfig.Clear()
        changeNoteSkin currentNoteSkin
        if themeConfig.OverrideAccentColor then accentColor <- themeConfig.DefaultAccentColor
        if fontBuilder.IsSome then font().Dispose(); 
        fontBuilder <- Some (lazy (Text.createFont themeConfig.Font))
        Logging.Debug(sprintf "===== Loaded %i themes (%i available) =====" <| loadedThemes.Count - 1 <| availableThemes.Count) ""