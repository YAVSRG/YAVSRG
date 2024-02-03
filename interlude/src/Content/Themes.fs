namespace Interlude.Content

open System
open System.IO
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Prelude
open Prelude.Data.Content
open Interlude

module Themes =

    let private DEFAULT_ID = "*default"
    let private DEFAULT = Theme.FromZipStream <| Utils.get_resource_stream "default.zip"

    let mutable private initialised = false
    let private loaded = Dictionary<string, Theme>()
    let mutable current = DEFAULT
    let mutable current_config = current.Config
    let private _selected_id = Setting.simple DEFAULT_ID

    let private load () =
        loaded.Clear()
        loaded.Add("*default", DEFAULT)

        for source in Directory.EnumerateDirectories(get_game_folder "Themes") do
            let id = Path.GetFileName source

            try
                let theme = Theme.FromFolderName id
                Logging.Debug(sprintf "  Loaded theme '%s' (%s)" theme.Config.Name id)
                loaded.Add(id, theme)
            with err ->
                Logging.Error("  Failed to load theme '" + id + "'", err)

        Logging.Info(sprintf "Loaded %i themes. (Including default)" loaded.Count)

    let save_config (new_config: ThemeConfig) =
        current.Config <- new_config
        current_config <- current.Config

    let reload_current () =
        let missing_textures = ResizeArray()
        let available_textures = ResizeArray()

        for id in Storage.THEME_TEXTURES do
            Sprites.remove id

            match current.GetTexture id with
            | Some(img, config) ->
                available_textures.Add
                    {
                        Label = id
                        Image = img
                        Rows = config.Rows
                        Columns = config.Columns
                        DisposeImageAfter = true
                    }
            | None ->
                Logging.Warn(
                    sprintf
                        "Theme texture '%s' didn't load properly, so it will appear as a white square ingame."
                        id
                )
                // todo: fall back to default theme textures like it used to
                missing_textures.Add id

        let atlas, sprites =
            Sprite.upload_many "THEME" true false (available_textures.ToArray())

        for id, sprite in sprites do
            Sprites.add id sprite

        for id in missing_textures do
            Sprites.add id (Texture.create_default_sprite atlas)

        for id in Storage.THEME_SOUNDS do
            match current.GetSound id with
            | Some stream -> SoundEffect.FromStream(id, stream) |> Sounds.add id
            | None ->
                match loaded.["*default"].GetSound id with
                | Some stream -> SoundEffect.FromStream(id, stream) |> Sounds.add id
                | None -> failwithf "Failed to load sound '%s' from *default" id

        if current_config.OverrideAccentColor then
            Palette.accent_color.Target <- current_config.DefaultAccentColor

        for font in DEFAULT.GetFonts() do
            Fonts.add font

        for font in current.GetFonts() do
            Fonts.add font

        if Style.font <> null then
            Style.font.Dispose()

        Style.font <- Fonts.create current_config.Font

        Style.click <- Sounds.get "click"
        Style.hover <- Sounds.get "hover"
        Style.text_close <- Sounds.get "text-close"
        Style.text_open <- Sounds.get "text-open"
        Style.key <- Sounds.get "key"

        Style.notify_error <- Sounds.get "notify-error"
        Style.notify_info <- Sounds.get "notify-info"
        Style.notify_system <- Sounds.get "notify-system"
        Style.notify_task <- Sounds.get "notify-task"
    
    let init_window () =
        load ()
    
        if not (loaded.ContainsKey _selected_id.Value) then
            Logging.Warn("Theme '" + _selected_id.Value + "' not found, switching to default")
            _selected_id.Value <- DEFAULT_ID
        current <- loaded.[_selected_id.Value]
        current_config <- current.Config
        reload_current()
        initialised <- true

    let selected_id =
        Setting.make (fun new_id ->
            if initialised then
                let old_id = _selected_id.Value
                if not (loaded.ContainsKey new_id) then
                    Logging.Warn("Theme '" + new_id + "' not found, switching to default")
                    _selected_id.Value <- DEFAULT_ID
                else _selected_id.Value <- new_id
                
                if _selected_id.Value <> old_id then
                    current <- loaded.[_selected_id.Value]
                    current_config <- current.Config
                    reload_current()
            else
                _selected_id.Value <- new_id
        ) (fun () -> _selected_id.Value)

    let list () =
        loaded |> Seq.map (fun kvp -> (kvp.Key, kvp.Value.Config.Name)) |> Array.ofSeq

    let create_new (id: string) : bool =
        let id = Text.RegularExpressions.Regex("[^a-zA-Z0-9_-]").Replace(id, "")
        let target = Path.Combine(get_game_folder "Themes", id)

        if id <> "" && not (Directory.Exists target) then
            DEFAULT.ExtractToFolder(target)
            load ()
            selected_id.Value <- id

            save_config
                { current_config with
                    Name = current_config.Name + " (Extracted)"
                }
            true
        else
            false