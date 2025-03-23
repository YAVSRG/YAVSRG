namespace Interlude.Content

open System
open System.IO
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Prelude
open Prelude.Skins
open Prelude.Skins.Themes
open Interlude

module Themes =

    /// Theme IDs are: *default (represents embedded theme), or the name of a folder under Themes
    let private DEFAULT_ID = "*default"
    let private DEFAULT : Theme = Theme.FromZipStream <| Utils.get_resource_stream "default.zip"

    let mutable private initialised = false
    let private loaded = Dictionary<string, Theme>()
    let mutable current = DEFAULT
    let mutable current_config = current.Config
    let private _selected_id = Setting.simple DEFAULT_ID

    /// Once called: All themes from the Themes folder have been read; The valid ones are loaded into the available list
    /// Can be called multiple times to re-load with the latest data
    let private load () =
        loaded.Clear()
        loaded.Add("*default", DEFAULT)

        for source in Directory.EnumerateDirectories(get_game_folder "Themes") do
            let id = Path.GetFileName source

            try
                if id = DEFAULT_ID then failwith "How did we get here?"
                let theme = Theme.FromFolderName id
                loaded.Add(id, theme)
            with err ->
                Logging.Error "  Failed to load theme '%s': %O" id err

    let save_config (new_config: ThemeConfig) : unit =
        current.Config <- new_config
        current_config <- current.Config

    /// (Re)loads all textures, sounds, fonts from the currently active theme
    let private load_current () : unit =
        let missing_textures = ResizeArray()
        let available_textures = ResizeArray()

        for id in Theme.TEXTURES do
            Sprites.remove id

            match current.GetTexture id with
            | TextureOk(img, columns, rows) ->
                available_textures.Add
                    {
                        Label = id
                        Image = img
                        Rows = rows
                        Columns = columns
                        DisposeImageAfter = true
                    }
            | TextureError reason ->
                Logging.Error "Problem with theme texture '%s': %s\nIt will appear as a white square ingame." id reason
                // todo: fall back to default theme textures like it used to
                missing_textures.Add id
            | TextureNotRequired -> failwith "currently impossible as all theme textures are required"

        let atlas, sprites =
            Sprite.upload_many "THEME" true false (available_textures.ToArray())

        for id, sprite in sprites do
            Sprites.add true id sprite

        for id in missing_textures do
            Sprites.add true id (Texture.create_default_sprite atlas)

        for id in Theme.SOUNDS do
            match current.GetSound id with
            | Some stream -> SoundEffect.FromStream(id, stream) |> Sounds.add id
            | None ->
                match loaded.["*default"].GetSound id with
                | Some stream -> SoundEffect.FromStream(id, stream) |> Sounds.add id
                | None -> failwithf "Failed to load sound '%s' from *default" id

        if current_config.AlwaysUseDefaultAccentColor then
            Palette.accent_color.Target <- current_config.DefaultAccentColor

        for font in DEFAULT.GetFonts() do
            Fonts.add font

        for font in current.GetFonts() do
            Fonts.add font

        if not (Object.ReferenceEquals(Style.font, null)) then
            Style.font.Dispose()

        Style.font <- Fonts.create current_config.Font SpriteFontOptions.Default

        Style.click <- Sounds.get "click"
        Style.hover <- Sounds.get "hover"
        Style.text_close <- Sounds.get "text-close"
        Style.text_open <- Sounds.get "text-open"
        Style.key <- Sounds.get "key"

        Style.notify_error <- Sounds.get "notify-error"
        Style.notify_info <- Sounds.get "notify-info"
        Style.notify_system <- Sounds.get "notify-system"
        Style.notify_task <- Sounds.get "notify-task"

    /// Reloads the theme config, all textures, sounds, fonts from the currently active theme
    let reload_current() =
        current.ReloadFromDisk()
        current_config <- current.Config
        load_current()

    let init () =
        load ()

        if not (loaded.ContainsKey _selected_id.Value) then
            Logging.Warn "Theme '%s' not found, switching to default" _selected_id.Value
            _selected_id.Value <- DEFAULT_ID

        current <- loaded.[_selected_id.Value]
        current_config <- current.Config
        load_current ()
        initialised <- true

    let selected_id =
        Setting.make
            (fun new_id ->
                if initialised then
                    let old_id = _selected_id.Value

                    if not (loaded.ContainsKey new_id) then
                        Logging.Warn "Theme '%s' not found, switching to default" new_id
                        _selected_id.Value <- DEFAULT_ID
                    else
                        _selected_id.Value <- new_id

                    if _selected_id.Value <> old_id then
                        current <- loaded.[_selected_id.Value]
                        current_config <- current.Config
                        load_current ()
                else
                    _selected_id.Value <- new_id
            )
            (fun () -> _selected_id.Value)

    /// Each theme ID and its name, in no particular order
    let list () : (string * string) array =
        loaded |> Seq.map (fun kvp -> (kvp.Key, kvp.Value.Config.Name)) |> Array.ofSeq

    let create_new (id: string) : bool =
        let id = Text.RegularExpressions.Regex("[^a-zA-Z0-9_-]").Replace(id, "")
        let target = Path.Combine(get_game_folder "Themes", id)

        if id <> "" && not (Directory.Exists target) then
            DEFAULT.ExtractToFolder(target) |> ignore
            load ()
            selected_id.Value <- id

            save_config
                { current_config with
                    Name = current_config.Name + " (Extracted)"
                }

            true
        else
            false