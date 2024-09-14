namespace Interlude.Content

open Percyqaz.Common
open Percyqaz.Flux.UI

type Content() =

    static member init_startup() = Data.init_startup ()

    static member init_window() =
        Logging.Info "===== Loading game content ====="
        Tables.init_window ()
        Rulesets.init_window ()
        Themes.init_window ()
        Skins.init_window ()
        Sounds.init_window ()

    static member deinit() = Data.deinit ()

    static member Scores = Data.user_db
    static member Library = Data.library
    static member Cache = Data.library.Cache
    static member Collections = Data.library.Collections

    static member Table = Tables.current
    static member ThemeConfig = Themes.current_config
    static member Theme = Themes.current
    static member NoteskinConfig = Skins.current_noteskin.Config
    static member Noteskin = Skins.current_noteskin
    static member NoteskinMeta = Skins.current_noteskin_meta
    static member HUD = Skins.current_hud.Config
    static member HUDMeta = Skins.current_hud_meta
    static member Texture(id: string) = Sprites.get id

    static member OnChartAdded = Data.charts_updated
    static member TriggerChartAdded() = ensure_ui_thread Data.charts_updated_ev.Trigger
