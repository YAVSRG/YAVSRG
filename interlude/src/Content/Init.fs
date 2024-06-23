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
        Noteskins.init_window ()

    static member deinit() = Data.deinit ()

    static member Scores = Data.score_db
    static member Library = Data.library
    static member Cache = Data.library.Cache
    static member Collections = Data.library.Collections

    static member Table = Tables.current
    static member ThemeConfig = Themes.current_config
    static member Theme = Themes.current
    static member NoteskinConfig = Noteskins.current.Config
    static member Noteskin = Noteskins.current
    static member Texture(id: string) = Sprites.get id

    static member OnChartAdded = Data.charts_updated
    static member TriggerChartAdded() = ensure_ui_thread Data.charts_updated_ev.Trigger
