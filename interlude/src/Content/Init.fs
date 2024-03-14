namespace Interlude.Content

open Percyqaz.Common
open Prelude.Data.Charts

type Content () =

    static member init_startup () =
        Library.init_startup ()
        Data.init_startup ()

    static member init_window () =
        Logging.Info "===== Loading game content ====="
        Tables.init_window ()
        Rulesets.init_window ()
        Themes.init_window ()
        Noteskins.init_window ()

    static member deinit () =
        Library.deinit ()
        Data.deinit ()

    static member Scores = Data.score_db

    static member Table = Tables.current
    static member ThemeConfig = Themes.current_config
    static member Theme = Themes.current
    static member NoteskinConfig = Noteskins.current_config
    static member Noteskin = Noteskins.current
    static member Texture (id: string) = Sprites.get id