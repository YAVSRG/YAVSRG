namespace Interlude.Content

open Percyqaz.Common
open Prelude.Data.Library
open Prelude.Data.User.Stats

module private Data =
    let mutable library: Library = Unchecked.defaultof<_>
    let mutable stats: Stats = Unchecked.defaultof<_>

    let init () : unit =
        library <- Library.Load()
        stats <- Stats.FromLibrary(library)

    let deinit () : unit =
        if not (isNull (stats :> obj)) then
            stats.SaveCurrentSession(Timestamp.now())
        if not (isNull (library :> obj)) then
            library.Save()

    let charts_updated_ev = Event<unit>()
    let charts_updated = charts_updated_ev.Publish