namespace Interlude.Content

open Prelude.Data.Library

module private Data =
    let mutable library: Library = Unchecked.defaultof<_>

    let init () : unit =
        library <- Library.Load()

    let deinit () : unit =
        if not (isNull (library :> obj)) then
            library.Save()

    let charts_updated_ev = Event<unit>()
    let charts_updated = charts_updated_ev.Publish