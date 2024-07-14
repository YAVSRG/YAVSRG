namespace Percyqaz.Flux.Audio

[<AutoOpen>]
module private Helpers =
    let display_bass_error b = () // if b then () else Logging.Debug("Bass Error: " + Bass.LastError.ToString(), Environment.StackTrace)

    let mutable internal current_device = -1