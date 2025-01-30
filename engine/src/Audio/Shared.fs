namespace Percyqaz.Flux.Audio

open System
open ManagedBass
open Percyqaz.Common

[<AutoOpen>]
module private Helpers =

    let display_bass_error b = () //if b then () else Logging.Debug "Bass Error: %O\n%O" Bass.LastError Environment.StackTrace