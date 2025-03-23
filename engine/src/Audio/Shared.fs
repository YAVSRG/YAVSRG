namespace Percyqaz.Flux.Audio

open System
open ManagedBass
open Percyqaz.Common

[<AutoOpen>]
module private Helpers =

    let display_bass_error<'T> (return_value: 'T) : unit = () //if return_value then () else Logging.Debug "Bass Error: %O\n%O" Bass.LastError Environment.StackTrace