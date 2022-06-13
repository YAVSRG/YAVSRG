namespace Percyqaz.Flux.Input

open System.Collections.Generic
open OpenTK.Windowing.GraphicsLibraryFramework
open Bind

type Hotkey = string

module Hotkeys =

    let private defaults = Dictionary<string, Bind>()
    let private hotkeys = Dictionary<string, Bind>()

    let register (id: string) (value: Bind) =
        defaults.Add(id, value)
        hotkeys.Add(id, value)

    let inline get (id: string) = hotkeys.[id]

    let reset (id: string) =
        hotkeys.[id] <- defaults.[id]

    let init() =
        register "exit" (mk Keys.Escape)
        register "select" (mk Keys.Enter)
        register "up" (mk Keys.Up)
        register "down" (mk Keys.Down)
        register "left" (mk Keys.Left)
        register "right" (mk Keys.Right)

[<AutoOpen>]
module Helpers =
    
    // The hotkey lookup operator
    let inline (!|) (id: string) = Hotkeys.get id