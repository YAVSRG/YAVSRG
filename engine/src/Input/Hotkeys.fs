namespace Percyqaz.Flux.Input

open System.Collections.Generic
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Bind

type Hotkey = string

module Hotkeys =

    let private defaults = Dictionary<string, Bind>()
    let hotkeys = Dictionary<string, Bind>()

    let register (id: string) (value: Bind) =
        defaults.Add(id, value)
        hotkeys.Add(id, value)

    do
        register "none" Bind.Dummy
        register "exit" (mk Keys.Escape)
        register "select" (mk Keys.Enter)
        register "up" (mk Keys.Up)
        register "down" (mk Keys.Down)
        register "left" (mk Keys.Left)
        register "right" (mk Keys.Right)

    let inline get (id: string) = hotkeys.[id]

    let inline set (id: string) (value: Bind) = hotkeys.[id] <- value

    let reset (id: string) =
        if defaults.ContainsKey id then
            hotkeys.[id] <- defaults.[id]
        else
            Logging.Warn "Cannot reset hotkey '%s', no default exists" id

    let reset_all () =
        for id in defaults.Keys do
            reset id

    let import (d: Dictionary<string, Bind>) =
        for k in d.Keys do
            if (hotkeys.Remove k) then
                hotkeys.Add(k, d.[k])

        hotkeys

    let export (d: Dictionary<string, Bind>) =
        d.Clear()

        for k in hotkeys.Keys do
            d.Add(k, hotkeys.[k])

[<AutoOpen>]
module LookupOperator =
    let inline (~%%) (id: string) = Hotkeys.get id