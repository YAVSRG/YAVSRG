namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Input

[<Sealed>]
type HotkeyListener(hotkey: Hotkey, action: unit -> unit) =
    inherit StaticWidget(NodeType.None)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if (%%hotkey).Pressed() then
            action ()

    override this.Draw() = ()

// todo: remove and put a 'delay' setting on the main HotkeyListener ?
[<Sealed>]
type HotkeyHoldAction(hotkey: Hotkey, on_tap: unit -> unit, on_hold: unit -> unit) =
    inherit StaticWidget(NodeType.None)

    let HOLD_TIME_MS = 200.0

    let mutable hold_time_remaining = 0.0

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if (%%hotkey).Pressed() then
            on_tap ()
            hold_time_remaining <- HOLD_TIME_MS

        if (%%hotkey).Held() then
            if hold_time_remaining > 0.0 then
                hold_time_remaining <- hold_time_remaining - elapsed_ms

                if hold_time_remaining < 0 then
                    on_hold ()

    override this.Draw() = ()