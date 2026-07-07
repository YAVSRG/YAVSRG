namespace Prelude.Data.User.Stats

open System
open Percyqaz.Common

[<AbstractClass>]
type Clock() =
    abstract member Now : unit -> int64
    member this.Today() : DateOnly = 
        let today_as_datetime = timestamp_to_rg_calendar_day(this.Now())
        DateOnly.FromDateTime(today_as_datetime)
    
type SystemClock() =
    inherit Clock()
    override this.Now() : int64 = Timestamp.now()