namespace Prelude.Tests.Helpers

open Prelude.Data.User.Stats

type VirtualClock(start_time: int64) =
    inherit Clock()
    let mutable time = start_time
    override this.Now() : int64 = time
    
    member this.Add(milliseconds: int64) : unit =
        time <- time + milliseconds
        
    member inline this.AddSeconds(seconds: int64) : unit =
        this.Add(seconds * 1000L)
        
    member inline this.AddMinutes(minutes: int64) : unit =
        this.AddSeconds(minutes * 60L)