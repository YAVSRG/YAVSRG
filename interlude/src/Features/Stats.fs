namespace Interlude.Features

open System

module Stats =

    let session_start = DateTime.Now

    let session_length() = 
        let ts = DateTime.Now - session_start
        sprintf "%02i:%02i:%02i" (ts.TotalHours |> floor |> int) ts.Minutes ts.Seconds