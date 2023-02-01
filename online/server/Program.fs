open System
open Interlude.Web.Shared

let handle_packet(id: Guid, packet: Upstream) =
    printfn "%O >> %A" id packet 

Server.init { Address = "0.0.0.0"; Port = 32767; Handle_Packet = handle_packet }

Server.start()

Console.ReadLine() |> ignore

Server.stop()