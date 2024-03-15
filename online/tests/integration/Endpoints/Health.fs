namespace Interlude.Web.Tests.Integration

open NUnit.Framework
open Interlude.Web.Shared.Requests
open System.Threading

module Health =

    [<Test>]
    let Status () =
        use done_signal = new AutoResetEvent(false)

        Health.Status.get (
            Option.get
            >> fun (res: Health.Status.Response) ->
                printfn "%s" res.Status
                done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))
