namespace Interlude.Web.Tests.Integration

open NUnit.Framework
open Interlude.Web.Shared.Requests
open System.Threading

module Friends =

    [<Test>]
    let List () =
        use done_signal = new AutoResetEvent(false)

        Friends.List.get (
            Option.get
            >> fun (res: Friends.List.Response) ->
                printfn "%A" res.Friends
                done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Add () =
        use done_signal = new AutoResetEvent(false)

        Friends.Add.post (
            { User = USERNAME },
            Option.get
            >> fun (res: bool) ->
                printfn "%A" res
                done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Remove () =
        use done_signal = new AutoResetEvent(false)

        Friends.Remove.delete (
            USERNAME,
            Option.get
            >> fun (res: bool) ->
                printfn "%A" res
                done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))
