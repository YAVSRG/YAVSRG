namespace Interlude.Web.Tests.Integration

open NUnit.Framework
open Interlude.Web.Shared.Requests
open System.Threading

module Players =

    [<Test>]
    let Online () =
        use done_signal = new AutoResetEvent(false)

        Players.Online.get (
            Option.get
            >> fun (res: Players.Online.Response) ->
                printfn "%A" res.Players
                done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Search () =
        use done_signal = new AutoResetEvent(false)

        Players.Search.get (
            USERNAME,
            Option.get
            >> fun (res: Players.Search.Response) ->
                printfn "%A" res.Matches
                done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    module Profile =

        [<Test>]
        let View_Me () =
            use done_signal = new AutoResetEvent(false)

            Players.Profile.View.get_me (
                Option.get
                >> fun (res: Players.Profile.View.Response) ->
                    printfn "%A" res
                    done_signal.Set() |> ignore
            )

            Assert.IsTrue(done_signal.WaitOne(500))

        [<Test>]
        let View () =
            use done_signal = new AutoResetEvent(false)

            Players.Profile.View.get (
                USERNAME,
                Option.get
                >> fun (res: Players.Profile.View.Response) ->
                    printfn "%A" res
                    done_signal.Set() |> ignore
            )

            Assert.IsTrue(done_signal.WaitOne(500))
