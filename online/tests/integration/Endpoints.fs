namespace Interlude.Web.Tests.Integration

open NUnit.Framework
open Interlude.Web.Shared.Requests
open Interlude.Web.Shared.API
open System.Threading

module Endpoints =
    
    [<SetUpFixture>]
    type Setup() =

        [<OneTimeSetUp>]
        member _.InitAndAuth() =
            Client.init "https://localhost/"
            let http_client = new System.Net.Http.HttpClient()
            task {
                let! response = http_client.GetStringAsync("https://localhost/auth/dummy?username=Integration")
                let token =
                    match Prelude.Common.JSON.FromString<string>(response) with
                    | Ok t -> t
                    | _ -> failwithf "Failed to get auth token from dummy endpoint, instead got %s" response
                Client.authenticate token
            } |> Async.AwaitTask |> Async.RunSynchronously

    module Health =

        [<Test>]
        let Status () =
            use done_signal = new AutoResetEvent(false)

            Health.Status.get (Option.get >> fun (res: Health.Status.Response) -> 
                printfn "%s" res.Status
                done_signal.Set() |> ignore
            )

            Assert.IsTrue(done_signal.WaitOne(500))
