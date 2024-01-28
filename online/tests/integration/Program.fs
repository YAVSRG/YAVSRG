namespace Interlude.Web.Tests.Integration

open Interlude.Web.Shared.API
open NUnit.Framework

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

module Program =

    [<EntryPoint>]
    let main _ = 0