namespace Interlude.Web.Tests.Integration

open NUnit.Framework
open Interlude.Web.Shared.Requests
open Interlude.Web.Shared.API
open System.Threading

[<AutoOpen>]
module TestConstants =
    let CRESCENT_MOON = "1467CD6DEB4A3B87FA58FAB4F2398BE9AD7B0017031C511C549D3EF28FFB58D3"
    let SCJ4 = "SC(J4)548E5A"
    let USERNAME = "Integration"

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

    module Charts =

        [<Test>]
        let Identify_KnownChart () =
            
            use done_signal = new AutoResetEvent(false)

            Charts.Identify.get (CRESCENT_MOON, Option.get >> fun (res: Charts.Identify.Response) -> 
                printfn "%A" res.Info.Value
                done_signal.Set() |> ignore
            )

            Assert.IsTrue(done_signal.WaitOne(500))
            
        [<Test>]
        let Identify_UnknownChart () =
                        
            use done_signal = new AutoResetEvent(false)
            
            Charts.Identify.get ("NOTAVALIDCHARTID", Option.get >> fun (res: Charts.Identify.Response) -> 
                match res.Info with Some info -> failwithf "Unexpected data %A" info | None -> ()
                done_signal.Set() |> ignore
            )
            
            Assert.IsTrue(done_signal.WaitOne(500))

        module Scores =

            [<Test>]
            let Leaderboard () =

                use done_signal = new AutoResetEvent(false)
            
                Charts.Scores.Leaderboard.get (CRESCENT_MOON, SCJ4, Option.get >> fun (res: Charts.Scores.Leaderboard.Response) -> 
                    printfn "%A" res.RulesetId
                    printfn "%A" res.Scores
                    done_signal.Set() |> ignore
                )
            
                Assert.IsTrue(done_signal.WaitOne(500))

    module Tables =
        
        [<Test>]
        let Records () =
            use done_signal = new AutoResetEvent(false)
                        
            Tables.Records.get (USERNAME, "crescent", Option.get >> fun (res: Tables.Records.Response) -> 
                printfn "%A" res.Scores
                done_signal.Set() |> ignore
            )
                        
            Assert.IsTrue(done_signal.WaitOne(500))
            
        [<Test>]
        let Leaderboard () =
            use done_signal = new AutoResetEvent(false)
                            
            Tables.Leaderboard.get ("crescent", Option.get >> fun (res: Tables.Leaderboard.Response) -> 
                printfn "%A" res.Players
                done_signal.Set() |> ignore
            )
                            
            Assert.IsTrue(done_signal.WaitOne(500))

        [<Test>]
        let List () =
            use done_signal = new AutoResetEvent(false)
                        
            Tables.List.get (Option.get >> fun (res: Tables.List.Response) -> 
                printfn "%A" res.Tables
                done_signal.Set() |> ignore
            )
              
            // This test fails because the endpoint doesn't exist!
            Assert.IsTrue(done_signal.WaitOne(500))
            
        module Suggestions =

            [<Test>]
            let List () =
                use done_signal = new AutoResetEvent(false)
                                    
                Tables.Suggestions.List.get ("crescent", Option.get >> fun (res: Tables.Suggestions.List.Response) -> 
                    printfn "%A" res.Suggestions
                    done_signal.Set() |> ignore
                )
                                    
                Assert.IsTrue(done_signal.WaitOne(500))
                
            [<Test>]
            let Missing () =
                use done_signal = new AutoResetEvent(false)
                                                    
                Tables.Suggestions.Missing.get ("crescent", Option.get >> fun (res: Tables.Suggestions.Missing.Response) -> 
                    printfn "%A" res.Suggestions
                    done_signal.Set() |> ignore
                )
                                                    
                Assert.IsTrue(done_signal.WaitOne(500))

            [<Test>]
            let Preview () =
                use done_signal = new AutoResetEvent(false)
                                                    
                Tables.Suggestions.Preview.get ("crescent", Option.get >> fun (res: Tables.Suggestions.Preview.Response) -> 
                    printfn "%A" res.Table
                    done_signal.Set() |> ignore
                )
                                                    
                Assert.IsTrue(done_signal.WaitOne(500))

    module Players =

        [<Test>]
        let Online () =
            use done_signal = new AutoResetEvent(false)
                                                    
            Players.Online.get (Option.get >> fun (res: Players.Online.Response) -> 
                printfn "%A" res.Players
                done_signal.Set() |> ignore
            )
                                                    
            Assert.IsTrue(done_signal.WaitOne(500))
            
        [<Test>]
        let Search () =
            use done_signal = new AutoResetEvent(false)
                                                                
            Players.Search.get (USERNAME, Option.get >> fun (res: Players.Search.Response) -> 
                printfn "%A" res.Matches
                done_signal.Set() |> ignore
            )
                                                                
            Assert.IsTrue(done_signal.WaitOne(500))
        
        module Profile =

            [<Test>]
            let View_Me () =
                use done_signal = new AutoResetEvent(false)
                                                            
                Players.Profile.View.get_me (Option.get >> fun (res: Players.Profile.View.Response) -> 
                    printfn "%A" res
                    done_signal.Set() |> ignore
                )
                                                            
                Assert.IsTrue(done_signal.WaitOne(500))
                
            [<Test>]
            let View () =
                use done_signal = new AutoResetEvent(false)
                                                                            
                Players.Profile.View.get (USERNAME, Option.get >> fun (res: Players.Profile.View.Response) -> 
                    printfn "%A" res
                    done_signal.Set() |> ignore
                )
                                                                            
                Assert.IsTrue(done_signal.WaitOne(500))

    module Friends =

        [<Test>]
        let List () =
            use done_signal = new AutoResetEvent(false)
                                                    
            Friends.List.get (Option.get >> fun (res: Friends.List.Response) -> 
                printfn "%A" res.Friends
                done_signal.Set() |> ignore
            )
                                                    
            Assert.IsTrue(done_signal.WaitOne(500))

        [<Test>]
        let Add () =
            use done_signal = new AutoResetEvent(false)
                                                            
            Friends.Add.post ({ User = USERNAME }, Option.get >> fun (res: bool) -> 
                printfn "%A" res
                done_signal.Set() |> ignore
            )
                                                            
            Assert.IsTrue(done_signal.WaitOne(500))

        [<Test>]
        let Remove () =
            use done_signal = new AutoResetEvent(false)
                                                                    
            Friends.Remove.delete (USERNAME, Option.get >> fun (res: bool) -> 
                printfn "%A" res
                done_signal.Set() |> ignore
            )
                                                                    
            Assert.IsTrue(done_signal.WaitOne(500))