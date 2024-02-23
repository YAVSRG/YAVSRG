open NUnit.Framework

[<SetUpFixture>]
type Setup() =

    [<OneTimeSetUp>]
    member _.Setup() = ()

    [<OneTimeTearDown>]
    member _.Teardown() = ()

module Program =

    [<EntryPoint>]
    let main _ = 0
