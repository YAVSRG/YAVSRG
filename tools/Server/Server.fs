namespace YAVSRG.CLI.Features

open System.IO
open System.Net.Http
open YAVSRG.CLI.Utils

module Server =

    let run_in_docker () =
        exec_at (Path.Combine(YAVSRG_PATH, "online")) "docker" "compose -p interludeweb up --build --detach"

    let run_domain_tests () =
        exec_at (Path.Combine(YAVSRG_PATH, "online", "tests", "domain")) "dotnet" "test"

    let run_all_tests () =
        exec_at (Path.Combine(YAVSRG_PATH, "online", "tests", "domain")) "dotnet" "test"
        exec_at (Path.Combine(YAVSRG_PATH, "online", "tests", "integration")) "dotnet" "test"
        exec_at (Path.Combine(YAVSRG_PATH, "online", "tests", "client")) "dotnet" "run"

    let generate_certs () =
        exec_at
            (Path.Combine(YAVSRG_PATH, "online", "secrets"))
            "dotnet"
            "dev-certs https --trust -ep localhost.pfx -p DEVELOPMENT"

    let down_detector () =
        use client = new HttpClient()

        let response =
            client.Send(new HttpRequestMessage(HttpMethod.Get, "https://api.yavsrg.net/health"))

        if response.IsSuccessStatusCode then
            printfn "Server OK"
        else
            printfn "!! No reply from the server"

        let response =
            client.Send(
                new HttpRequestMessage(
                    HttpMethod.Head,
                    "https://cdn.yavsrg.net/3769432CBF00E56035035D1D0FAE74DA7313E56E443F32A1C457B0E781E42B6F"
                )
            )

        if response.IsSuccessStatusCode then
            printfn "CDN OK"
        else
            printfn "!! Error from CDN"

        let response =
            client.Send(new HttpRequestMessage(HttpMethod.Head, "https://catboy.best/d/919633n"))

        if response.IsSuccessStatusCode then
            printfn "Mino OK"
        else
            printfn "!! Error from Mino beatmap mirror"

        let response =
            client.Send(
                new HttpRequestMessage(
                    HttpMethod.Head,
                    "https://downloads.etternaonline.com/packs/%23774%20Etterna%20Explosion%20Excitepack.zip"
                )
            )

        if response.IsSuccessStatusCode then
            printfn "EtternaOnline OK"
        else
            printfn "!! Error from EtternaOnline downloads"
