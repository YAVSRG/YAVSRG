namespace YAVSRG.CLI.Features

open System.IO
open System.Net.Http
open YAVSRG.CLI.Utils

module Server =

    let run_in_docker () : unit =
        printfn "Running Interlude server via docker"
        
        let data_path = Path.Combine(YAVSRG_PATH, "online", "data")
        if not (Directory.Exists(data_path)) then
            printfn "First time setup: Creating data folder @ %s" data_path
            Directory.CreateDirectory(data_path) |> ignore
        
        let secrets_path = Path.Combine(YAVSRG_PATH, "online", "secrets")
        if not (Directory.Exists(secrets_path)) then
            printfn "First time setup: Creating secrets folder @ %s" secrets_path
            Directory.CreateDirectory(secrets_path) |> ignore
            
        let cert_path = Path.Combine(secrets_path, "localhost.pfx")
        if not (File.Exists(cert_path)) then
            printfn "First time setup: Creating and trusting local certificate @ %s" cert_path
            exec_at
                secrets_path
                "dotnet"
                "dev-certs https --trust -ep localhost.pfx -p DEVELOPMENT"
            
        exec_at (Path.Combine(YAVSRG_PATH, "online")) "docker" "compose -p interludeweb up --build --detach"
        
    let run_local () : unit =
        printfn "Running Interlude server locally (not in a docker container)"
        
        let data_path = Path.Combine(YAVSRG_PATH, "online", "server", "bin", "Debug", DOTNET_VERSION, "data")
        if not (Directory.Exists(data_path)) then
            printfn "First time setup: Creating data folder @ %s" data_path
            Directory.CreateDirectory(data_path) |> ignore
        
        let secrets_path = Path.Combine(YAVSRG_PATH, "online", "server", "bin", "Debug", DOTNET_VERSION, "secrets")
        if not (Directory.Exists(secrets_path)) then
            printfn "First time setup: Creating secrets folder @ %s" secrets_path
            Directory.CreateDirectory(secrets_path) |> ignore
            
        let cert_path = Path.Combine(secrets_path, "localhost.pfx")
        if not (File.Exists(cert_path)) then
            printfn "First time setup: Creating and trusting local certificate @ %s" cert_path
            exec_at
                secrets_path
                "dotnet"
                "dev-certs https --trust -ep localhost.pfx -p DEVELOPMENT"
                
        exec_at (Path.Combine(YAVSRG_PATH, "online", "server")) "dotnet" "build --configuration Debug -v q"
        exec_at (Path.Combine(YAVSRG_PATH, "online", "server", "bin", "Debug", DOTNET_VERSION))
            "dotnet"
            "run --project ../../../Interlude.Web.Server.fsproj --configuration Debug -v q"

    let run_domain_tests () : unit =
        exec_at (Path.Combine(YAVSRG_PATH, "online", "tests", "domain")) "dotnet" "test"

    let run_all_tests () : unit =
        exec_at (Path.Combine(YAVSRG_PATH, "online", "tests", "domain")) "dotnet" "test"
        exec_at (Path.Combine(YAVSRG_PATH, "online", "tests", "integration")) "dotnet" "test"
        exec_at (Path.Combine(YAVSRG_PATH, "online", "tests", "client")) "dotnet" "run"

    let down_detector () : unit =
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
