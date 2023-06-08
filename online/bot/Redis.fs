namespace Interlude.Web.Bot

open System
open System.Net.Security
open System.Security.Authentication
open System.Security.Cryptography.X509Certificates
open NRedisStack.RedisStackCommands
open NRedisStack.Search
open NRedisStack.Search.Literals.Enums
open StackExchange.Redis

module Redis = 

    type Role =
        | DEVELOPER = 0
        | MODERATOR = 1
        | DONATOR = 2
        | ALPHATESTER = 3
        | BETATESTER = 4

    type User =
        {
            Name: string
            DateSignedUp: DateTime
            LastLogin: DateTime
            DiscordId: string
            Byte: byte option
            Roles: Set<Role>
        }
        static member Index =
            Schema()
                .AddTextField(new FieldName("$.Name", "name"))
                .AddTextField(new FieldName("$.DiscordId", "discord_id"))

    let redis: ConnectionMultiplexer = 
        let options = new ConfigurationOptions()
        options.EndPoints.Add("localhost", 6379)
        options.User <- "default"
        options.Password <- "redispw"
        options.Ssl <- false
        options.SslProtocols <- SslProtocols.Tls12
        options.add_CertificateSelection(LocalCertificateSelectionCallback(fun _ _ _ _ _ -> new X509Certificate2("localhost.pfx", "DEVELOPMENT")))
        options.add_CertificateValidation(RemoteCertificateValidationCallback(fun _ _ _ _ -> true))
        ConnectionMultiplexer.Connect(options)

    let main() =

        let db = redis.GetDatabase()
        let ft = db.FT()
        let json = db.JSON()

        let user1 = { Name = "Paul John"; DateSignedUp = DateTime.Today.ToUniversalTime(); LastLogin = DateTime.Now.ToUniversalTime(); DiscordId = "1"; Byte = None; Roles = Set.ofSeq [Role.DEVELOPER; Role.MODERATOR] }
        let user2 = { Name = "Paul Zamir"; DateSignedUp = DateTime.Today.ToUniversalTime(); LastLogin = DateTime.Now.ToUniversalTime(); DiscordId = "2"; Byte = Some 127uy; Roles = Set.ofSeq [Role.ALPHATESTER] }

        ft.DropIndex("idx:users") |> ignore
        ft.Create(
            "idx:users",
            FTCreateParams()
                .On(IndexDataType.JSON)
                .Prefix("user:"),
            User.Index
            ) |> printfn "%b"

        let user_id = db.StringIncrement("count:users", 1L)
        json.Set("user:" + user_id.ToString(), "$", user1) |> ignore
        let user_id = db.StringIncrement("count:users", 1L)
        json.Set("user:" + user_id.ToString(), "$", user2) |> ignore

        //json.Get<User>("user:2", "$") |> printfn "%A"
        json.Get<Set<Role>>("user:2", "$.Roles") |> printfn "%A"
        
        printfn "Search results"
        for d in ft.Search("idx:users", new Query("@discord_id:2")).Documents do
            for kvp in d.GetProperties() do
                printfn "%s: %A" kvp.Key kvp.Value