namespace Interlude.Web.Server.API.Auth

open System.Net.Http
open System.Net.Http.Json
open Prelude
open Percyqaz.Common
open Percyqaz.Json
open Interlude.Web.Shared.API
open Interlude.Web.Server
open Interlude.Web.Server.Domain

module Discord =

    let ROUTE = (GET, "/auth/discord")

    [<Json.AutoCodec>]
    type Response = { Status: string }

    let http_client = new HttpClient()

    type DiscordOAuthResponse = { access_token: string; token_type: string }
    type DiscordIdentityResponse = { id: string; username: string; discriminator: string }

    let handle (body: string, query_params: Map<string, string array>, headers: Map<string, string>) : Async<Response> = 
        async {
            let code = query_params.["code"].[0]
            let state = query_params.["state"].[0]

            // use code to get an oauth token on behalf of discord user
            let form = 
                dict [
                    "client_id", SECRETS.DiscordClientId
                    "client_secret", SECRETS.DiscordClientSecret
                    "grant_type", "authorization_code"
                    "code", code
                    "redirect_uri", "https://localhost/auth/discord"
                ]

            let data = new FormUrlEncodedContent(form)
            data.Headers.Clear()
            data.Headers.Add("Content-Type", "application/x-www-form-urlencoded")

            let! oauth_response = http_client.PostAsync("https://discord.com/api/oauth2/token", data) |> Async.AwaitTask

            if not oauth_response.IsSuccessStatusCode then
                Logging.Error(sprintf "Discord OAuth request failed: %s" oauth_response.ReasonPhrase)
                return { Status = "Discord OAuth error" }
            else

            let! response = oauth_response.Content.ReadFromJsonAsync<DiscordOAuthResponse>() |> Async.AwaitTask

            // use oauth token to get api information about "@me" on behalf of the user
            let identity_request = new HttpRequestMessage(HttpMethod.Get, "https://discord.com/api/users/@me")
            identity_request.Headers.Clear()
            identity_request.Headers.Add("Authorization", response.token_type + " " + response.access_token)
            let identity_response = http_client.Send(identity_request)

            if not identity_response.IsSuccessStatusCode then
                Logging.Error(sprintf "Discord Identity request failed: %s" identity_response.ReasonPhrase)
                return { Status = "Discord Identity error" }
            else

            let! response = identity_response.Content.ReadFromJsonAsync<DiscordIdentityResponse>() |> Async.AwaitTask

            match! AuthFlow.receive_discord_callback(state, int64 response.id, response.username + "#" + response.discriminator) with
            | Ok () -> return { Status = "The last step registration process is now available in your Interlude client. You can now safely close this window" }
            | Error e -> return { Status = e }
        }