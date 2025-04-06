namespace Interlude.Web.Server.API.Auth

open NetCoreServer
open System.Net.Http
open System.Net.Http.Json
open Percyqaz.Common
open Interlude.Web.Shared
open Interlude.Web.Server
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Services

module Discord =

    let http_client = new HttpClient()

    type DiscordOAuthResponse =
        {
            access_token: string
            token_type: string
        }

    type DiscordIdentityResponse =
        {
            id: string
            username: string
            discriminator: string
        }

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            require_query_parameter query_params "code"
            require_query_parameter query_params "state"

            let code = query_params.["code"].[0]
            let state = query_params.["state"].[0]

            // use code to get an oauth token on behalf of discord user
            let form =
                dict
                    [
                        "client_id", SECRETS.DiscordClientId
                        "client_secret", SECRETS.DiscordClientSecret
                        "grant_type", "authorization_code"
                        "code", code
                        "redirect_uri", "https://" + SECRETS.ApiBaseUrl + "/auth/discord"
                    ]

            let data = new FormUrlEncodedContent(form)
            data.Headers.Clear()
            data.Headers.Add("Content-Type", "application/x-www-form-urlencoded")

            let! oauth_response =
                http_client.PostAsync("https://discord.com/api/oauth2/token", data)
                |> Async.AwaitTask

            if not oauth_response.IsSuccessStatusCode then
                Logging.Error "Discord OAuth request failed: %s" oauth_response.ReasonPhrase

                oauth_response.Content.ReadAsStringAsync()
                |> Async.AwaitTask
                |> Async.RunSynchronously
                |> Logging.Error "%s"

                response.ReplyRedirect("https://yavsrg.net/login_failed")
            else

            let! oauth_data =
                oauth_response.Content.ReadFromJsonAsync<DiscordOAuthResponse>()
                |> Async.AwaitTask

            // use oauth token to get api information about "@me" on behalf of the user
            let identity_request =
                new HttpRequestMessage(HttpMethod.Get, "https://discord.com/api/users/@me")

            identity_request.Headers.Clear()
            identity_request.Headers.Add("Authorization", oauth_data.token_type + " " + oauth_data.access_token)
            let identity_response = http_client.Send(identity_request)

            if not identity_response.IsSuccessStatusCode then
                Logging.Error "Discord Identity request failed: %s" identity_response.ReasonPhrase

                identity_response.Content.ReadAsStringAsync()
                |> Async.AwaitTask
                |> Async.RunSynchronously
                |> Logging.Error "%s"

                response.ReplyRedirect("https://yavsrg.net/login_failed")
            else

            let! identity =
                identity_response.Content.ReadFromJsonAsync<DiscordIdentityResponse>()
                |> Async.AwaitTask

            let discord_tag =
                if identity.discriminator <> "0" then
                    identity.username + "#" + identity.discriminator
                else
                    identity.username

            match Users.DiscordAuthFlow.receive_discord_callback (state, uint64 identity.id, discord_tag) with
            | true -> response.ReplyRedirect("https://yavsrg.net/login_success")
            | false -> response.ReplyRedirect("https://yavsrg.net/login_failed")
        }