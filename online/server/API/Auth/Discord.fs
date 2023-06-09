namespace Interlude.Web.Server.API.Auth

open NetCoreServer
open System.Net.Http
open System.Net.Http.Json
open Percyqaz.Common
open Interlude.Web.Shared.API
open Interlude.Web.Server
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain

module Discord =

    let ROUTE = (GET, "/auth/discord")

    let http_client = new HttpClient()

    type DiscordOAuthResponse = { access_token: string; token_type: string }
    type DiscordIdentityResponse = { id: string; username: string; discriminator: string }

    let handle (body: string, query_params: Map<string, string array>, headers: Map<string, string>, response: HttpResponse) = 
        async {
            if not (query_params.ContainsKey "code" && query_params.ContainsKey "state") then
                response.MakeErrorResponse(400) |> ignore
            else

            let code = query_params.["code"].[0]
            let state = query_params.["state"].[0]

            // use code to get an oauth token on behalf of discord user
            let form = 
                dict [
                    "client_id", SECRETS.DiscordClientId
                    "client_secret", SECRETS.DiscordClientSecret
                    "grant_type", "authorization_code"
                    "code", code
                    "redirect_uri", "https://" + SECRETS.ApiBaseUrl + "/auth/discord"
                ]

            let data = new FormUrlEncodedContent(form)
            data.Headers.Clear()
            data.Headers.Add("Content-Type", "application/x-www-form-urlencoded")

            let! oauth_response = http_client.PostAsync("https://discord.com/api/oauth2/token", data) |> Async.AwaitTask

            if not oauth_response.IsSuccessStatusCode then
                Logging.Error(sprintf "Discord OAuth request failed: %s" oauth_response.ReasonPhrase)
                Logging.Error(oauth_response.Content.ReadAsStringAsync() |> Async.AwaitTask |> Async.RunSynchronously)
                response.ReplyRedirect("https://yavsrg.net/login_failed")
            else

            let! oauth_data = oauth_response.Content.ReadFromJsonAsync<DiscordOAuthResponse>() |> Async.AwaitTask

            // use oauth token to get api information about "@me" on behalf of the user
            let identity_request = new HttpRequestMessage(HttpMethod.Get, "https://discord.com/api/users/@me")
            identity_request.Headers.Clear()
            identity_request.Headers.Add("Authorization", oauth_data.token_type + " " + oauth_data.access_token)
            let identity_response = http_client.Send(identity_request)

            if not identity_response.IsSuccessStatusCode then
                Logging.Error(sprintf "Discord Identity request failed: %s" identity_response.ReasonPhrase)
                Logging.Error(identity_response.Content.ReadAsStringAsync() |> Async.AwaitTask |> Async.RunSynchronously)
                response.ReplyRedirect("https://yavsrg.net/login_failed")
            else

            let! identity = identity_response.Content.ReadFromJsonAsync<DiscordIdentityResponse>() |> Async.AwaitTask

            match! AuthFlow.receive_discord_callback(state, uint64 identity.id, identity.username + "#" + identity.discriminator) with
            | true -> response.ReplyRedirect("https://yavsrg.net/login_success") // todo: this page needs to have more info
            | false -> response.ReplyRedirect("https://yavsrg.net/login_failed")
        }