namespace Interlude.Web.Server.Bot

open Interlude.Web.Server

[<AutoOpen>]
module Constants =
    let GUILD_ID = 395040265112518669uL
    let PERCYQAZ_ID = 165506274820096000uL

    let MAIN_CHANNEL_ID =
        if SECRETS.IsProduction then
            1108884675298078811uL
        else
            1116491396443025578uL

    let ADMIN_CHANNEL_ID =
        if SECRETS.IsProduction then
            1116491477640564756uL
        else
            1116491426067394661uL

    let FEED_CHANNEL_ID =
        if SECRETS.IsProduction then
            1074467175571656785uL
        else
            1116491426067394661uL
