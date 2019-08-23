using System;
using DiscordRPC;
using Prelude.Utilities;

namespace Interlude.IO
{
    public class Discord
    {
        static DiscordRpcClient client;

        public static void Update()
        {
            client.Invoke();
        }

        public static void Init()
        {
            client = new DiscordRpcClient("420320424199716864", true);
            client.OnJoinRequested += JoinRequest;
            client.Initialize();
        }

        public static void JoinRequest(object sender, DiscordRPC.Message.JoinRequestMessage e)
        {
            Logging.Log("Request to join from " + e.User.Username + "#" + e.User.Discriminator, "");
        }

        public static void Shutdown()
        {
            client.Dispose();
        }

        public static void SetPresence(string State, string Details, bool AcceptJoin)
        {
            //AcceptJoin &= Game.Multiplayer.LobbyKey != "";
            if (Details.Length > 128) Details = Details.Substring(0, 125) + "...";
            try
            {
                client.SetPresence(new RichPresence()
                {
                    Details = Details,
                    State = State,
                    Assets = new Assets
                    {
                        LargeImageKey = "logo",
                        LargeImageText = "It's a rhythm game"
                    },
                    Party = AcceptJoin ? new Party()
                    {
                        ID = "InterludeLobby",
                        Max = 16,
                        Size = 1,
                    } : null,
                    Secrets = new Secrets
                    {
                        JoinSecret = ""//AcceptJoin ? Game.Multiplayer.LobbyKey : ""
                    }
                });
            }
            catch (Exception e)
            {
                Logging.Log("Couldn't update rich prescence", e.ToString(), Logging.LogType.Warning);
            }
        }
    }
}
