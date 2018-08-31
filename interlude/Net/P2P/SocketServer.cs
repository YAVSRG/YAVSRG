using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Net;
using System.Net.Sockets;
using System.Threading.Tasks;
using YAVSRG.Net.P2P.Protocol.Packets;
using YAVSRG.Gameplay;

namespace YAVSRG.Net.P2P
{
    public class SocketServer
    {
        private Socket sock;
        private SocketAsyncEventArgs accept;
        public ClientWrapper[] Clients = new ClientWrapper[16];
        public bool Running = false;
        public int ChartPicker = 0;
        private bool Playing = false;
        private string PlayingHash;
        private DateTime? ScoreTimeout;
        private List<Score> Scores;

        public SocketServer()
        {

        }

        public bool Start()
        {
            try
            {
                Utilities.Logging.Log("Trying to host server..");
                sock = new Socket(SocketType.Stream, ProtocolType.Tcp);
                sock.Bind(new IPEndPoint(0, 32767));
                sock.Listen(3);
                accept = new SocketAsyncEventArgs();
                accept.Completed += OnAccept;
                sock.AcceptAsync(accept);
                Utilities.Logging.Log("Looks good. Server is awaiting connections.");

                PacketPing.OnReceive += HandlePing;
                PacketAuth.OnReceive += HandleAuth;
                PacketMessage.OnReceive += HandleMessage;
                PacketPlay.OnReceive += HandlePlay;
                PacketScore.OnReceive += HandleScore;
                PacketDisconnect.OnReceive += HandleDisconnect;

                Running = true;
                return true;
            }
            catch (Exception e)
            {
                Utilities.Logging.Log("Failed to start server: " + e.ToString(), Utilities.Logging.LogType.Error);
                sock.Disconnect(false);
                sock.Dispose();
                return false;
            }
        }

        public void Shutdown()
        {
            for (int i = 0; i < Clients.Length; i++)
            {
                Kick("Host closed the lobby", i);
            }

            PacketPing.OnReceive -= HandlePing;
            PacketAuth.OnReceive -= HandleAuth;
            PacketMessage.OnReceive -= HandleMessage;
            PacketPlay.OnReceive -= HandlePlay;
            PacketScore.OnReceive -= HandleScore;
            PacketDisconnect.OnReceive -= HandleDisconnect;
            accept.Completed -= OnAccept;

            sock.Close();
            sock.Dispose();

            Running = false;
        }

        public void Update()
        {
            for (int i = 0; i < Clients.Length; i++)
            {
                if (Clients[i] != null)
                {
                    Clients[i].Update(i);
                    if (Clients[i].Closed)
                    {
                        Utilities.Logging.Log("Dropped client with id " + i.ToString());
                        Clients[i].Destroy();
                        if (Clients[i].LoggedIn) Broadcast("{c:FFFF00}" + Clients[i].Username + " left the lobby.");
                        Clients[i] = null;
                        if (i == 0) //host left for whatever reason
                        {
                            Shutdown();
                        }
                    }
                }
            }
            if (Playing && ScoreTimeout != null && (DateTime.Now - ScoreTimeout).Value.TotalMilliseconds > 3000)
            {
                SendToAll(new PacketScoreboard() { scores = Scores });
                Playing = false;
                Scores.Clear();
                ScoreTimeout = null;
                Utilities.Logging.Log("Timed out while waiting for scores from players");
            }
        }

        public void SendToAll(object packet)
        {
            for (int i = 0; i < Clients.Length; i++)
            {
                Clients[i]?.SendPacket(packet);
            }
        }

        public void Broadcast(string message)
        {
            SendToAll(new PacketMessage() { text = message });
        }

        public void Message(string message, int id)
        {
            Clients[id]?.SendPacket(new PacketMessage() { text = message });
        }

        public void Kick(string reason, int id)
        {
            Message("You have been kicked: " + reason, id);
            Clients[id]?.SendPacket(new PacketDisconnect());
            Clients[id]?.Disconnect();
        }

        private void HandleMessage(PacketMessage packet, int id)
        {
            if (id >= 0 && Clients[id]?.LoggedIn == true)
            {
                if (packet.text.StartsWith("*"))
                {
                    Broadcast("*" + Clients[id].Username + " " + packet.text.Substring(1));
                }
                else
                {
                    Broadcast(Clients[id].Username + ": " + packet.text);
                }
            }
        }

        private void HandleAuth(PacketAuth packet, int id)
        {
            if (Clients[id] != null)
            {
                if (Clients[id].LoggedIn)
                {
                    Utilities.Logging.Log("Client tried to log in twice!", Utilities.Logging.LogType.Warning);
                }
                else
                {
                    Clients[id].Auth(packet);
                    Broadcast("{c:FFFF00}" + Clients[id].Username + " joined the lobby!");
                }
            }
            else
            {
                Utilities.Logging.Log("Received auth packet for empty slot!", Utilities.Logging.LogType.Warning);
            }
        }

        private void HandlePing(PacketPing packet, int id)
        {
            if (id >= 0)
                Clients[id]?.Ping();
        }

        private void HandlePlay(PacketPlay packet, int id)
        {
            if (id >= 0 && Clients[id].LoggedIn)
            {
                if (id == ChartPicker)
                {
                    Broadcast("{c:DDDDEE}" + Clients[id].Username + " (Chart picker) is playing " + packet.name + " [" + packet.diff + "] from " + packet.pack + " (" + Utils.RoundNumber(packet.rate) + "x)");
                    Playing = true;
                    PlayingHash = packet.hash;
                    Scores = new List<Score>();
                    for (int i = 0; i < Clients.Length; i++)
                    {
                        if (Clients[i] != null && Clients[i].LoggedIn)
                        {
                            Clients[i].ExpectingScore = true;
                            if (i != ChartPicker)
                            {
                                Clients[i].SendPacket(packet);
                            }
                        }
                    }
                }
                else
                {
                    Broadcast("{c:BBBBCC}" + Clients[id].Username + " is playing " + packet.name + " [" + packet.diff + "] from " + packet.pack + " (" + Utils.RoundNumber(packet.rate) + "x)");
                    if (Playing && packet.hash != PlayingHash)
                    {
                        Clients[id].ExpectingScore = false;
                    }
                }
            }
        }

        private void HandleScore(PacketScore packet, int id)
        {
            if (Playing)
            {
                if (packet.score != null && Clients[id].ExpectingScore)
                {
                    Scores.Add(packet.score);
                    if (ScoreTimeout == null)
                    {
                        ScoreTimeout = DateTime.Now;
                    }
                }
                Clients[id].ExpectingScore = false;
                bool ExpectingMoreScores = false;
                for (int i = 0; i < Clients.Length; i++)
                {
                    if (Clients[i]?.ExpectingScore == true)
                    {
                        ExpectingMoreScores = true;
                        break;
                    }
                }
                if (!ExpectingMoreScores)
                {
                    SendToAll(new PacketScoreboard() { scores = Scores });
                    Playing = false;
                    Scores.Clear();
                    ScoreTimeout = null;
                    Utilities.Logging.Log("Got score from all participating players :)");
                }
            }
        }

        private void HandleDisconnect(PacketDisconnect packet, int id)
        {
            if (id >= 0)
            {
                Clients[id]?.Disconnect();
            }
        }

        private void OnAccept(object o, SocketAsyncEventArgs e)
        {
            bool freeslot = false;
            for (int i = 0; i < Clients.Length; i++)
            {
                if (Clients[i] == null)
                {
                    freeslot = true;
                    Clients[i] = new ClientWrapper(e.AcceptSocket);
                    Utilities.Logging.Log("Accepted new connection, client id is " + i.ToString());
                    Message("Welcome to " + Game.Options.Profile.Name + "'s lobby!", i);
                    break;
                }
            }
            if (!freeslot)
            {
                Utilities.Logging.Log("There was a new connection, but the server slots are full!", Utilities.Logging.LogType.Warning);
                e.AcceptSocket.Close();
            }

            accept = new SocketAsyncEventArgs();
            accept.Completed += OnAccept;
            sock.AcceptAsync(accept);
        }
    }
}
