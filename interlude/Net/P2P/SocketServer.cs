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
        private ClientWrapper[] clients = new ClientWrapper[16];
        public bool Running = false;
        public int ChartPicker = 0;
        private bool Playing = false;
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

                Running = true;
                return true;
            }
            catch (Exception e)
            {
                Utilities.Logging.Log("Failed to start server: "+ e.ToString(), Utilities.Logging.LogType.Error);
                sock.Disconnect(false);
                sock.Dispose();
                return false;
            }
        }

        public void Shutdown()
        {
            for (int i = 0; i < clients.Length; i++)
            {
                Kick("Host closed the lobby", i);
            }

            PacketPing.OnReceive -= HandlePing;
            PacketAuth.OnReceive -= HandleAuth;
            PacketMessage.OnReceive -= HandleMessage;
            PacketPlay.OnReceive -= HandlePlay;
            PacketScore.OnReceive -= HandleScore;
            accept.Completed -= OnAccept;

            sock.Close();
            sock.Dispose();

            Running = false;
        }

        public void Update()
        {
            for (int i = 0; i < clients.Length; i++)
            {
                if (clients[i] != null)
                {
                    clients[i].Update(i);
                    if (clients[i].Closed)
                    {
                        Utilities.Logging.Log("Dropped client with id " + i.ToString());
                        clients[i].Destroy();
                        if (clients[i].LoggedIn) Broadcast(clients[i].Username + " left the lobby.");
                        clients[i] = null;
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
            for (int i = 0; i < clients.Length; i++)
            {
                clients[i]?.SendPacket(packet);
            }
        }

        public void Broadcast(string message)
        {
            SendToAll(new PacketMessage() { text = message });
        }

        public void Message(string message, int id)
        {
            clients[id]?.SendPacket(new PacketMessage() { text = message });
        }

        public void Kick(string reason, int id)
        {
            Message("You have been kicked: "+reason, id);
            clients[id]?.Disconnect();
        }

        private void HandleMessage(PacketMessage packet, int id)
        {
            if (id >= 0 && clients[id]?.LoggedIn == true)
            {
                if (packet.text.StartsWith("*"))
                {
                    Broadcast("*" + clients[id].Username + " " + packet.text.Substring(1));
                }
                else
                {
                    Broadcast(clients[id].Username + ": " + packet.text);
                }
            }
        }

        private void HandleAuth(PacketAuth packet, int id)
        {
            if (clients[id] != null)
            {
                if (clients[id].LoggedIn)
                {
                    Utilities.Logging.Log("Client tried to log in twice!", Utilities.Logging.LogType.Warning);
                }
                else
                {
                    clients[id].Auth(packet);
                    Broadcast(clients[id].Username + " joined the lobby!");
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
                clients[id]?.Ping();
        }

        private void HandlePlay(PacketPlay packet, int id)
        {
            if (id >= 0)
            {
                if (id == ChartPicker)
                {
                    Playing = true;
                    Scores = new List<Score>();
                    for (int i = 0; i < clients.Length; i++)
                    {
                        if (clients[i] != null && clients[i].LoggedIn)
                        {
                            clients[i].ExpectingScore = true;
                            if (i != ChartPicker)
                            {
                                clients[i].SendPacket(packet);
                            }
                        }
                    }
                }
                else
                {
                    Broadcast(clients[id].Username + " is playing " + packet.name + " [" + packet.diff + "] from " + packet.pack + " (" + Utils.RoundNumber(packet.rate) + "x)");
                    if (Playing)
                    {
                        clients[id].ExpectingScore = false;
                    }
                }
            }
        }

        private void HandleScore(PacketScore packet, int id)
        {
            if (Playing)
            {
                if (packet.score != null && clients[id].ExpectingScore)
                {
                    Scores.Add(packet.score);
                    if (ScoreTimeout == null)
                    {
                        ScoreTimeout = DateTime.Now;
                    }
                }
                clients[id].ExpectingScore = false;
                bool ExpectingMoreScores = false;
                for (int i = 0; i < clients.Length; i++)
                {
                    if (clients[i]?.ExpectingScore == true)
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

        private void OnAccept(object o, SocketAsyncEventArgs e)
        {
            bool freeslot = false;
            for (int i = 0; i < clients.Length; i++)
            {
                if (clients[i] == null)
                {
                    freeslot = true;
                    clients[i] = new ClientWrapper(e.AcceptSocket);
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
