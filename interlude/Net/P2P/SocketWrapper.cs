using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Net;
using System.Net.Sockets;

namespace YAVSRG.Net.P2P
{
    public class SocketWrapper
    {
        protected Socket sock;
        private string buffer = "";
        public bool Closed = false;
        public bool Destroyed = false;

        public SocketWrapper(Socket sock)
        {
            this.sock = sock;
        }

        public void SendPacket(object o)
        {
            if (Closed) return;
            try
            {
                sock.Send(Encoding.UTF8.GetBytes(Protocol.Protocol.WritePacket(o)));
            }
            catch (Exception e)
            {
                Error(e);
            }
        }

        protected void Error(Exception e)
        {
            Utilities.Logging.Log("An error has occured in a socket", e.ToString(), Utilities.Logging.LogType.Error);
            Disconnect();
        }

        public virtual void Disconnect()
        {
            if (Closed) return;
            Closed = true;
        }

        public virtual void Destroy() //happens after disconnection, but is different method so that remaining data can be read first
        {
            sock.Close();
            sock.Dispose();
            Destroyed = true;
        }

        public virtual void Update(int id)
        {
            if (Destroyed) return;
            if (sock.Available > 0)
            {
                try
                {
                    byte[] bytes = new byte[512];
                    int length = sock.Receive(bytes);
                    buffer += Encoding.UTF8.GetString(bytes, 0, length);
                    while (buffer.Contains('\n'))
                    {
                        string[] split = buffer.Split(new[] { '\n' }, 2);
                        Protocol.Protocol.HandlePacket(split[0], id);
                        buffer = split[1];
                    }
                }
                catch (Exception e)
                {
                    Error(e);
                }
            }
        }
    }
}
