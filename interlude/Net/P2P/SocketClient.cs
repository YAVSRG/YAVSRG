using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Net.Sockets;
using System.Net;

namespace YAVSRG.Net.P2P
{
    public class SocketClient : SocketWrapper
    {
        public SocketClient(long ip) : base(null)
        {
            try
            {
                sock = new Socket(SocketType.Stream, ProtocolType.Tcp);
                sock.Connect(new IPAddress(ip), 32767);
            }
            catch (Exception e)
            {
                Error(e);
            }
        }

        public void Update()
        {
            base.Update(-1);
        }
    }
}
