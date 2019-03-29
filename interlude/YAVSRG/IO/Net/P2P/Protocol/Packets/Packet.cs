using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Net.P2P.Protocol.Packets
{
    public class Packet<T>
    {
        public static event Action<T,int> OnReceive;

        public static void HandlePacket(T Packet, int id)
        {
            OnReceive(Packet, id);
        }
    }
}
