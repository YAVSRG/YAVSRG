using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Net.Protocol.Packets
{
    public class Packet<T>
    {
        public static event Action<T,int> OnReceive;

        //id is -1 if packet is from server -> client
        //id is 0 or higher corresponding to client id for client -> server
        //allows servers to function as a client too without interfering with itself, but irrelevant for pure clients
        public static void HandlePacket(T Packet, int id)
        {
            OnReceive(Packet, id);
        }
    }
}
