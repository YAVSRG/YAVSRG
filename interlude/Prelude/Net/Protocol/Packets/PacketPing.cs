using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Net.Protocol.Packets
{
    public class PacketPing : Packet<PacketPing>
    {
        public int id;
    }
}
