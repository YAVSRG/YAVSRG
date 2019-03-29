using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Interlude.Gameplay;

namespace Interlude.Net.P2P.Protocol.Packets
{
    public class PacketScoreboard : Packet<PacketScoreboard>
    {
        public List<Score> scores;
    }
}
