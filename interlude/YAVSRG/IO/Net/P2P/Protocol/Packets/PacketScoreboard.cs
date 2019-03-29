using System.Collections.Generic;
using Prelude.Gameplay;

namespace Interlude.Net.P2P.Protocol.Packets
{
    public class PacketScoreboard : Packet<PacketScoreboard>
    {
        public List<Score> scores;
    }
}
