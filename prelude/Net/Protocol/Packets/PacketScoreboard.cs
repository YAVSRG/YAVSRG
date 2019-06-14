using System.Collections.Generic;
using Prelude.Gameplay;

namespace Prelude.Net.Protocol.Packets
{
    public class PacketScoreboard : Packet<PacketScoreboard>
    {
        public List<Score> scores;
    }
}
