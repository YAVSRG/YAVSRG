using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Gameplay.Mods
{
    public class AutoPlay : Mod
    {
        public override void ApplyToHitData(ChartWithModifiers c, ref HitData[] hitdata, string data)
        {
            for (int i = 0; i < hitdata.Length; i++)
            {
                for (byte k = 0; k < hitdata[i].hit.Length; k++)
                {
                    if (hitdata[i].hit[k] == 1)
                    {
                        hitdata[i].hit[k] = 2;
                    }
                }
            }
        }

        public override int GetStatus(string data) { return 2; }

        public override string GetName(string data)
        {
            return "Auto";
        }

        public override string GetDescription(string data) { return "Automatically plays the chart for you! (With perfect accuracy)"; }
    }
}
