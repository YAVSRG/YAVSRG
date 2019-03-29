using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Gameplay.Mods
{
    public class NoLN : Mod
    {
        public override void Apply(ChartWithModifiers c, string data)
        {
            base.Apply(c, data);
            if (data == "all")
            {
                foreach (GameplaySnap s in c.Notes.Points)
                {
                    s.ends.value = 0;
                    s.middles.value = 0;
                    s.taps.value += s.holds.value;
                    s.holds.value = 0;
                }
            }
        }

        public override void ApplyToHitData(ChartWithModifiers c, ref HitData[] hitdata, string data)
        {
            if (data == "release")
            {
                for (int i = 0; i < hitdata.Length; i++)
                {
                    for (byte k = 0; k < hitdata[i].hit.Length; k++)
                    {
                        if (c.Notes.Points[i].ends.GetColumn(k))
                        {
                            hitdata[i].hit[k] = 0;
                        }
                    }
                }
            }
        }

        public override bool IsApplicable(ChartWithModifiers c, string data)
        {
            foreach (GameplaySnap s in c.Notes.Points)
            {
                if (s.holds.value > 0)
                {
                    return true;
                }
            }
            return true; //debug
        }

        public override string[] Settings { get { return new string[] { "release", "all" }; } }

        public override string GetName(string data)
        {
            return data == "release" ? "NoReleases" : "NoHolds";
        }

        public override string GetDescription(string data) { return  data == "release" ? "Disables the need to release the ends of hold notes with exact timing, as these are not given judgements.\nYou are still required to be holding them while hitting another other simultaneous notes." : "Removes all hold notes from a chart and replaces them with a tap note where they began."; }
    }
}
