using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Beatmap;

namespace YAVSRG.Gameplay
{
    public class PlayingChart
    {
        public class HitData : OffsetItem
        {
            public float[] delta;
            public byte[] hit;

            public HitData(Snap s, int keycount)
            {
                Offset = s.Offset;
                hit = new byte[keycount];
                foreach (int k in s.Combine().GetColumns())
                {
                    hit[k] = 1;
                }
                delta = new float[keycount];
            }
        }

        public Chart c;
        public CCScoring scorer;
        public HitData[] hitdata;

        public PlayingChart(Chart c)
        {
            this.c = c;
            scorer = new CCScoring(Game.Options.Profile.HitWindows(), new int[] { 10, 9, 5, 1, -10, 0 }, 10);
            int to = c.States.Count;
            hitdata = new HitData[to];
            for (int i = 0; i < to; i++)
            {
                hitdata[i] = new HitData(c.States.Points[i], c.Keys);
            }
        }

        public int Combo()
        {
            return scorer.Combo;
        }

        public float Accuracy()
        {
            return scorer.Accuracy();
        }

        public void Update(float time)
        {
            scorer.Update(time,hitdata);
        }
    }
}
