using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Beatmap.DifficultyRating
{
    public class RatingReport
    {
        static readonly float TIMEEXPONENT = -1.7f; //difficulty inversely proportional to time between each note
        static readonly float SMOOTHEXPONENT = 2f;
        static readonly float TIMESCALE = 1000;
        static readonly float BASESCALE = 50;

        public List<float> raw;
        public float[] breakdown;

        public RatingReport(Chart map, float rate, float hitwindow)
        {
            KeyLayout layout = new KeyLayout(map.Keys);
            int hands = layout.hands.Count;
            float[] fingers = new float[map.Keys];

            raw = new List<float>();
            Snap[] snaps = map.States.Points;
            Snap current;
            float delta;
            float temp;
            float temp2;
            Snap.BinarySwitcher s;

            for (int i = 0; i < snaps.Length; i++)
            {
                temp2 = 0;
                for (int h = 0; h < hands; h++)
                {
                    temp = 0;
                    current = snaps[i].Mask(layout.hands[h].Mask());
                    s = new Snap.BinarySwitcher(current.taps.value + current.holds.value);
                    foreach (int k in s.GetColumns())
                    {
                        if (fingers[k] > 0)
                        {
                            delta = (current.Offset - fingers[k]) / rate;
                            temp += GetSpeedMult(delta)/s.Count;
                        }
                        fingers[k] = current.Offset;
                    }
                    temp2 += BASESCALE*temp/hands;
                }
                raw.Add(temp2);
            }
            breakdown = new float[] { DataSet.Mean(raw), 0, 0, 0, 0 };
        }

        public float GetSpeedMult(float delta)
        {
            return (float)Math.Pow(delta, TIMEEXPONENT) * TIMESCALE;
        }
    }
}
