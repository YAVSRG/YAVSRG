using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Beatmap.DifficultyRating
{
    public class RatingReportOld
    {
        static readonly float TIMEEXPONENT = -1.8f; //difficulty inversely proportional to time between each note
        static readonly float SMOOTHEXPONENT = 0.75f;
        static readonly float SKILLSETEXPONENT = 0.5f; //this is the weakness of the calc
        static readonly float HANDEXPONENT = 0.8f; //as long as ratings are about 10 per movement this is a good number pick (maths involved)
        static readonly float JACKMULTIPLIER = 1.25f; //fixed value depending on timeexponent to make jt = roll (maths involved)
        static readonly float SCALE = 1000000;

        public List<float>[] combine;
        public List<float>[] combineskillset;
        public List<float>[,] raw;
        public List<float> final;
        public float[] breakdown;

        public RatingReportOld(Chart map, float rate, float hitwindow)
        {
            KeyLayout layout = new KeyLayout(map.Keys);
            int hands = layout.hands.Count;
            Snap[] previousHands = new Snap[hands];

            raw = new List<float>[hands, 6];
            combine = new List<float>[hands];
            combineskillset = new[] { new List<float>(), new List<float>(), new List<float>(), new List<float>(), new List<float>(), new List<float>() };
            for (int i = 0; i < 6; i++)
            {
                for (int h = 0; h < hands; h++)
                {
                    raw[h, i] = new List<float>();
                }
            }
            combine.Initialize();

            Snap[] snaps = map.Notes.Points.ToArray();
            Snap current;
            Snap previous;
            float delta, manip, mult;

            for (int i = 0; i < snaps.Length; i++)
            {
                for (int h = 0; h < hands; h++)
                {
                    current = snaps[i].Mask(layout.hands[h].Mask());
                    previous = previousHands[h];

                    if (previous == null)
                    {
                        previousHands[h] = current;
                        continue;
                    }
                    delta = (current.Offset - previousHands[h].Offset) / rate;
                    mult = GetSpeedMult(delta);

                    //MANIPULATION -- reduce the rating of stuff that is close together
                    manip = (float)Math.Pow(delta / hitwindow * 0.5,0.5f);
                    mult *= manip;

                    //JACKHAMMERS
                    var jack = new BinarySwitcher(previous.taps.value & (current.holds.value | current.taps.value));
                    if (jack.value > 0)
                    {
                        raw[h, 0].Add(GetSpeedMult(delta * JACKMULTIPLIER));
                    }
                    else
                    {
                        raw[h, 0].Add(0);
                    }

                    //STRAIN
                    raw[h, 1].Add(GetFingerStrain(
                    new BinarySwitcher(previous.holds.value | previous.taps.value | previous.middles.value),
                    new BinarySwitcher(current.holds.value | current.taps.value | current.middles.value),
                    layout.hands[h]) * mult);

                    //LONG NOTE
                    if (current.middles.value > 0)
                    {
                        //LIFTS
                        raw[h, 2].Add(1.0f * GetFingerStrain(
                        new BinarySwitcher(current.middles.value),
                        new BinarySwitcher(current.ends.value),
                        layout.hands[h]) * mult
                        + 1.0f * GetFingerStrain(
                        new BinarySwitcher(current.middles.value),
                        new BinarySwitcher(current.holds.value | current.taps.value),
                        layout.hands[h]) * mult);
                    }
                    else
                    {
                        raw[h, 2].Add(0);
                    }
                    previousHands[h] = current;
                }
            }


            for (int h = 0; h < hands; h++)
            {
                combine[h] = DataSet.Smooth(DataSet.Combine(raw[h, 0], raw[h, 1], raw[h, 2], SKILLSETEXPONENT),SMOOTHEXPONENT);
            }
            for (int i = 0; i < 6; i++)
            {
                combineskillset[i] = DataSet.Combine(raw, i, HANDEXPONENT);
            }
            final = DataSet.Combine(combine,HANDEXPONENT);
            breakdown = new[] {
                DataSet.Mean(final),
                0,0,0
            };
        }

        public float GetSpeedMult(float delta)
        {
            return (float)Math.Pow(delta,TIMEEXPONENT)*SCALE;
        }

        public float GetFingerStrain(BinarySwitcher a, BinarySwitcher b, KeyLayout.Hand hand)
        {
            float r = 0f;
            b.value &= ~(a.value);
            foreach (int k in a.GetColumns())
            {
                foreach (int j in b.GetColumns())
                {
                    r += 1/HandDistance(k, j, hand);
                }
            }
            return r;
        }

        public float HandDistance(int a, int b, KeyLayout.Hand hand)
        {
            return Math.Abs(hand.GetFingerPosition(a) - hand.GetFingerPosition(b));
        }
    }
}
