using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Beatmap.DifficultyRating
{
    public class RatingReport
    {
        static readonly float TIMEEXPONENT = -1.8f; //difficulty inversely proportional to time between each note
        static readonly float SMOOTHEXPONENT = 0.6f;
        static readonly float SKILLSETEXPONENT = 0.8f;
        static readonly float HANDEXPONENT = 0.3f;
        static readonly float JACKMULTIPLIER = 1.5f;
        static readonly float SCALE = 1000000;

        public List<float>[] combine;
        public List<float>[] combineskillset;
        public List<float>[,] raw;
        public List<float> final;
        public float[] breakdown;

        public RatingReport(Chart map, float rate, float hitwindow)
        {
            KeyLayout layout = new KeyLayout(map.Keys);
            int hands = layout.hands.Count;
            Snap[] previousHands = new Snap[hands];

            raw = new List<float>[hands, 6];//jack, stream, longnote, rhythm, sv, ??
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

            Snap[] snaps = map.States.Points;
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

                    //MANIPULATION -- This will break some stuff unless i fix

                    manip = delta / hitwindow;
                    if (manip < 2) //this is a grace note
                    {
                        mult /= 10f;
                    }

                    //JACKHAMMERS
                    var jack = new Snap.BinarySwitcher(previous.taps.value & (current.holds.value | current.taps.value));
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
                    new Snap.BinarySwitcher(previous.holds.value | previous.taps.value | previous.middles.value),
                    new Snap.BinarySwitcher(current.holds.value | current.taps.value | current.middles.value),
                    layout.hands[h]) * mult);

                    //LONG NOTE
                    if (current.middles.value > 0)
                    {
                        //LIFTS
                        raw[h, 2].Add(1.0f * GetFingerStrain(
                        new Snap.BinarySwitcher(current.middles.value),
                        new Snap.BinarySwitcher(current.ends.value),
                        layout.hands[h]) * mult
                        + 1.0f * GetFingerStrain(
                        new Snap.BinarySwitcher(current.middles.value),
                        new Snap.BinarySwitcher(current.holds.value | current.taps.value),
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
                DataSet.Mean(combineskillset[0]),
                DataSet.Mean(combineskillset[1]),
                DataSet.Mean(combineskillset[2])
            };
        }

        public float GetSpeedMult(float delta)
        {
            return (float)Math.Pow(delta,TIMEEXPONENT)*SCALE;
        }

        public float GetFingerStrain(Snap.BinarySwitcher a, Snap.BinarySwitcher b, KeyLayout.Hand hand)
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
