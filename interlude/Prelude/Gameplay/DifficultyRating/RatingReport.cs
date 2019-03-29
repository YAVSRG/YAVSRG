using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Prelude.Gameplay.Charts.YAVSRG;

namespace Prelude.Gameplay.DifficultyRating
{
    public class RatingReport
    {
        public float Physical, Technical;
        public double[] OverallPhysical, OverallTechnical;
        public float[,] Delta;
        public double[,] Jack, Trill, PhysicalComposite, Anchor;

        const double OHTNERF = 3;

        float[] fingers;

        //Calculates "difficulty" in terms of requirements in physical (conditioning) and technical (muscle memory/reading/general accuracy)
        //This is documented in more detail on the wiki (or at least will be)
        public RatingReport(ChartWithModifiers chart, float rate, KeyLayout.Layout playstyle)
        {
            KeyLayout layout = KeyLayout.GetLayout(playstyle, chart.Keys);
            int hands = layout.hands.Count;
            fingers = new float[chart.Keys];

            OverallPhysical = new double[chart.Notes.Points.Count];
            OverallTechnical = new double[chart.Notes.Points.Count];
            Delta = new float[chart.Notes.Points.Count, chart.Keys];
            Jack = new double[chart.Notes.Points.Count, chart.Keys];
            Trill = new double[chart.Notes.Points.Count, chart.Keys];
            PhysicalComposite = new double[chart.Notes.Points.Count, chart.Keys];
            Anchor = new double[chart.Notes.Points.Count, chart.Keys];
            List<GameplaySnap> snaps = chart.Notes.Points;
            Snap current;
            BinarySwitcher s;
            
            double[] currentStrain = new double[chart.Keys];
            float[] lastHandUse = new float[hands];
            List<double> handDiff = new List<double>();
            float delta;

            for (int i = 0; i < snaps.Count; i++)
            {
                if (snaps[i].taps.value + snaps[i].holds.value == 0) { continue; }
                //PHYSICAL ----
                for (int h = 0; h < hands; h++)
                {
                    current = snaps[i].Mask(layout.hands[h].Mask()); //bit mask to only look at notes corresponding to this hand
                    delta = (snaps[i].Offset - lastHandUse[h]) / rate;
                    s = new BinarySwitcher(current.taps.value | current.holds.value); //s = consider button presses

                    foreach (byte k in s.GetColumns()) //calculate value for each note and put it through overall algorithm
                    {
                        CalculateNoteDifficulty(k, i, current.Offset, layout.hands[h].Mask(), rate);
                        CalcUtils.UpdateStrain(ref currentStrain[k], PhysicalComposite[i, k] * 0.55, (snaps[i].Offset - fingers[k]) / rate);
                    }
                    //these are separate for loops because this assignment affects the other two
                    foreach (byte k in s.GetColumns()) //record times for calculation of the next snap
                    {
                        fingers[k] = current.Offset;
                    }
                    lastHandUse[h] = snaps[i].Offset;
                }
                OverallPhysical[i] = GetSnapDifficulty(currentStrain, (ushort)(snaps[i].taps.value | snaps[i].holds.value | snaps[i].ends.value)); //calculate difficulty for hands overall
                //TECHNICAL ----
                OverallTechnical[i] = GetStreamCurve((snaps[i].Offset - lastHandUse[0]) / rate);
                // ----
            }
            Physical = CalcUtils.GetOverallDifficulty(OverallPhysical);
            Technical = CalcUtils.GetOverallDifficulty(OverallTechnical); //final values are meaned because your accuracy is a mean average of hits
            //difficulty of each snap is assumed to be a measure of how unlikely it is you will hit it well
        }

        protected double GetSnapDifficulty(double[] strain, ushort mask)
        {
            List<double> values = new List<double>();
            foreach (byte k in new BinarySwitcher(mask).GetColumns())
            {
                values.Add(strain[k]);
            }
            return CalcUtils.RootMeanPower(values, 1);
        }

        protected void CalculateNoteDifficulty(byte column, int index, float offset, int columnsInHand, float rate)
        {
            BinarySwitcher s = new BinarySwitcher(columnsInHand);
            s.RemoveColumn(column);
            float delta1;
            if (fingers[column] > 0) //if this is not the first note in this column in the map
            {
                delta1 = Delta[index, column] = (offset - fingers[column]) / rate;
                Jack[index, column] = Math.Pow(GetJackCurve(delta1),OHTNERF); //add base jack value
            }
            else //if it is, this is some temp fix
            {
                delta1 = 10000;
            }
            foreach (byte k in s.GetColumns()) //for all other columns on this hand
            {
                if (fingers[k] > 0) //if this is not the first note in this column in the map
                {
                    float delta2 = (offset - fingers[k]) / rate;
                    Trill[index,column] += Math.Pow(GetStreamCurve(delta2) * GetJackCompensation(delta1, delta2),OHTNERF); //add trill part where applicable 
                }
            }
            PhysicalComposite[index, column] = Math.Pow(Trill[index, column] + Jack[index, column], 1 / OHTNERF);
        }

        protected float GetJackCompensation(float jackdelta, float streamdelta) //jumpjacks do not involve you swapping your fingers over to hit them, therefore ignore the stream part of a calc when there is a jack
        {
            float n = jackdelta / streamdelta;
            return (float)Math.Min(Math.Pow(Math.Max(Math.Log(n, 2), 0), 2), 1);
        }

        protected float GetStreamCurve(float delta) //how hard is it to hit these two adjacent notes? when they are VERY close together you can hit them at the same time so no difficulty added
        {
            float widthScale = 0.02f;
            float heightScale = 10f * 1.4f;
            float curveExponent = 1f;
            float cutoffExponent = 10f;
            return (float)Math.Max((heightScale / Math.Pow(widthScale * delta, curveExponent) - 0.1f * heightScale / Math.Pow(widthScale * delta, curveExponent * cutoffExponent)), 0);
        }

        protected float GetJackCurve(float delta) //how hard is it to hit these two notes in the same column? closer = exponentially harder
        {
            float widthScale = 0.02f;
            float heightScale = 10f * 2.5f;
            float curveExponent = 1f;
            return (float)Math.Min(heightScale / Math.Pow(widthScale * delta, curveExponent), 20);
        }
    }
}