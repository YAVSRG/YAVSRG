using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;

namespace YAVSRG.Charts.DifficultyRating
{
    public class PlayerRating
    {
        
        //uncommented due to being subject to change
        //how it works may be explained in design document
        public static float GetRating(RatingReport r, ScoreTracker.HitData[] hitdata) //calculates rating of a play - i.e how capable you showed you were when you played this chart
        {
            double v = 0;
            int samplesize = 2;
            List<double> sample = new List<double>();
            float rms, sd;
            for (int i = 0; i < r.PhysicalData.Length; i++)
            {
                if (r.PhysicalData[i] > 0)
                {
                    sample.Clear();
                    for (int s = 0; s < samplesize; s++)
                    {
                        if (i > s)
                        {
                            for (byte k = 0; k < hitdata[i - s].hit.Length; k++)
                            {
                                if (hitdata[i - s].hit[k] > 0)
                                {
                                    sample.Add(hitdata[i - s].delta[k]);
                                }
                            }
                        }
                    }
                    if (sample.Count > 0)
                    {
                        rms = (float)Math.Max(2,Utils.RootMeanPower(sample, 2));
                        sd = 9f;
                        float w = Func((20 - rms) / sd);
                        if (v <= 0.01 || double.IsNaN(v))
                        {
                            v = w * r.PhysicalData[i];
                        }
                        v *= Math.Exp(0.02 * w * Math.Max(0,Math.Log(w * r.PhysicalData[i] / v)));
                    }
                }
            }
            return (float)v;
        }

        public static float Func(double value) //en.wikipedia.org/wiki/Normal_distribution#Cumulative_distribution_function
        {
            value /= 1.414213562f;
            return (float)Math.Max(0,Math.Min((0.5f + Math.Pow(Math.PI, -0.5) * (value - Math.Pow(value, 3) / 3 + Math.Pow(value, 5) / 10 - Math.Pow(value, 7) / 42 + Math.Pow(value, 9) / 216)),1));
        }
    }
}
