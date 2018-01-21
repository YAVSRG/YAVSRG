using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Beatmap.DifficultyRating;
using System.Drawing;
using static YAVSRG.Interface.ScreenUtils;
using YAVSRG.Beatmap;

namespace YAVSRG.Interface.Widgets
{
    public class ChartDifficulty : InfoCard
    {
        RatingReport diff;
        float[] rating;
        int points;

        public ChartDifficulty(Chart c) : base()
        {
            ChangeChart(c);
        }

        public void ChangeChart(Chart c)
        {
            diff = new RatingReport(c, Game.Options.Profile.Rate, 45f);
            rating = diff.breakdown;
            points = diff.final.Count;
            info = new string[]
            {
                Utils.RoundNumber(rating[0]),Utils.RoundNumber(0),
                "this",Utils.RoundNumber(rating[1]),
                "has",Utils.RoundNumber(rating[2]),
                "all",Utils.RoundNumber(rating[3]),
                "gotta",Utils.RoundNumber(0),
                "go",Utils.RoundNumber(0),
                Utils.FormatTime(c.GetDuration()/Game.Options.Profile.Rate),((int)(c.GetBPM()*Game.Options.Profile.Rate)).ToString()+"BPM",
            };
        }

        public override void Draw(float left, float top, float right, float bottom)
        {

            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            if (diff != null)
            {
                //DrawGraph(left, top, right, bottom);
            }
        }

        private void DrawGraph(float left, float top, float right, float bottom)
        {
            float w = 480f / points;
            for (int i = 0; i < points; i++)
            {
                float x = left + 10 + i * w;
                SpriteBatch.DrawRect(x - 2, top + 702 - diff.final[i]*4, x + 2, top + 698 - diff.final[i] * 4, Color.White);
                SpriteBatch.DrawRect(x - 2, top + 702 - diff.combineskillset[0][i] * 4, x + 2, top + 698 - diff.combineskillset[0][i] * 4, Color.Red);
                SpriteBatch.DrawRect(x - 2, top + 702 - diff.combineskillset[1][i] * 4, x + 2, top + 698 - diff.combineskillset[1][i] * 4, Color.Blue);
                SpriteBatch.DrawRect(x - 2, top + 702 - diff.combineskillset[2][i] * 4, x + 2, top + 698 - diff.combineskillset[2][i] * 4, Color.Green);
            }
        }
    }
}
