using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;

namespace YAVSRG.Interface.Widgets
{
    public class ScoreCard : Widget
    {
        Score c;
        float acc;
        string accdisplay;
        float rating;
        string mods;

        public ScoreCard(Score c)
        {
            PositionBottomRight(430, 100, AnchorType.MIN, AnchorType.MIN);
            ScoreSystem score = ScoreSystem.GetScoreSystem(ScoreType.Default);
            var hd = ScoreTracker.StringToHitData(c.hitdata, c.keycount);
            score.ProcessScore(hd);
            acc = score.Accuracy();
            accdisplay = Utils.RoundNumber(acc) + "%";
            rating = Charts.DifficultyRating.PlayerRating.GetRating(new Charts.DifficultyRating.RatingReport(Game.Gameplay.GetModifiedChart(c.mods), c.rate, c.playstyle), hd);
            mods = Game.Gameplay.GetModString(c.mods, c.rate, c.playstyle);
            this.c = c;
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.DrawFrame(left, top, right, bottom, 30f, Game.Screens.HighlightColor);
            SpriteBatch.Font1.DrawText(c.player, 35f, left + 5, top, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawTextToFill(mods, left, bottom - 40,
                right - SpriteBatch.Font2.DrawJustifiedText(c.time.ToString(), 20f, right - 5, bottom - 35, Game.Options.Theme.MenuFont) - 10,
                bottom, Game.Options.Theme.MenuFont);

            SpriteBatch.Font1.DrawJustifiedText(accdisplay, 35f, right - 5, top, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawJustifiedText(Utils.RoundNumber(rating), 20f, right - 5, bottom - 60, Game.Options.Theme.MenuFont);
        }

        public static Comparison<Widget> Compare = (a, b) => { return ((ScoreCard)b).rating.CompareTo(((ScoreCard)a).rating); };
    }
}
