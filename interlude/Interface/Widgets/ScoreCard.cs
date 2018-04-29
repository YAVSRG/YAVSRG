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
        Sprite frame;
        float acc;
        string accdisplay;
        string combo;
        string rating;
        string mods;

        public ScoreCard(Score c)
        {
            frame = Content.LoadTextureFromAssets("frame");
            PositionBottomRight(430, 100, AnchorType.MIN, AnchorType.MIN);
            ScoreSystem score = ScoreSystem.GetScoreSystem(ScoreType.Default);
            score.ProcessScore(ScoreTracker.StringToHitData(c.hitdata, c.keycount));
            acc = score.Accuracy();
            accdisplay = Utils.RoundNumber(acc) + "%";
            combo = score.BestCombo.ToString() + "x";
            rating = "0.00";
            mods = string.Join(",", c.mods);
            this.c = c;
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.DrawFrame(frame, left, top, right, bottom, 30f, Game.Screens.HighlightColor);
            SpriteBatch.Font1.DrawText(c.player, 35f, left + 5, top, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawTextToFill(mods, left, bottom - 40, right-180, bottom, Game.Options.Theme.MenuFont);

            SpriteBatch.Font1.DrawJustifiedText(accdisplay, 35f, right - 5, top, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawJustifiedText(rating, 20f, right - 5, bottom - 60, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawJustifiedText(c.date + " " + c.time, 20f, right - 5, bottom - 35, Game.Options.Theme.MenuFont);
        }

        public static Comparison<Widget> Compare = (a, b) => { return ((ScoreCard)b).acc.CompareTo(((ScoreCard)a).acc); };
    }
}
