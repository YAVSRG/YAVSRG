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
            PositionBottomRight(20, 100, AnchorType.MAX, AnchorType.MIN);
            ScoreSystem score = ScoreSystem.GetScoreSystem(ScoreType.Default);
            var hd = ScoreTracker.StringToHitData(c.hitdata, c.keycount);
            score.ProcessScore(hd);
            acc = score.Accuracy();
            accdisplay = Utils.RoundNumber(acc) + "%";
            rating = Charts.DifficultyRating.PlayerRating.GetRating(new Charts.DifficultyRating.RatingReport(Game.Gameplay.GetModifiedChart(c.mods), c.rate, c.playstyle), hd);
            mods = Game.Gameplay.GetModString(c.mods, c.rate, c.playstyle);
            this.c = c;
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            ScreenUtils.DrawFrame(bounds, 30f, Game.Screens.HighlightColor);
            SpriteBatch.Font1.DrawText(c.player, 35f, bounds.Left + 5, bounds.Top, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawTextToFill(mods, new Rect(bounds.Left, bounds.Bottom - 40,
                bounds.Right - SpriteBatch.Font2.DrawJustifiedText(c.time.ToString(), 20f, bounds.Right - 5, bounds.Bottom - 35, Game.Options.Theme.MenuFont) - 10,
                bounds.Bottom), Game.Options.Theme.MenuFont);

            SpriteBatch.Font1.DrawJustifiedText(accdisplay, 35f, bounds.Right - 5, bounds.Top, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawJustifiedText(Utils.RoundNumber(rating), 20f, bounds.Right - 5, bounds.Bottom - 60, Game.Options.Theme.MenuFont);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            if (ScreenUtils.CheckButtonClick(bounds))
            {
                //Game.Screens.AddDialog(new Dialogs.ScoreInfoDialog(c, (s) => { }));
            }
        }

        public static Comparison<Widget> Compare = (a, b) => { return ((ScoreCard)b).rating.CompareTo(((ScoreCard)a).rating); };
    }
}
