using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;
using System.Drawing;

namespace YAVSRG.Interface.Widgets
{
    public class ScoreCard : Widget
    {
        Score c;
        float acc;
        string accdisplay;
        float rating;
        string mods;

        public ScoreCard(Score score)
        {
            PositionBottomRight(20, 100, AnchorType.MAX, AnchorType.MIN);
            this.c = score;
            try
            {
                ScoreSystem scoring = ScoreSystem.GetScoreSystem(ScoreType.Default);
                var hd = ScoreTracker.StringToHitData(score.hitdata, score.keycount);
                scoring.ProcessScore(hd);
                acc = scoring.Accuracy();
                accdisplay = Utils.RoundNumber(acc) + "%";
                var chart = Game.Gameplay.GetModifiedChart(score.mods);
                rating = Charts.DifficultyRating.PlayerRating.GetRating(new Charts.DifficultyRating.RatingReport(chart, score.rate, score.playstyle), hd);
                mods = Game.Gameplay.GetModString(chart, score.rate, score.playstyle);
            }
            catch
            {
                accdisplay = "???%";
                mods = "????";
            }
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            ScreenUtils.DrawFrame(bounds, 30f, Game.Screens.HighlightColor);
            SpriteBatch.Font1.DrawText(c.player, 35f, bounds.Left + 5, bounds.Top, Game.Options.Theme.MenuFont, true, Color.Black);
            SpriteBatch.Font2.DrawTextToFill(mods, new Rect(bounds.Left, bounds.Bottom - 40,
                bounds.Right - SpriteBatch.Font2.DrawJustifiedText(c.time.ToString(), 20f, bounds.Right - 5, bounds.Bottom - 35, Game.Options.Theme.MenuFont, true, Color.Black) - 10,
                bounds.Bottom), Game.Options.Theme.MenuFont, true, Color.Black);

            SpriteBatch.Font1.DrawJustifiedText(accdisplay, 35f, bounds.Right - 5, bounds.Top, Game.Options.Theme.MenuFont, true, Color.Black);
            SpriteBatch.Font2.DrawJustifiedText(Utils.RoundNumber(rating), 20f, bounds.Right - 5, bounds.Bottom - 60, Game.Options.Theme.MenuFont, true, Color.Black);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            if (ScreenUtils.MouseOver(bounds))
            {
                if (accdisplay != "???%" && Input.MouseClick(OpenTK.Input.MouseButton.Left))
                {
                    Game.Screens.AddDialog(new Dialogs.ScoreInfoDialog(c, (s) => { }));
                }
                else if (Input.MouseClick(OpenTK.Input.MouseButton.Right) && Input.KeyPress(OpenTK.Input.Key.Delete))
                {
                    SetState(0);
                    Game.Gameplay.ChartSaveData.Scores.Remove(c);
                }
            }
        }

        public static Comparison<Widget> Compare = (a, b) => { return ((ScoreCard)b).rating.CompareTo(((ScoreCard)a).rating); };
    }
}
