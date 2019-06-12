using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    class CreditsPanel : OptionsPanel
    {
        string[] lines;
        public CreditsPanel(InfoBox ib) : base(ib, "Credits")
        {
            lines = ResourceGetter.GetCredits().Split('\n');
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            for (int i = 0; i < lines.Length; i++)
            {
                SpriteBatch.Font1.DrawCentredText(lines[i], 20f, bounds.CenterX, bounds.Top + 150 + 30 * i, System.Drawing.Color.White, true, System.Drawing.Color.Black);
            }
        }
    }
}
