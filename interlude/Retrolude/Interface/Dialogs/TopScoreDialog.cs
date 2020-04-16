using Interlude.Interface.Widgets;

namespace Interlude.Interface.Dialogs
{
    public class TopScoreDialog : FadeDialog
    {
        bool Technical;
        TopScoreDisplay Display;

        public TopScoreDialog(bool tech) : base((s) => { })
        {
            Technical = tech;
            AddChild(Display = new TopScoreDisplay(Technical));
            AddChild(new TextPicker("Keymode", new[] { "3K", "4K", "5K", "6K", "7K", "8K", "9K", "10K" }, (int)Game.Options.Profile.DefaultKeymode, (i) => { Display.Refresh(i); }).Reposition(-210, 1, -60, 1, -10, 1, -10, 1));
            Reposition(100, 0, 100, 0, -100, 1, -100, 1);
            Display.Refresh((int)Game.Options.Profile.DefaultKeymode);
        }
    }
}
