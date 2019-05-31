using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using Interlude.Interface.Widgets;
using Interlude.Gameplay;
using System.Drawing;

namespace Interlude.Interface.Dialogs
{
    public class ProfileDialog : FadeDialog
    {
        bool recalcScores;

        public ProfileDialog(Action<string> action) : base(action)
        {
            var f = new FrameContainer() { VerticalFade = 0, HorizontalFade = 100f };
            AddChild(f);
            f.AddChild(new SimpleButton("New Profile...", NewProfile, () => false, 30f)
                .Reposition(220, 0, 10, 0, -10, 1, 60, 0));
            f.AddChild(new SimpleButton("Rename Profile...", RenameProfile, () => false, 30f)
                .Reposition(220, 0, 70, 0, -10, 1, 120, 0));
            f.AddChild(new SimpleButton("Refresh Scores...", () =>
            {
                if (!recalcScores)
                    Game.Tasks.AddTask(Game.Options.Profile.Stats.RecalculateTop(), (b) => { recalcScores = false; }, "RecalculateTop", false);
                recalcScores = true;
            }, () => recalcScores, 30f)
                .Reposition(220, 0, 130, 0, -10, 1, 180, 0));
            //todo: refresh these when profile changed
            f.AddChild(new TextPicker("", new[] { "3K", "4K", "5K", "6K", "7K", "8K", "9K", "10K" }, (int)Game.Options.Profile.DefaultKeymode, (i) => { Game.Options.Profile.PreferredKeymode = (Options.Profile.Keymode)i; })
                .Reposition(220, 0, -60, 1, -10, 1, -10, 1));
            f.AddChild(new TickBox("I only play", Game.Options.Profile.KeymodePreference, (b) => { Game.Options.Profile.KeymodePreference = b; ChartLoader.Refresh(); })
                .Reposition(220, 0, -110, 1, -10, 1, -60, 1));

            var profileSelector = new FlowContainer();
            for (int i = 0; i < Options.Options.Profiles.Count; i++)
            {
                profileSelector.AddChild(ProfileButton(Options.Options.Profiles[i]));
            }
            AddChild(profileSelector.Reposition(10, 0, 10, 0, 210, 0, -10, 1));

            Reposition(0, 0, 0, 0.5f, 500, 0, 0, 1);
        }

        Widget ProfileButton(Options.Profile p)
        {
            return new SimpleButton(p.Name, () => { Game.Options.ChangeProfile(p); }, () => (Game.Options.Profile == p), 20f).BR_DeprecateMe(0, 50, AnchorType.MAX, AnchorType.MIN);
        }

        void NewProfile()
        {
            Game.Screens.AddDialog(new TextDialog("Enter profile name:", (s) =>
            {
                Options.Profile p = new Options.Profile();
                p.Rename(s);
                Options.Options.Profiles.Add(p);
                Game.Options.ChangeProfile(p);
                Close(p.Name);
            }));
        }

        void RenameProfile()
        {
            Game.Screens.AddDialog(new TextDialog("Rename profile to:", (s) => { Game.Options.Profile.Rename(s); }));
        }
    }
}