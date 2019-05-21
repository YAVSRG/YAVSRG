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
                .TL_DeprecateMe(220, 10, AnchorType.MIN, AnchorType.MIN).BR_DeprecateMe(10, 60, AnchorType.MAX, AnchorType.MIN));
            f.AddChild(new SimpleButton("Rename Profile...", RenameProfile, () => false, 30f)
                .TL_DeprecateMe(220, 70, AnchorType.MIN, AnchorType.MIN).BR_DeprecateMe(10, 120, AnchorType.MAX, AnchorType.MIN));
            f.AddChild(new SimpleButton("Refresh Scores...", () =>
            {
                if (!recalcScores)
                    Game.Tasks.AddTask(Game.Options.Profile.Stats.RecalculateTop(), (b) => { recalcScores = false; }, "RecalculateTop", false);
                recalcScores = true;
            }, () => recalcScores, 30f)
                .TL_DeprecateMe(220, 130, AnchorType.MIN, AnchorType.MIN).BR_DeprecateMe(10, 180, AnchorType.MAX, AnchorType.MIN));
            f.AddChild(new TextPicker("", new[] { "3K", "4K", "5K", "6K", "7K", "8K", "9K", "10K" }, (int)Game.Options.Profile.DefaultKeymode, (i) => { Game.Options.Profile.PreferredKeymode = (Options.Profile.Keymode)i; }).TL_DeprecateMe(220, 60, AnchorType.MIN, AnchorType.MAX).BR_DeprecateMe(10, 10, AnchorType.MAX, AnchorType.MAX));
            f.AddChild(new TickBox("I only play", Game.Options.Profile.KeymodePreference, (b) => { Game.Options.Profile.KeymodePreference = b; }).TL_DeprecateMe(220, 110, AnchorType.MIN, AnchorType.MAX).BR_DeprecateMe(10, 60, AnchorType.MAX, AnchorType.MAX));

            var profileSelector = new FlowContainer();
            for (int i = 0; i < Options.Options.Profiles.Count; i++)
            {
                profileSelector.AddChild(ProfileButton(Options.Options.Profiles[i]));
            }
            AddChild(profileSelector.TL_DeprecateMe(10, 10, AnchorType.MIN, AnchorType.MIN).BR_DeprecateMe(210, 10, AnchorType.MIN, AnchorType.MAX));

            TL_DeprecateMe(0, 0, AnchorType.MIN, AnchorType.CENTER).BR_DeprecateMe(500, 0, AnchorType.MIN, AnchorType.MAX);
        }

        Widget ProfileButton(Options.Profile p)
        {
            return new SimpleButton(p.Name, () => { Game.Options.ChangeProfile(p); }, () => (Game.Options.Profile == p), 20f).BR_DeprecateMe(0, 50, AnchorType.MAX, AnchorType.MIN);
        }

        void NewProfile()
        {
            Game.Screens.AddDialog(new TextDialog("Enter profile name:", (s) =>
            {
                Options.Profile p = new Options.Profile() { Name = s, ProfilePath = new Regex("[^a-zA-Z0-9_-]").Replace(s, "") + ".json" };
                Options.Options.Profiles.Add(p);
                Game.Options.ChangeProfile(p);
                Close(p.Name);
            }));
        }

        void RenameProfile()
        {
            Game.Screens.AddDialog(new TextDialog("Rename profile to:", (s) => { Game.Options.Profile.Name = s; }));
        }
    }
}