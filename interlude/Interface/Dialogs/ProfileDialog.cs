using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using YAVSRG.Interface.Widgets;
using YAVSRG.Gameplay;
using System.Drawing;

namespace YAVSRG.Interface.Dialogs
{
    class ProfileDialog : FadeDialog
    {
        bool recalcScores;

        public ProfileDialog(Action<string> action) : base(action)
        {
            var f = new FrameContainer() { VerticalFade = 0, HorizontalFade = 100f };
            AddChild(f);
            f.AddChild(new SimpleButton("New Profile...", NewProfile, () => false, 30f)
                .PositionTopLeft(220, 10, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(10, 60, AnchorType.MAX, AnchorType.MIN));
            f.AddChild(new SimpleButton("Rename Profile...", RenameProfile, () => false, 30f)
                .PositionTopLeft(220, 70, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(10, 120, AnchorType.MAX, AnchorType.MIN));
            f.AddChild(new SimpleButton("Refresh Scores...", () =>
            {
                if (!recalcScores)
                    Game.Tasks.AddTask(Game.Options.Profile.Stats.RecalculateTop(), (b) => { recalcScores = false; }, "RecalculateTop", false);
                recalcScores = true;
            }, () => recalcScores, 30f)
                .PositionTopLeft(220, 130, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(10, 180, AnchorType.MAX, AnchorType.MIN));

            var profileSelector = new FlowContainer();
            for (int i = 0; i < Options.Options.Profiles.Count; i++)
            {
                profileSelector.AddChild(ProfileButton(Options.Options.Profiles[i]));
            }
            AddChild(profileSelector.PositionTopLeft(10, 10, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(210, 10, AnchorType.MIN, AnchorType.MAX));

            PositionTopLeft(0, 0, AnchorType.MIN, AnchorType.CENTER).PositionBottomRight(500, 0, AnchorType.MIN, AnchorType.MAX);
        }

        Widget ProfileButton(Options.Profile p)
        {
            return new SimpleButton(p.Name, () => { Game.Options.ChangeProfile(p); }, () => (Game.Options.Profile == p), 20f).PositionBottomRight(0, 50, AnchorType.MAX, AnchorType.MIN);
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