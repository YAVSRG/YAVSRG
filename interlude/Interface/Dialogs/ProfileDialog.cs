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
        ScrollContainer profileSelector, physicalTop;

        public ProfileDialog(Action<string> action) : base(action)
        {
            AddChild(new FramedButton("buttonbase", "New Profile", NewProfile)
                .PositionTopLeft(200, 100, AnchorType.MAX, AnchorType.MAX).PositionBottomRight(0, 0, AnchorType.MAX, AnchorType.MAX));
            profileSelector = new ScrollContainer(5, 5, false);
            for (int i = 0; i < Options.Options.Profiles.Count; i++)
            {
                profileSelector.AddChild(ProfileButton(Options.Options.Profiles[i]));
            }
            AddChild(profileSelector.PositionTopLeft(200, 0, AnchorType.MAX, AnchorType.MIN).PositionBottomRight(0, 100, AnchorType.MAX, AnchorType.MAX));
            AddChild(new TextPicker("Keymode", new[] { "3K", "4K", "5K", "6K", "7K", "8K", "9K", "10K" }, 1, (i) => { ChangeKeymode(i); }).PositionBottomRight(100,50,AnchorType.MIN,AnchorType.MIN));
            physicalTop = new ScrollContainer(5, 5, false, 2, false, false);
            AddChild(physicalTop.PositionTopLeft(50, 100, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(250, 50, AnchorType.MAX, AnchorType.MAX));

            ChangeKeymode(1);
            //Game.Tasks.AddTask(Game.Options.Profile.Stats.RecalculateTop(), (b) => { ChangeKeymode(1); }, "RecalculateTop", false);
        }

        private void ChangeKeymode(int keymode)
        {
            Game.Tasks.AddTask((Output) => {
                physicalTop.Items().Clear();
                bool l = false;
                foreach (ScoreInfoProvider si in Game.Options.Profile.Stats.GetPhysicalTop(keymode))
                {
                    physicalTop.AddChild(new TopScoreCard(si, l, false).PositionBottomRight(0, 100, AnchorType.MAX, AnchorType.MIN));
                    l = !l;
                }
                return true;
            }, (b) => { }, "TopScores", false);
        }

        private Widget ProfileButton(Options.Profile p)
        {
            return new SimpleButton(p.Name, () => { Game.Options.ChangeProfile(p); }, () => (Game.Options.Profile == p), 20f).PositionBottomRight(0, 50, AnchorType.MAX, AnchorType.MIN);
        }

        private void NewProfile()
        {
            Game.Screens.AddDialog(new TextDialog("Enter Profile Name:", (s) =>
            {
                Options.Profile p = new Options.Profile() { Name = s, ProfilePath = new Regex("[^a-zA-Z0-9_-]").Replace(s, "") + ".json" };
                Options.Options.Profiles.Add(p);
                Game.Options.ChangeProfile(p);
                Close(p.Name);
            }));
        }
    }
}
