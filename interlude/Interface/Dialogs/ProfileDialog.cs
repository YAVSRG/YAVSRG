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
        ScrollContainer profileSelector;

        public ProfileDialog(Action<string> action) : base(action)
        {
            AddChild(new FramedButton("buttonbase", "New Profile", NewProfile)
                .PositionTopLeft(200, 100, AnchorType.MAX, AnchorType.MAX).PositionBottomRight(0, 0, AnchorType.MAX, AnchorType.MAX));
            profileSelector = new ScrollContainer(5, 5, false);
            for (int i = 0; i < Options.Options.Profiles.Count; i++)
            {
                profileSelector.AddChild(ProfileButton(Options.Options.Profiles[i]));
            }
            AddChild(profileSelector.PositionTopLeft(0.9f, 0, AnchorType.LERP, AnchorType.MIN).PositionBottomRight(0.99f, 100, AnchorType.LERP, AnchorType.MAX));
            //Game.Tasks.AddTask(Game.Options.Profile.Stats.RecalculateTop(), (b) => { ChangeKeymode(1); }, "RecalculateTop", false);
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
