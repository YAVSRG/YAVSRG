using System;
using Prelude.Utilities;
using Interlude.Interface.Widgets;
using Interlude.Gameplay;

namespace Interlude.Interface.Dialogs
{
    public class ProfileDialog : FadeDialog
    {
        bool recalcScores;

        public ProfileDialog(Action<string> action) : base((s) => { ChartLoader.Refresh(); action(s); })
        {
            var f = new FrameContainer() { VerticalFade = 0, HorizontalFade = 100f };
            AddChild(f);
            f.AddChild(new SimpleButton("New Profile...", NewProfile, () => false, null) { FontSize = 30 }
                .Reposition(220, 0, 10, 0, -10, 1, 60, 0));
            f.AddChild(new SimpleButton("Rename Profile...", RenameProfile, () => false, null) { FontSize = 30 }
                .Reposition(220, 0, 70, 0, -10, 1, 120, 0));
            f.AddChild(new SimpleButton("Refresh Scores...", () =>
            {
                if (!recalcScores)
                    Game.Tasks.AddTask(Game.Options.Profile.Stats.RecalculateTop(), (b) => { recalcScores = false; }, "RecalculateTop", false);
                recalcScores = true;
            }, () => recalcScores, null)
            { FontSize = 30 }
                .Reposition(220, 0, 130, 0, -10, 1, 180, 0));
            //todo: refresh these when profile changed
            f.AddChild(new Selector("", new[] { "3K", "4K", "5K", "6K", "7K", "8K", "9K", "10K" }, new SetterGetter<int>((i) => { Game.Options.Profile.PreferredKeymode = (Options.Profile.Keymode)i; }, () => (int)Game.Options.Profile.PreferredKeymode))
                .Reposition(220, 0, -60, 1, -10, 1, -10, 1));
            f.AddChild(new TickBox("I only play", new SetterGetter<bool>((b) => { Game.Options.Profile.KeymodePreference = b; }, () => Game.Options.Profile.KeymodePreference))
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
            return new SimpleButton(p.Name, () => { Game.Options.ChangeProfile(p); }, () => (Game.Options.Profile == p), null).Reposition(0, 0, 0, 0, 0, 1, 50, 0);
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