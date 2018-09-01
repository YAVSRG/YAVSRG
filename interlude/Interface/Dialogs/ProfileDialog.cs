using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using YAVSRG.Interface.Widgets;

namespace YAVSRG.Interface.Dialogs
{
    class ProfileDialog : Dialog
    {
        public ProfileDialog(Action<string> action) : base(action)
        {
            PositionTopLeft(-200, -100, AnchorType.CENTER, AnchorType.CENTER);
            PositionBottomRight(200, 100, AnchorType.CENTER, AnchorType.CENTER);
            AddChild(new FramedButton("buttonbase","New Profile",NewProfile)
                .PositionTopLeft(0,-100,AnchorType.MIN,AnchorType.MIN).PositionBottomRight(0,0,AnchorType.MAX,AnchorType.MIN));
            for (int i = 0; i < Options.Options.Profiles.Count; i++)
            {
                AddChild(new FramedButton("buttonbase", Options.Options.Profiles[i].Name, ClosureToChangeProfile(Options.Options.Profiles[i]))
                    .PositionTopLeft(0, i*100, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(0, 100+i*100, AnchorType.MAX, AnchorType.MIN));
            }
        }

        public Action ClosureToChangeProfile(Options.Profile p)
        {
            return () => { Game.Options.ChangeProfile(p); Close(p.Name); };
        }

        public void NewProfile()
        {
            Game.Screens.AddDialog(new TextDialog("Enter Profile Name:", (s) => {
                Options.Profile p = new Options.Profile() { Name = s, ProfilePath = new Regex("[^a-zA-Z0-9_-]").Replace(s, "")+".json" };
                Options.Options.Profiles.Add(p);
                Game.Options.ChangeProfile(p);
                Close(p.Name);
            }));
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
        }
    }
}
