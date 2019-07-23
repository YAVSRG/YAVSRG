using OpenTK.Input;
using Interlude.IO;

namespace Interlude.Options
{
    public class Keybinds
    {
        //general
        public Bind Exit = new KeyBind(Key.Escape);
        public Bind Select = new KeyBind(Key.Enter);
        public Bind Search = new KeyBind(Key.Tab);
        public Bind BossKey = new KeyBind(Key.Insert);
        public Bind CollapseToolbar = new AltBind(new KeyBind(Key.T), false, true);
        public Bind Help = new KeyBind(Key.Slash);
        public Bind Screenshot = new KeyBind(Key.F12);
        public Bind Chat = new KeyBind(Key.F8);
        public Bind Volume = new KeyBind(Key.AltLeft);
        public Bind Next = new KeyBind(Key.Right);
        public Bind Previous = new KeyBind(Key.Left);
        public Bind End = new AltBind(new KeyBind(Key.Right), false, true);
        public Bind Start = new AltBind(new KeyBind(Key.Left), false, true);

        public Bind Import = new AltBind(new KeyBind(Key.I), false, true);
        public Bind Options = new AltBind(new KeyBind(Key.O), false, true);
        public Bind Music = new AltBind(new KeyBind(Key.P), false, true);

        //level select
        public Bind UpRate = new KeyBind(Key.Plus);
        public Bind DownRate = new KeyBind(Key.Minus);
        public Bind UpHalfRate = new AltBind(new KeyBind(Key.Plus), false, true);
        public Bind DownHalfRate = new AltBind(new KeyBind(Key.Minus), false, true);
        public Bind UpSmallRate = new AltBind(new KeyBind(Key.Plus), true, false);
        public Bind DownSmallRate = new AltBind(new KeyBind(Key.Minus), true, false);
        public Bind Up = new KeyBind(Key.Up);
        public Bind Down = new KeyBind(Key.Down);
        public Bind Collections = new KeyBind(Key.F1);
        public Bind Goals = new KeyBind(Key.F2);
        public Bind Editor = new KeyBind(Key.F3);
        public Bind Mods = new KeyBind(Key.F4);
        public Bind RandomChart = new KeyBind(Key.F6);

        //play
        public Bind ChangeOffset = new KeyBind(Key.Plus);
        public Bind Skip = new KeyBind(Key.Space);
        public Bind HideUI = new KeyBind(Key.Tab);
    }
}
