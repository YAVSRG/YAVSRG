using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK.Input;

namespace YAVSRG.Interface
{
    class Input
    {
        private static List<Key> k;
        private static List<Key> ok;
        private static List<MouseButton> m;
        private static List<MouseButton> om;

        public static int MouseX;
        public static int MouseY;
        public static int MouseScroll;

        public static void Init()
        {
            k = new List<Key>();
            ok = new List<Key>();
            m = new List<MouseButton>();
            om = new List<MouseButton>();
            
            Game.Instance.MouseDown += MouseDown;
            Game.Instance.MouseUp += MouseUp;
            Game.Instance.KeyDown += KeyDown;
            Game.Instance.KeyUp += KeyUp;
            Game.Instance.MouseMove += MouseMove;
            Game.Instance.MouseWheel += MouseWheel;
        }

        private static void MouseWheel(object sender, MouseWheelEventArgs e)
        {
            MouseScroll = e.Delta;
        }

        private static void MouseMove(object sender, MouseMoveEventArgs e)
        {
            MouseX = e.X - ScreenUtils.ScreenWidth;
            MouseY = e.Y - ScreenUtils.ScreenHeight;
        }

        private static void KeyUp(object sender, KeyboardKeyEventArgs e)
        {
            while (k.Contains(e.Key))
            {
                k.Remove(e.Key);
            }
        }

        private static void KeyDown(object sender, KeyboardKeyEventArgs e)
        {
            k.Add(e.Key);
        }

        private static void MouseUp(object sender, MouseButtonEventArgs e)
        {
            while (m.Contains(e.Button))
            {
                m.Remove(e.Button);
            }
        }

        private static void MouseDown(object sender, MouseButtonEventArgs e)
        {
            m.Add(e.Button);
        }

        public static bool MouseClick(MouseButton b)
        {
            return (m.Contains(b) && !om.Contains(b));
        }

        public static bool MouseRelease(MouseButton b)
        {
            return (!m.Contains(b) && om.Contains(b));
        }

        public static bool MousePress(MouseButton b)
        {
            return (m.Contains(b));
        }

        public static bool KeyTap(Key b)
        {
            return (k.Contains(b) && !ok.Contains(b));
        }

        public static bool KeyRelease(Key b)
        {
            return (!k.Contains(b) && ok.Contains(b));
        }

        public static bool KeyPress(Key b)
        {
            return (k.Contains(b));
        }

        public static void Update()
        {
            ok = new List<Key>(k);
            om = new List<MouseButton>(m);
            MouseScroll = 0;
        }
    }
}
