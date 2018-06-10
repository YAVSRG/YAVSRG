using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace YAVSRG.Utilities
{
    public class TrayIcon
    {
        NotifyIcon icon;

        public TrayIcon()
        {
            icon = new NotifyIcon();
            icon.Click += (o,e) => Game.Instance.ExpandFromIcon();
            icon.Icon = new System.Drawing.Icon("icon.ico");
            Hide();
            icon.Text = "Interlude";
        }

        public void Destroy()
        {
            icon.Dispose();
        }

        public void Show()
        {
            icon.Visible = true;
        }

        public void Hide()
        {
            icon.Visible = false;
        }

        public void Text(string s)
        {
            icon.ShowBalloonTip(2000, "Interlude", s, ToolTipIcon.Info);
        }
    }
}
