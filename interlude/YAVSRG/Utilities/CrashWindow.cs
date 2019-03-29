using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Interlude.Utilities
{
    public partial class CrashWindow : Form
    {
        public CrashWindow(string error)
        {
            InitializeComponent();
            Text = "Interlude has crashed: " + IO.ResourceGetter.CrashSplash();
            label1.Text = IO.ResourceGetter.CrashSplash()+"\n\n"+error; //set display to the error message
        }

        private void button1_Click(object sender, EventArgs e)
        {
            System.Diagnostics.Process.Start("file://" + System.IO.Path.GetFullPath(Game.WorkingDirectory)); //multiplatform hack (i think)
        }
    }
}
