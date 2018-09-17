using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace YAVSRG.Interface
{
    public partial class CrashWindow : Form
    {
        public CrashWindow(string error)
        {
            InitializeComponent();
            label1.Text = "[22:48] Xonica: why do you complain about other rhythm games when yours just crashes\n\n"+error; //set display to the error message
        }

        private void button1_Click(object sender, EventArgs e)
        {
            System.Diagnostics.Process.Start("file://" + Game.WorkingDirectory); //multiplatform hack (i think)
        }
    }
}
