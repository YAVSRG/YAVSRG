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
            label1.Text = error; //set display to the error message
        }

        private void button1_Click(object sender, EventArgs e)
        {
            System.Diagnostics.Process.Start("file://" + Content.WorkingDirectory); //multiplatform hack (i think)
        }
    }
}
