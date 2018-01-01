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
            label1.Text = error;
        }
    }
}
