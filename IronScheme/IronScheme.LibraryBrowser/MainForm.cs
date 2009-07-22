using System.Windows.Forms;
using System.Diagnostics;

namespace IronScheme.LibraryBrowser
{
  public partial class MainForm : Form
  {
    public MainForm()
    {
      InitializeComponent();
    }

    public Process Process { get; set; }

    private void webBrowser1_Navigated(object sender, WebBrowserNavigatedEventArgs e)
    {
      if (Process.HasExited)
      {
        MessageBox.Show("WebServer did not start correctly. Exited with code: " + Process.ExitCode, "Error");
      }
    }
  }
}
