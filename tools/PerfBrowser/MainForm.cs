using System;
using System.Diagnostics;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;

namespace PerfBrowser
{
	/// <summary>
	/// Summary description for Form1.
	/// </summary>
	public class MainForm : System.Windows.Forms.Form
	{
        private System.Windows.Forms.ColumnHeader col1;
        private System.Windows.Forms.ColumnHeader col2;
        private System.Windows.Forms.ComboBox comboBox;
        private System.Windows.Forms.ListView lvCounters;
        private System.Windows.Forms.TreeView tvCategories;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Splitter splitter2;
        private System.Windows.Forms.Panel panel2;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		public MainForm()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();
		}

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if( disposing )
			{
				if (components != null) 
				{
					components.Dispose();
				}
			}
			base.Dispose( disposing );
		}

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
            this.lvCounters = new System.Windows.Forms.ListView();
            this.col1 = new System.Windows.Forms.ColumnHeader();
            this.col2 = new System.Windows.Forms.ColumnHeader();
            this.comboBox = new System.Windows.Forms.ComboBox();
            this.tvCategories = new System.Windows.Forms.TreeView();
            this.panel1 = new System.Windows.Forms.Panel();
            this.splitter2 = new System.Windows.Forms.Splitter();
            this.panel2 = new System.Windows.Forms.Panel();
            this.panel1.SuspendLayout();
            this.panel2.SuspendLayout();
            this.SuspendLayout();
            // 
            // lvCounters
            // 
            this.lvCounters.Anchor = (((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
                | System.Windows.Forms.AnchorStyles.Left) 
                | System.Windows.Forms.AnchorStyles.Right);
            this.lvCounters.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
                                                                                         this.col1,
                                                                                         this.col2});
            this.lvCounters.Cursor = System.Windows.Forms.Cursors.Default;
            this.lvCounters.Location = new System.Drawing.Point(0, 36);
            this.lvCounters.MultiSelect = false;
            this.lvCounters.Name = "lvCounters";
            this.lvCounters.Size = new System.Drawing.Size(468, 324);
            this.lvCounters.Sorting = System.Windows.Forms.SortOrder.Ascending;
            this.lvCounters.TabIndex = 2;
            this.lvCounters.View = System.Windows.Forms.View.Details;
            // 
            // col1
            // 
            this.col1.Text = "Name";
            this.col1.Width = 314;
            // 
            // col2
            // 
            this.col2.Text = "Value";
            this.col2.Width = 150;
            // 
            // comboBox
            // 
            this.comboBox.Anchor = ((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
                | System.Windows.Forms.AnchorStyles.Right);
            this.comboBox.Location = new System.Drawing.Point(8, 8);
            this.comboBox.Name = "comboBox";
            this.comboBox.Size = new System.Drawing.Size(688, 21);
            this.comboBox.TabIndex = 2;
            this.comboBox.Text = ".";
            this.comboBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.comboBox_KeyDown);
            // 
            // tvCategories
            // 
            this.tvCategories.Anchor = (((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
                | System.Windows.Forms.AnchorStyles.Left) 
                | System.Windows.Forms.AnchorStyles.Right);
            this.tvCategories.ImageIndex = -1;
            this.tvCategories.Location = new System.Drawing.Point(8, 36);
            this.tvCategories.Name = "tvCategories";
            this.tvCategories.SelectedImageIndex = -1;
            this.tvCategories.Size = new System.Drawing.Size(212, 324);
            this.tvCategories.Sorted = true;
            this.tvCategories.TabIndex = 3;
            this.tvCategories.AfterSelect += new System.Windows.Forms.TreeViewEventHandler(this.tvCategories_AfterSelect);
            // 
            // panel1
            // 
            this.panel1.Controls.AddRange(new System.Windows.Forms.Control[] {
                                                                                 this.tvCategories});
            this.panel1.Dock = System.Windows.Forms.DockStyle.Left;
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(220, 369);
            this.panel1.TabIndex = 0;
            // 
            // splitter2
            // 
            this.splitter2.Location = new System.Drawing.Point(220, 0);
            this.splitter2.Name = "splitter2";
            this.splitter2.Size = new System.Drawing.Size(8, 369);
            this.splitter2.TabIndex = 1;
            this.splitter2.TabStop = false;
            // 
            // panel2
            // 
            this.panel2.Controls.AddRange(new System.Windows.Forms.Control[] {
                                                                                 this.lvCounters});
            this.panel2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panel2.Location = new System.Drawing.Point(228, 0);
            this.panel2.Name = "panel2";
            this.panel2.Size = new System.Drawing.Size(476, 369);
            this.panel2.TabIndex = 3;
            // 
            // MainForm
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(704, 369);
            this.Controls.AddRange(new System.Windows.Forms.Control[] {
                                                                          this.comboBox,
                                                                          this.panel2,
                                                                          this.splitter2,
                                                                          this.panel1});
            this.Name = "MainForm";
            this.Text = "Performance Browser";
            this.Load += new System.EventHandler(this.MainForm_Load);
            this.panel1.ResumeLayout(false);
            this.panel2.ResumeLayout(false);
            this.ResumeLayout(false);

        }
		#endregion

		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main() 
		{
			Application.Run(new MainForm());
		}

        private void RefreshContent()
        {
            // Clear everthing currently displayed in the app
            this.tvCategories.Nodes.Clear();
            this.lvCounters.Items.Clear();
            this.Refresh();

            // Suspend drawing until we're done
            this.tvCategories.BeginUpdate();

            try
            {
                PerformanceCounterCategory[] aPerfCat = PerformanceCounterCategory.GetCategories(this.comboBox.Text);

                foreach(PerformanceCounterCategory perfcat in aPerfCat)
                {
                    TreeNode nodeCat = new TreeNode(perfcat.CategoryName);
                    this.tvCategories.Nodes.Add(nodeCat);

                    try
                    {
                        String[] astrInstNames = perfcat.GetInstanceNames();

                        foreach(String strInstName in astrInstNames)
                            nodeCat.Nodes.Add(new TreeNode(strInstName));
                    }
                    catch(Exception ex)
                    {
                        Console.WriteLine(ex);
                    }
                }

                //this.tvCategories.SelectedNode.Index = 0;
            }
            catch(Exception)
            {
                this.lvCounters.Items.Add("Machine not found.");
            }

            // Resume drawing
            this.tvCategories.EndUpdate();
        }

        private void MainForm_Load(object sender, System.EventArgs e)
        {
            this.comboBox.Text = SystemInformation.ComputerName;
            this.RefreshContent();
        }

        private void comboBox_KeyDown(object sender, System.Windows.Forms.KeyEventArgs e)
        {
            if(e.KeyCode == Keys.Enter)
                this.RefreshContent();
        }

        private void tvCategories_AfterSelect(object sender, System.Windows.Forms.TreeViewEventArgs e)
        {
            // Clear the listview to prep it for new data
            this.lvCounters.Items.Clear();

            // If a category was selected, that has multiple instances skip everything
            if(e.Node.Parent == null && e.Node.GetNodeCount(false) > 0)
                return;

            // If there are no instances just get the category name, otherwise get category and instance name
            String  strCategory;
            String  strInstance = null;

            if(e.Node.Parent == null)
            {
                strCategory = e.Node.Text;
            }
            else
            {
                strCategory = e.Node.Parent.Text;
                strInstance = e.Node.Text;
            }

            // Suspend drawing until we're done
            this.lvCounters.BeginUpdate();

            // Get the selected category
            PerformanceCounterCategory perfcat = new PerformanceCounterCategory(strCategory, this.comboBox.Text);

            try
            {
                InstanceDataCollectionCollection datacollcoll = perfcat.ReadCategory();

                foreach(InstanceDataCollection datacoll in datacollcoll.Values)
                {
                    foreach(InstanceData data in datacoll.Values)
                    {
                        if(strInstance == null || data.InstanceName == strInstance)
                        {
                            ListViewItem item = new ListViewItem(datacoll.CounterName);
                            item.SubItems.Add(data.RawValue.ToString());
                            this.lvCounters.Items.Add(item);
                            break;
                        }
                    }
                }
            }
            catch(Exception ex)
            {
                Console.WriteLine(ex);
            }

            // Result drawing
            this.lvCounters.EndUpdate();
        }
	}
}
