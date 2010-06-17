package gWidgetsrJava;

// TODO: * cut down imports
// * bug with class
// * add in different renderers for different classes (checkbox for logical,
//   combobox for factors)

//import javax.swing.*;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.*;
import javax.swing.ToolTipManager;
import javax.swing.JPopupMenu;
import javax.swing.JMenuItem;
import javax.swing.ButtonGroup;
import javax.swing.JRadioButtonMenuItem;

import java.awt.Component;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.*;
import java.util.Vector;


// This table class comes with scrollbars
public class gDf extends JPanel implements KeyListener, MouseListener, ActionListener {

    private JTable table;
    private JTableHeader tableHeader;
    private DefaultTableModel tableModel;

    // globals for storing data
    private String[] Rclass;

    // globals for the popupmenu
    private JPopupMenu popup;
    private JMenuItem menuItem;
    private JRadioButtonMenuItem[] rbMenuItem;
    private int currentColumn;// column for popup
    
    // constructor
    public gDf(int rows, int cols) {

	super(new GridLayout(1,0)); //container

	tableModel = new DefaultTableModel(rows,cols);
	table = new JTable(tableModel);

	table.addKeyListener(this);
	table.putClientProperty("terminateEditOnFocusLost", Boolean.TRUE);
	table.setAutoCreateColumnsFromModel(false); // see FAQ, this saves properties
	table.setAutoResizeMode(table.AUTO_RESIZE_OFF);

	// turn off tooltips -- is this faster?
	ToolTipManager.sharedInstance().unregisterComponent(table);
	ToolTipManager.sharedInstance().unregisterComponent(table.getTableHeader());

	// array to hold class information
	Rclass = new String[100]; // artificially cap at 100 variables

	//Create the popup menu for the columns
	popup = new JPopupMenu();
	// new column
	menuItem = new JMenuItem("New column");
	menuItem.setActionCommand("NewColumn");
	menuItem.addActionListener(this);
	popup.add(menuItem);
	// class checkbox
	popup.addSeparator();
	ButtonGroup group = new ButtonGroup();
	rbMenuItem = new JRadioButtonMenuItem[4];
	rbMenuItem[0] = new JRadioButtonMenuItem("character");
	rbMenuItem[0].setActionCommand("character");
	rbMenuItem[0].addActionListener(this);
	group.add(rbMenuItem[0]);
	popup.add(rbMenuItem[0]);

	rbMenuItem[1] = new JRadioButtonMenuItem("numeric");
	rbMenuItem[1].setActionCommand("numeric");
	rbMenuItem[1].addActionListener(this);
	group.add(rbMenuItem[1]);
	popup.add(rbMenuItem[1]);

	rbMenuItem[2] = new JRadioButtonMenuItem("logical");
	rbMenuItem[2].setActionCommand("logical");
	rbMenuItem[2].addActionListener(this);
	group.add(rbMenuItem[2]);
	popup.add(rbMenuItem[2]);

	rbMenuItem[3] = new JRadioButtonMenuItem("factor");
	rbMenuItem[3].setActionCommand("factor");
	rbMenuItem[3].addActionListener(this);
	group.add(rbMenuItem[3]);
	popup.add(rbMenuItem[3]);

	// edit name
	popup.addSeparator();
	menuItem = new JMenuItem("Edit variable name");
	menuItem.setActionCommand("EditVariable");
	menuItem.addActionListener(this);
	popup.add(menuItem);



	tableHeader = table.getTableHeader();
	//	tableHeader.addMouseListener(this);
	tableHeader.addMouseListener(this);

        //Create the scroll pane and add the table to it.
        JScrollPane scrollPane = 
	    new JScrollPane(table,
			    JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
			    JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
	


        //Add the scroll pane to this panel.
        add(scrollPane);
    }


    //override size issues with scrollbars
    // is this working with larger guys now?
    public Dimension getPreferredScrollableViewportSize() {
	Dimension size = table.getPreferredScrollableViewportSize();
	return new Dimension(Math.min(getPreferredSize().width, size.width), size.height);
    }
    
    // return table part
    public JTable getTable() {
	return table;
    }

    public void addRow() {
	int noCols = table.getColumnCount();
	String[] blankVals = new String[noCols];
	tableModel.addRow((Object[])blankVals);
    }

    // method to add a column to the model
    public void addColumn(String title, String theRclass){
	int theColumn = tableModel.getColumnCount();
	TableColumn newColumn = new TableColumn(theColumn);
	newColumn.setHeaderValue(title);
	table.addColumn(newColumn);
	tableModel.addColumn(theColumn);
	Rclass[theColumn] = theRclass;
    }


    // edit header names
    public Object getColumnName(int col) {
	TableColumnModel tcm = table.getColumnModel();
	TableColumn tc = tcm.getColumn(col);
	return tc.getHeaderValue();
    }

    public void setColumnName(int col, String value) {
	TableColumnModel tcm = table.getColumnModel();
	TableColumn tc = tcm.getColumn(col);
	tc.setHeaderValue(value);
	table.getTableHeader().repaint();
    }

    // edit column class
    public Object getColumnRclass(int col) {
	return Rclass[col];
    }

    public void setColumnRclass(int col, String value) {
	Rclass[col] = value;
    }



    // key listener
    // -> at end new column
    // Enter, Tab or downarrow at bottom new row
    
    /** Handle the key typed event from the text field. */
    public void keyTyped(KeyEvent e) {}

    /** Handle the key pressed event from the text field. */
    public void keyPressed(KeyEvent e) {

	//	int keyCode = e.getKeyCode();
	//	System.out.println("pressed"+keyCode+":"+KeyEvent.getKeyText(keyCode));

	// new column? If at end
	if(table.getSelectedColumn() == tableModel.getColumnCount() - 1 &&
	   (e.getKeyCode() == KeyEvent.VK_RIGHT ||
	    e.getKeyCode() == KeyEvent.VK_TAB)) {

	    // we had trouble with adding a column this way
	    // went with popup menu instead
	    // how to save value?

// 	    table.getCellEditor().stopCellEditing();



// 	    // where were we
// 	    int theRow = table.getSelectedRow();
// 	    int theColumn = tableModel.getColumnCount(); // new column number

// 	    // add new column
// 	    TableColumn newColumn = new TableColumn(theColumn);
// 	    newColumn.setHeaderValue("X"+theColumn);
// 	    table.addColumn(newColumn);
// 	    tableModel.addColumn(theColumn);
// 	    //	    tableModel.addColumn("name"); // get column name

// 	    // set the cursor
// 	    // This seems not to work!
// 	    table.setColumnSelectionInterval(theColumn+1,theColumn+1);
	} else if(table.getSelectedRow() == tableModel.getRowCount() - 1 &&
		  (e.getKeyCode() == KeyEvent.VK_DOWN ||
		   e.getKeyCode() == KeyEvent.VK_ENTER)) {
	    // add new row
	    int ncols = tableModel.getColumnCount();
	    tableModel.addRow(new Vector(ncols));
	}
	    
    }
    /** Handle the key released event from the text field. */
    public void keyReleased(KeyEvent e) {}


    /** mouse listener on table header */
    public void mouseClicked(MouseEvent e) {
	// what is class?
	TableColumnModel columnModel = tableHeader.getColumnModel();
	int viewColumn = columnModel.getColumnIndexAtX(e.getX());
	currentColumn = columnModel.getColumn(viewColumn).getModelIndex();
	//		System.out.println("current column"+currentColumn+"currentRclass"+Rclass[currentColumn]+"set popup accordingly");
	
	// adjust menuitem to reflect Rclass
	// BUGGY: this doesn't seem to work before the popup happens
	if (Rclass[currentColumn] == "character") {
	    rbMenuItem[0].setSelected(true);	    
	} else if (Rclass[currentColumn] == "numeric") {
	    rbMenuItem[1].setSelected(true);    
	} else if (Rclass[currentColumn] == "logical") {
	    rbMenuItem[2].setSelected(true);    
	} else if (Rclass[currentColumn] == "factor") {
	    rbMenuItem[3].setSelected(true);    
	} 
	
	popup.show(e.getComponent(),
		   e.getX(), e.getY());

	if (e.isPopupTrigger()) {
	    System.out.println("popup trigger");
	}
    }
    // added a fancier popup than this which just changed column name
// 	if (e.getClickCount() == 2) {

// 	    // Popup a menubar with:
// 	    // Change Name
// 	    // Type of variable (radio)
// 	    // new column
	    


// 	    System.out.println("Mouse event");
// 	    JTableHeader h = (JTableHeader) e.getSource();
// 	    TableColumnModel columnModel = h.getColumnModel();
// 	    int viewColumn = columnModel.getColumnIndexAtX(e.getX());
// 	    int column = columnModel.getColumn(viewColumn).getModelIndex();
// 	    if (column != -1) {
// 	    // change header on column
// 		System.out.println("Add dialog to column"+column);
		
// 		// also set class
// 		String s = 
// 		    (String)JOptionPane.showInputDialog(table,
// 							"Name of column",
// 							"Set column name",
// 							JOptionPane.PLAIN_MESSAGE,
// 							null,
// 							null,
// 							getColumnName(column));
		
// 		//If a string was returned, say so.
// 		if ((s != null) && (s.length() > 0)) {
// 		    setColumnName(column,s);
// 		}
// 	    }
// 	}
//    }
    public void mousePressed(MouseEvent e) {}
    public void mouseReleased(MouseEvent e) {}
    public void mouseEntered(MouseEvent e) {}
    public void mouseExited(MouseEvent e) {}




    // handler for popup  menu
    public void actionPerformed(ActionEvent e) {
	// work on this
	String actionCommand = e.getActionCommand();

	if(actionCommand == "NewColumn") {
	    addColumn("X"+tableModel.getColumnCount(),"character");
	} else if(actionCommand == "EditVariable") {
	    String s = 
		(String)JOptionPane.showInputDialog(table,
						    "Name of column",
						    "Set column name",
						    JOptionPane.PLAIN_MESSAGE,
						    null,
						    null,
						    getColumnName(currentColumn));
	    
	    //If a string was returned, say so.
	    if ((s != null) && (s.length() > 0)) {
		setColumnName(currentColumn,s);
	    }
	} else if(actionCommand == "character") { // change rClass
	    Rclass[currentColumn] = "character";
	    rbMenuItem[0].setSelected(true);
	} else if(actionCommand == "numeric") {
	    Rclass[currentColumn] = "numeric";
	    rbMenuItem[1].setSelected(true);
	} else if(actionCommand == "logical") {
	    Rclass[currentColumn] = "logical";
	    rbMenuItem[2].setSelected(true);
	} else if(actionCommand == "factor") {
	    Rclass[currentColumn] = "factor";
	    rbMenuItem[3].setSelected(true);
	} 
    }

}