package gWidgetsrJava;

// INSERT PROPER ATTRIBUTION HERE -- THIS CAME FROM SOMEWHERE ON THE WEB
// LIKELY The JFC Swing tutorial
// By Kathy Walrath, Mary Campione, Safari Tech Books Online 
//

import javax.swing.JFrame;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.*;
import javax.swing.ToolTipManager;
import javax.swing.TransferHandler;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.InputEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.datatransfer.*;
import java.awt.dnd.*;

import java.io.IOException;

import gWidgetsrJava.TableSorter;
import gWidgetsrJava.ColoredTable;

// This table class comes with scrollbars
public class gTable extends JPanel implements MouseMotionListener {

    // add in sorting along headers
    // not editable (TableSorter
    // remove tooltips for speed

    //    private JTable table;
    private ColoredTable table;
    private DefaultTableModel tableModel;

    // globals for storing data
    private String[] Rclass;
    
    // special column
    private int chosenColumn;	//  0 based
    

    // constructor
    public gTable(int rows, int cols, int chosencolumn) {

	// fro JTable java tutorial
	super(new GridLayout(1,0)); //container

	tableModel = new DefaultTableModel(rows,cols);

	TableSorter sorter = new TableSorter(tableModel);
	table = new ColoredTable(sorter);             
	sorter.setTableHeader(table.getTableHeader()); 

	// properties
	table.setAutoResizeMode(table.AUTO_RESIZE_OFF);
	table.setDragEnabled(true); // DND
	table.setTransferHandler(new TableTransferHandler());
	table.setMaximumSize(new Dimension(1600,1600));
	//    This sets the preferred size of the viewport, and
	// is the proper way to get big tables
	// see JTable FAQ for this, and overriding the getPreferredSize method
	table.setPreferredScrollableViewportSize(table.getPreferredSize());

	// turn off tooltips -- is this faster?
	ToolTipManager.sharedInstance().unregisterComponent(table);
	ToolTipManager.sharedInstance().unregisterComponent(table.getTableHeader());

        //Set up tool tips for column headers.
	//        table.getTableHeader().setToolTipText("Click to specify sorting; Control-Click to specify secondary sorting");
	
	// array to hold class information
	Rclass = new String[100]; // artificially cap at 100 variables


        //Create the scroll pane and add the table to it.
        JScrollPane scrollPane = new JScrollPane(table);
	// set size on scrollpane
	scrollPane.setMaximumSize(new Dimension(1600,1600));


        //Add the scroll pane to this panel.
        add(scrollPane);

	// set the column
	chosenColumn = chosencolumn;
    }




    // override size issues with scrollbars
    public Dimension getPreferredScrollableViewportSize() {
	Dimension size = table.getPreferredScrollableViewportSize();
	return new Dimension(Math.min(getPreferredSize().width, size.width), size.height);
    }


    // TableModel interface methods 
    public void addRow() {
	int noCols = table.getColumnCount();
	String[] blankVals = new String[noCols];
	tableModel.addRow((Object[])blankVals);
    }
    
    // this isnt working
    // override table model
    public class myTableModel extends AbstractTableModel {
	private TableModel tableModel;

	// constructor
	public myTableModel(TableModel curModel) {
	    tableModel = curModel;
	}

		


	public int getRowCount() {
	    return (tableModel == null) ? 0 : tableModel.getRowCount();
	}

	public int getColumnCount() {
	    return (tableModel == null) ? 0 : tableModel.getColumnCount();
	}

	public String getColumnName(int column) {
	    return tableModel.getColumnName(column);
	}

	public Class getColumnClass(int column) {
	    return tableModel.getColumnClass(column);
	}

	public boolean isCellEditable(int row, int column) {
	    return false;
	}

	public Object getValueAt(int row, int column) {
	    return tableModel.getValueAt(row, column);
	}
	
	public void setValueAt(Object aValue, int row, int column) {
	    tableModel.setValueAt(aValue,row, column);
	}
    }

    
    // edit column class
    public Object getColumnRclass(int col) {
	return Rclass[col];
    }

    public void setColumnRclass(int col, String value) {
	Rclass[col] = value;
	calcColumnWidths(table);
    }



    // return table
    public JTable getTable() {
	return table;
    }

    /* DND support */
public abstract class StringTransferHandler extends TransferHandler {
    
    protected abstract String exportString(JComponent c);
    protected abstract void importString(JComponent c, String str);
    protected abstract void cleanup(JComponent c, boolean remove);
    
    protected Transferable createTransferable(JComponent c) {
        return new StringSelection(exportString(c));
    }
    
    public int getSourceActions(JComponent c) {
        return COPY_OR_MOVE;
    }
    
    public boolean importData(JComponent c, Transferable t) {
        if (canImport(c, t.getTransferDataFlavors())) {
            try {
                String str = (String)t.getTransferData(DataFlavor.stringFlavor);
                importString(c, str);
                return true;
            } catch (UnsupportedFlavorException ufe) {
            } catch (IOException ioe) {
            }
        }

        return false;
    }
    
    protected void exportDone(JComponent c, Transferable data, int action) {
        cleanup(c, action == MOVE);
    }
    
    public boolean canImport(JComponent c, DataFlavor[] flavors) {
        for (int i = 0; i < flavors.length; i++) {
            if (DataFlavor.stringFlavor.equals(flavors[i])) {
                return true;
            }
        }
        return false;
    }
    
}


    public class TableTransferHandler extends StringTransferHandler {
	private int[] rows = null;
	private int addIndex = -1; //Location where items were added
	private int addCount = 0;  //Number of items added.
	
	protected String exportString(JComponent c) {
	    JTable table = (JTable)c;
	    int row = table.getSelectedRow();

	    Object value = tableModel.getValueAt(row, chosenColumn);

	    return value.toString();
    }

     protected void importString(JComponent c, String str) {
         JTable target = (JTable)c;
         DefaultTableModel model = (DefaultTableModel)target.getModel();
         int index = target.getSelectedRow();

         //Prevent the user from dropping data back on itself.
         //For example, if the user is moving rows #4,#5,#6 and #7 and
         //attempts to insert the rows after row #5, this would
         //be problematic when removing the original rows.
         //So this is not allowed.
         if (rows != null && index >= rows[0] - 1 &&
               index <= rows[rows.length - 1]) {
             rows = null;
             return;
         }

         int max = model.getRowCount();
         if (index < 0) {
             index = max;
         } else {
             index++;
             if (index > max) {
                 index = max;
             }
         }
         addIndex = index;
         String[] values = str.split("\n");
         addCount = values.length;
         int colCount = target.getColumnCount();
         for (int i = 0; i < values.length && i < colCount; i++) {
             model.insertRow(index++, values[i].split(","));
         }
     }
    protected void cleanup(JComponent c, boolean remove) {
        JTable source = (JTable)c;
        if (remove && rows != null) {
            DefaultTableModel model =
                 (DefaultTableModel)source.getModel();

            //If we are moving items around in the same table, we
            //need to adjust the rows accordingly, since those
            //after the insertion point have moved.
            if (addCount > 0) {
                for (int i = 0; i < rows.length; i++) {
                    if (rows[i] > addIndex) {
                        rows[i] += addCount;
                    }
                }
            }
            for (int i = rows.length - 1; i >= 0; i--) {
                model.removeRow(rows[i]);
            }
        }
        rows = null;
        addCount = 0;
        addIndex = -1;
    }
}


    // dnd through mousemotionlistener
    MouseEvent firstMouseEvent;


    public void mousePressed(MouseEvent e) {
	//Don't bother to drag if there is no image.
	firstMouseEvent = e;
	e.consume();
    }

    public void mouseDragged(MouseEvent e) {
	//Don't bother to drag if the component displays no image.

	if (firstMouseEvent != null) {
	    e.consume();

	    //If they are holding down the control key, COPY rather than MOVE
	    int ctrlMask = InputEvent.CTRL_DOWN_MASK;
	    int action = TransferHandler.COPY;

	    int dx = Math.abs(e.getX() - firstMouseEvent.getX());
	    int dy = Math.abs(e.getY() - firstMouseEvent.getY());
	    //Arbitrarily define a 5-pixel shift as the
	    //official beginning of a drag.
	    if (dx > 5 || dy > 5) {
		//This is a drag, not a click.
		JComponent c = (JComponent)e.getSource();
		
		//Tell the transfer handler to initiate the drag.
		TransferHandler handler = c.getTransferHandler();
		handler.exportAsDrag(c, firstMouseEvent, action);
		firstMouseEvent = null;
	    }
	}
    }

    public void mouseReleased(MouseEvent e) {
	firstMouseEvent = null;
    }

    public void mouseMoved(MouseEvent e) {
	firstMouseEvent = null;
    }



    /* Calculate column widgths */
    public static void calcColumnWidths(JTable table)
    {
	JTableHeader header = table.getTableHeader();
	
	TableCellRenderer defaultHeaderRenderer = null;
	
	if (header != null)
	    defaultHeaderRenderer = header.getDefaultRenderer();
	
	TableColumnModel columns = table.getColumnModel();
	TableModel data = table.getModel();
	
	int margin = columns.getColumnMargin(); // only JDK1.3
	
	int rowCount = data.getRowCount();
	
	int totalWidth = 0;
	
	for (int i = columns.getColumnCount() - 1; i >= 0; --i)
	    {
		TableColumn column = columns.getColumn(i);
		
		int columnIndex = column.getModelIndex();
		
		int width = -1; 
		
		TableCellRenderer h = column.getHeaderRenderer();
		
		if (h == null)
		    h = defaultHeaderRenderer;
		
		if (h != null) // Not explicitly impossible
		    {
			Component c = h.getTableCellRendererComponent
			    (table, column.getHeaderValue(),
			     false, false, -1, i);
			
			width = c.getPreferredSize().width;
		    }
		
		for (int row = rowCount - 1; row >= 0; --row)
		    {
			TableCellRenderer r = table.getCellRenderer(row, i);
			
			Component c = r.getTableCellRendererComponent
			    (table,
			     data.getValueAt(row, columnIndex),
			     false, false, row, i);
			
			width = Math.max(width, c.getPreferredSize().width);
		    }
		
		if (width >= 0)
		    column.setPreferredWidth(width + margin); // <1.3: without margin
		else
		    ; // ???
		
        totalWidth += column.getPreferredWidth();
	    }
	
	// only <1.3:   totalWidth += columns.getColumnCount() * columns.getColumnMargin();
	
	
	/* If you like; This does not make sense for two many columns!
	   Dimension size = table.getPreferredScrollableViewportSize();
	   
	   size.width = totalWidth;
	   
	   table.setPreferredScrollableViewportSize(size);
	*/
	
	// table.sizeColumnsToFit(-1); <1.3; possibly even table.revalidate()
	
	// if (header != null)
    //     header.repaint(); only makes sense when the header is visible (only <1.3)
    }

    
}