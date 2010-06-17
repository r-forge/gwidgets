package gWidgetsrJava;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import java.net.URL;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JTabbedPane;



public class gNotebook extends JTabbedPane implements MouseListener {
    
    public JTabbedPane notebook;

    public gNotebook() {

	notebook = new JTabbedPane();

    }
    
    // get notebook back
    public JTabbedPane getNotebook() {
	return notebook;

    //  This is taken from JGRHelp.java

    }
    
    public void addTab(String label, Component widget, String iconDir, Boolean closeButton) {

	// add a tab to the notebook. 
	// if closeButton = true add a close button
	CloseIcon closeIcon = new CloseIcon(iconDir+"close.jpg");
	
	if(closeButton == true) {
	    notebook.addTab(label,(Icon)closeIcon,widget);
	} else {
	    notebook.addTab(label,widget);
	}

	notebook.addMouseListener(this);

    }

    
    class CloseIcon extends ImageIcon {
	
	private int x, y, width, height;
	
	public CloseIcon(String path) {
	    super(path);
	    
	}
    
	public void paintIcon(Component c, Graphics g, int x, int y) {
	    this.x = x;
	    this.y = y;
	    width = getIconWidth();
	    height = getIconHeight();
	    super.paintIcon(c, g, x, y);
	}
	
	public Rectangle getBounds() {
	    return new Rectangle(x, y, width, height);
	}
    }
    
    
    
    /**
     * mouseClicked: handle mouse event: close tab.
     */
    public void mouseClicked(MouseEvent e) {
	int tabNumber = notebook.getUI().tabForCoordinate(notebook, e.getX(),
							  e.getY());
	if (tabNumber < 0)
	    return;
	Rectangle rect = ((CloseIcon) notebook.getIconAt(tabNumber)).getBounds();
	if (rect.contains(e.getX(), e.getY()))
	    notebook.remove(tabNumber);
    }

    /**
     * mouseEntered: handle mouse event.
     */
    public void mouseEntered(MouseEvent e) {
    }
    
    /**
     * mousePressed: handle mouse event.
     */
    public void mousePressed(MouseEvent e) {
    }
    
    /**
     * mouseReleased: handle mouse event.
     */
    public void mouseReleased(MouseEvent e) {
    }
    
    /**
     * mouseExited: handle mouse event.
     */
    public void mouseExited(MouseEvent e) {
    }
}    