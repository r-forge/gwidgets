/* add idlehandler */

package gWidgetsrJava;

import java.awt.event.*;
import javax.swing.event.*;

import javax.swing.Timer;

public class gIdle implements ActionListener {

    public Timer timer;
    public int id;
    
    //constructor
    public gIdle(int interval, int ID) {
	timer = new Timer(interval, this);
	id = ID;
	timer.start();
    }
    
    public void stopTimer() {
	timer.stop();
    }
    
    // action handler
    public void actionPerformed(ActionEvent e) {  
	try {
	    if (id==-1)
		System.out.println("Can't find object for source "+e.getSource()+".\n");
	    else
		org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+id+"\",type=\"addIdleListener\")");
	} catch (Exception ex) {
	    System.out.println("Couldn't dispatch event, error: "+ex);
	}
    }
}
