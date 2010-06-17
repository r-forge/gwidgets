// This just doesn't work. I tried to get the canvas, and not the
// frame. However, it appears there is some magic at the C level (the .so)
// file in the code
// To use this, you do something like
// > .jinit(.javaGD.get.class.path())
// > .javaGD.set.class.path("")
// > .jnew("gWidgetsrJava/gGraphics")
package gWidgetsrJava;

import org.rosuda.javaGD.*;
import java.awt.*;
import java.awt.event.*;
import java.lang.reflect.Method;

public class gGraphics extends GDInterface implements WindowListener {
    public Frame f;

    // constructor
    public gGraphics() {
        super();
    }
    
    public GDContainer getGDCanvas() {
	return c;
    }

    public void     gdOpen(double w, double h) {
        if (f!=null) gdClose();

        f=new Frame("JavaGD");
        f.addWindowListener(this);
        c=new GDCanvas(w, h);
	//        f.add((GDCanvas)c);
	//        f.pack();
	//        f.setVisible(true);
    }

    public void     gdActivate() {
        super.gdActivate();
        if (f!=null) {
            f.requestFocus();
	    //            f.setTitle("JavaGD "+((devNr>0)?("("+(devNr+1)+")"):"")+" *active*");
        }
    }

    public void     gdClose() {
        super.gdClose();
        if (f!=null) {
            c=null;
            f.removeAll();
            f.dispose();
            f=null;
        }
    }

    public void     gdDeactivate() {
        super.gdDeactivate();
	//        if (f!=null) f.setTitle("JavaGD "+((devNr>0)?("("+(devNr+1)+")"):""));
    }

    public void     gdNewPage(int devNr) { // new API: provides the device Nr.
        super.gdNewPage(devNr);
	//        if (f!=null) f.setTitle("JavaGD ("+(devNr+1)+")"+(active?" *active*":""));
    }

    /*-- WindowListener interface methods */
    
    public void windowClosing(WindowEvent e) {
        if (c!=null) executeDevOff();
    }
    public void windowClosed(WindowEvent e) {}
    public void windowOpened(WindowEvent e) {}
    public void windowIconified(WindowEvent e) {}
    public void windowDeiconified(WindowEvent e) {}
    public void windowActivated(WindowEvent e) {}
    public void windowDeactivated(WindowEvent e) {}
    
}
