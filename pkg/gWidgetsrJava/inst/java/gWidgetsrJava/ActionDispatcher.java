//
//  ActionDispatcher.java
//  iWidgets
//
//  Created by Simon Urbanek on Wed Jul 21 2004.
//  Copyright (c) 2004 Simon Urbanek. All rights reserved.
//
//  $Id: ActionDispatcher.java 988 2004-07-22 20:21:17Z urbaneks $

package gWidgetsrJava;

import java.awt.*;
import java.awt.event.*;
import javax.swing.event.*;
import java.util.*;

public class ActionDispatcher implements ActionListener, ChangeListener, DocumentListener, ItemListener, ListSelectionListener, MouseListener, WindowListener {
    static ActionDispatcher globalDispatcher = new ActionDispatcher();
    static int uaid = 1;

    Vector list;
    
    class ADEntry {
        public Object obj;
        int    uaid;
        ADEntry(Object o, int id) { obj=o; uaid=id; }
    }
    
    public ActionDispatcher() {
        list=new Vector();
    }

    public static ActionDispatcher getGlobalDispatcher() { return globalDispatcher; }
    
    synchronized public String add(Object o, Integer id) {
	//        list.addElement(new ADEntry(o, uaid));
	list.addElement(new ADEntry(o, id));
        // uaid++;
        return "actionDispEl."+(id-1);
    }

    synchronized public String rm(Object o) {
        int i=0, ct=list.size();
        while (i<ct) {
            ADEntry e=(ADEntry)list.elementAt(i);
            if (e.obj==o) {
                list.removeElement(e);
                return "actionDispEl."+e.uaid;
            }
            i++;
        }
        return null;
    }

    synchronized public int getID(Object o) {
        int i=0, ct=list.size();
        while (i<ct) {
            ADEntry e=(ADEntry)list.elementAt(i);
            if (e.obj==o)
		return e.uaid;
		//                return "actionDispEl."+e.uaid;
            i++;
        }
        return -1;		//  IDS are > 0
    }
    
    synchronized public Object getObject(String s) {
        if (s.substring(0,13).equals("actionDispEl.")) {
            String ids=s.substring(13);
            int i=0;
            try {
                i=Integer.parseInt(ids);
            } catch (NumberFormatException nfe) {}
            if (i<1) return null;
            int j=0, ct=list.size();
            while (j<ct) {
                ADEntry e=(ADEntry)list.elementAt(j);
                if (e.uaid==i) return e.obj;
                j++;
            }
        }
        return null;
    }

    synchronized public void rmByName(String s) {
        if (s.substring(0,13).equals("actionDispEl.")) {
            String ids=s.substring(13);
            int i=0;
            try {
                i=Integer.parseInt(ids);
            } catch (NumberFormatException nfe) {}
            if (i<1) return;
            int j=0, ct=list.size();
            while (j<ct) {
                ADEntry e=(ADEntry)list.elementAt(j);
                if (e.uaid==i) {
                    list.removeElementAt(j);
                    return;
                }
                j++;
            }
        }
    }
    
    /* The listeners */

    //ActionListener
    public void actionPerformed(ActionEvent e) {
        try {
            int sid=getID(e.getSource());
            if (sid==-1)
                System.out.println("Can't find object for action "+e+" and source "+e.getSource()+".\n");
            else
                org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"ActionEvent\")");
        } catch (Exception ex) {
            System.out.println("Couldn't dispatch event, error: "+ex);
        }
    }

    // ChangeListener
    public void stateChanged(ChangeEvent e) {
        try {
	    int sid=getID(e.getSource());
            if (sid==-1)
                System.out.println("Can't find object for source "+e.getSource()+".\n");
            else
                org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"ChangeEvent\")");
        } catch (Exception ex) {
            System.out.println("Couldn't dispatch event, error: "+ex);
        }
    }

    // ItemListener
    public void itemStateChanged(ItemEvent e) {
        try {
	    int sid=getID(e.getSource());
            if (sid==-1)
                System.out.println("Can't find object for source "+e.getSource()+".\n");
            else
                org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"ItemEvent\")");
        } catch (Exception ex) {
            System.out.println("Couldn't dispatch event, error: "+ex);
        }
    }

    // ListSelectionListener
    public void valueChanged(ListSelectionEvent e) {
        try {
            int sid=getID(e.getSource());
            if (sid==-1)
                System.out.println("Can't find object for source "+e.getSource()+".\n");
            else
                org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"ListSelectionEvent\")");
        } catch (Exception ex) {
            System.out.println("Couldn't dispatch event, error: "+ex);
        }
    }

    // 
    public void windowActivated(WindowEvent e) {
        try {
            int sid=getID(e.getSource());
            if (sid==-1)
                System.out.println("Can't find object for source "+e.getSource()+".\n");
            else
                org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"windowActivated\")");
        } catch (Exception ex) {
            System.out.println("Couldn't dispatch event, error: "+ex);
        }
    }
    public void windowClosing(WindowEvent e) {
        try {
            int sid=getID(e.getSource());
            if (sid==-1)
                System.out.println("Can't find object for source "+e.getSource()+".\n");
            else
                org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"WindowClosed\")");
        } catch (Exception ex) {
            System.out.println("Couldn't dispatch event, error: "+ex);
        }
}
    public void windowDeactivated(WindowEvent e) {
        try {
            int sid=getID(e.getSource());
            if (sid==-1)
                System.out.println("Can't find object for source "+e.getSource()+".\n");
            else
                org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"windowDeactivated\")");
        } catch (Exception ex) {
            System.out.println("Couldn't dispatch event, error: "+ex);
        }
}
    public void windowDeiconified(WindowEvent e) {
        try {
            int sid=getID(e.getSource());
            if (sid==-1)
                System.out.println("Can't find object for source "+e.getSource()+".\n");
            else
                org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"windowDeiconified\")");
        } catch (Exception ex) {
            System.out.println("Couldn't dispatch event, error: "+ex);
        }
}
    public void windowIconified(WindowEvent e) {
        try {
            int sid=getID(e.getSource());
            if (sid==-1)
                System.out.println("Can't find object for source "+e.getSource()+".\n");
            else
                org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"windowIconified\")");
        } catch (Exception ex) {
            System.out.println("Couldn't dispatch event, error: "+ex);
        }
}
    public void windowOpened(WindowEvent e) {
        try {
            int sid=getID(e.getSource());
            if (sid==-1)
                System.out.println("Can't find object for source "+e.getSource()+".\n");
            else
                org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"windowOpened\")");
        } catch (Exception ex) {
            System.out.println("Couldn't dispatch event, error: "+ex);
        }
}
    public void windowClosed(WindowEvent e) {
        try {
            int sid=getID(e.getSource());
            if (sid==-1)
                System.out.println("Can't find object for source "+e.getSource()+".\n");
            else
                org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"windowClosed\")");
        } catch (Exception ex) {
            System.out.println("Couldn't dispatch event, error: "+ex);
        }
    }

    //

    public void insertUpdate (DocumentEvent e) {
        try {
            int sid=getID(e.getDocument());
            if (sid==-1)
                System.out.println("Can't find object for source "+e.getDocument()+".\n");
            else
                org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"insertUpdate\")");
        } catch (Exception ex) {
            System.out.println("Couldn't dispatch event, error: "+ex);
        }

    }

    public void removeUpdate (DocumentEvent e) {
        try {
            int sid=getID(e.getDocument());
            if (sid==-1)
                System.out.println("Can't find object for source "+e.getDocument()+".\n");
            else
                org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"removeUpdate\")");
        } catch (Exception ex) {
            System.out.println("Couldn't dispatch event, error: "+ex);
        }

    }

    public void changedUpdate (DocumentEvent e) {
        try {
            int sid=getID(e.getDocument());
            if (sid==-1)
                System.out.println("Can't find object for source "+e.getDocument()+".\n");
            else
                org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"changedUpdate\")");
        } catch (Exception ex) {
            System.out.println("Couldn't dispatch event, error: "+ex);
        }

    }

    // for double clicks on table: http://www.chka.de/swing/table/faq.html
    public void mouseClicked(MouseEvent e) {

	if(e.getClickCount() == 1) {

	    try {
		int sid=getID(e.getSource());
		if (sid==-1)
		    System.out.println("Can't find object for source "+e.getSource()+".\n");
		else
		    org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"mouseClicked\")");
	    } catch (Exception ex) {
		System.out.println("Couldn't dispatch event, error: "+ex);
	    }
	}

	/// doubleclicke
        if (e.getClickCount() == 2) {
	    try {
		int sid=getID(e.getSource());
		if (sid==-1)
		    System.out.println("Can't find object for source "+e.getSource()+".\n");
		else
		    org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"TwoMouseClicked\")");
	    } catch (Exception ex) {
		System.out.println("Couldn't dispatch event, error: "+ex);
	    }
	    
	}
    }
    
    public void mouseExited(MouseEvent e) {
	try {
	    int sid=getID(e.getSource());
	    if (sid==-1)
		System.out.println("Can't find object for source "+e.getSource()+".\n");
	    else
		org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"mouseExited\")");
	} catch (Exception ex) {
	    System.out.println("Couldn't dispatch event, error: "+ex);
	}
    }
    public void mouseEntered(MouseEvent e) {
	try {
	    int sid=getID(e.getSource());
	    if (sid==-1)
		System.out.println("Can't find object for source "+e.getSource()+".\n");
	    else
		org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"mouseEntered\")");
	} catch (Exception ex) {
	    System.out.println("Couldn't dispatch event, error: "+ex);
	}
    }
    public void mouseReleased(MouseEvent e) {
	try {
	    int sid=getID(e.getSource());
	    if (sid==-1)
		System.out.println("Can't find object for source "+e.getSource()+".\n");
	    else
		org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"mouseReleased\")");
	} catch (Exception ex) {
	    System.out.println("Couldn't dispatch event, error: "+ex);
	}

}
    public void mousePressed(MouseEvent e) {
	try {
	    int sid=getID(e.getSource());
	    if (sid==-1)
		System.out.println("Can't find object for source "+e.getSource()+".\n");
	    else
		org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"mousePressed\")");
	} catch (Exception ex) {
	    System.out.println("Couldn't dispatch event, error: "+ex);
	}
    }


}


