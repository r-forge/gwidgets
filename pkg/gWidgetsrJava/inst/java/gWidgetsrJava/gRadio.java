//
//  iRadio.java
//  JRGui
//
//  Created by Simon Urbanek on Thu Jul 22 2004.
//  Copyright (c) 2004 Simon Urbanek. All rights reserved.
//
//  $Id: iRadio.java 985 2004-07-22 15:01:07Z urbaneks $

package gWidgetsrJava;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.Vector;

public class gRadio extends Box implements ActionListener {
    ButtonGroup bg;
    String[] it;
    JRadioButton[] rb;
    
    public gRadio(String[] items, int selected, boolean horizontal) {
      super(horizontal?BoxLayout.X_AXIS:BoxLayout.Y_AXIS);
        it=items;
        ls=new Vector();
        rb=new JRadioButton[items.length];
        bg=new ButtonGroup();
        int i=0;
        while (i<items.length) {
            rb[i]=new JRadioButton(items[i], (i==selected));
            rb[i].addActionListener(this);
            add(rb[i]);
            bg.add(rb[i]);
            i++;
        }
        validate();
    }
   
    Vector ls;

    public int getLength() {
	return rb.length;
    }

    public String getLabel(int i) {
	return rb[i].getText();
    }

    public void setLabel(int i, String text) {
	rb[i].setText(text);
    }


    public int getSelectedID() {
        int i=0;
        while (i<rb.length) {
            if (rb[i].isSelected()) return i;
            i++;
        }
        return -1;
    }

    public void setSelectedID(int id) {
        if (id>=0 && id<rb.length)
            rb[id].setSelected(true);
    }
    
    public void addActionListener(ActionListener l) {
        ls.addElement(l);
    }

    public void removeActionListener(ActionListener l) {
        ls.removeElement(l);
    }

    public void actionPerformed(ActionEvent e) {
        int i=0;
        ActionEvent ne=new ActionEvent(this, e.getID(), e.getActionCommand(), e.getModifiers());
        while (i<ls.size()) {
            ActionListener l=(ActionListener) ls.elementAt(i++);
            l.actionPerformed(ne);
        }
    }
}
