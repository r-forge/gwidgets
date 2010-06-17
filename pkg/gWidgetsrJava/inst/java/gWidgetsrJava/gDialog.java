package gWidgetsrJava;

import java.beans.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.event.*;
import javax.swing.*;

public class gDialog {
    
    // the field
    JOptionPane op;

    // constructor
    public gDialog() {
	JOptionPane op = new JOptionPane();
    }

    //methods
    public  void gMessage(Component frame, String msg, String title, String icon) {

	int iconType;
	int buttonType;
	
	// icon type
	if(icon.equals("question")) {
	    iconType = JOptionPane.QUESTION_MESSAGE;
	} else if(icon.equals("warning")) {
	    iconType = JOptionPane.WARNING_MESSAGE;
	} else if(icon.equals("error")) {
	    iconType = JOptionPane.ERROR_MESSAGE;
	} else {
	    iconType = JOptionPane.INFORMATION_MESSAGE;
	}

	op.showMessageDialog(frame, msg, title, 
			     iconType);

    }
// 	// how to handle return?
// 	if(op.getOptionType() == JOptionPane.OK_OPTION) {
// 	    return 1;
// 	} else if(op.getOptionType() == JOptionPane.NO_OPTION) {
// 	    return 0;
// 	} else if(op.getOptionType() == JOptionPane.CANCEL_OPTION) {
// 	    return -1;
// 	} else {
// 	    return 0;
// 	}


    // gconfirm 1 for yes, 0 for no
    public  int gConfirm(Component frame, String msg, String title, String icon) {

	int iconType;

	if(icon.equals("question")) {
	    iconType = JOptionPane.QUESTION_MESSAGE;
	} else if(icon.equals("warning")) {
	    iconType = JOptionPane.WARNING_MESSAGE;
	} else if(icon.equals("error")) {
	    iconType = JOptionPane.ERROR_MESSAGE;
	} else {
	    iconType = JOptionPane.INFORMATION_MESSAGE;
	}

	int i = op.showConfirmDialog(frame, msg, title, JOptionPane.YES_NO_CANCEL_OPTION);

	if( i == JOptionPane.YES_OPTION) {
	    return 1;
	} else if ( i == JOptionPane.NO_OPTION) {
	    return 0;
	} else if ( i == JOptionPane.CANCEL_OPTION) {
	    return -1;
	} else {
	    return 0;
	}
    }
    
    // ginput return string from input
    public  String gInput(Component frame, String msg, String txt, String title, String icon) {

	int iconType;

	if(icon.equals("question")) {
	    iconType = JOptionPane.QUESTION_MESSAGE;
	} else if(icon.equals("warning")) {
	    iconType = JOptionPane.WARNING_MESSAGE;
	} else if(icon.equals("error")) {
	    iconType = JOptionPane.ERROR_MESSAGE;
	} else {
	    iconType = JOptionPane.INFORMATION_MESSAGE;
	}

	String s = (String)op.showInputDialog(frame,msg,title,iconType,null,null, txt);

	return s;
    }
    
    //gbasicdialog return 1 for yes, 0 for no
    public int  gBasicDialog(Component frame, Box widget, String title) {
	
	int i = op.showConfirmDialog(frame,
				     widget,
				     title,
				     JOptionPane.YES_NO_OPTION,
				     JOptionPane.PLAIN_MESSAGE);


	if( i == JOptionPane.YES_OPTION || 
	    i == JOPtionPane.OK_OPTION) {
	    return 1;
	} else if ( i == JOptionPane.NO_OPTION)
	    return 0;
	} else if ( i == JOptionPane.CANCEL_OPTION) {
	    return -1;
	} else {
	    return 0;
	}
	
    }
}
