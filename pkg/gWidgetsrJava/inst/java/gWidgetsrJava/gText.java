package gWidgetsrJava;

// created with code from http://www.cs.cf.ac.uk/Dave/HCI/HCI_Handout_CALLER/node124.html

/* Make an inteface for gText that allows us to add font attributes */


import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;

class gText extends JTextPane {
    // variables
    private JTextPane textComponent;
    private DefaultStyledDocument	doc;
    private Hashtable	attributes;
    
    public JTextPane gText(Boolean wrap) {
 
	// Create styles for the document
	StyleContext sc = new StyleContext();
	doc = new DefaultStyledDocument( sc );
	createStyles( sc ); // where we define the availablestyles

	// Create a text pane to display text
	textComponent = new JTextPane( doc );
	textComponent$setLineWrap(wrap);
	textComponent$setWrapStyleWord(true);

	return(textComponent);
    }

    // Delete this but it shows how add a style
    // Handle changes to the combobox (style changes)
//     public void actionPerformed( ActionEvent e ) {
// 	if( e.getSource() == styleCombo)
// 	    {
// 		try {
// 		    // Determine the new style
// 		    Style s = (Style)attributes.get( styleCombo.getSelectedItem() );
		    
// 		    // Set the style and return to the editor
// 		    doc.insertString( textComponent.getCaret().getDot(), " ", s );
// 		    textComponent.grabFocus();
// 		}
// 		catch( BadLocationException exception )
// 		    {
// 		    }
// 	    }
//     }
	
    // Create some different font styles
    public void createStyles( StyleContext sc )
    {
	Style myStyle;
	
	// Allocate a hashtasble for our styles
	attributes = new Hashtable();
	
	// No style
	myStyle = sc.addStyle( null, null );
	attributes.put( "none", myStyle ); 
	
	// Normal
	myStyle = sc.addStyle( null, null );
	StyleConstants.setLeftIndent( myStyle, 10 );
	StyleConstants.setRightIndent( myStyle, 10 );
	StyleConstants.setFontFamily( myStyle, "Helvetica" );
	StyleConstants.setFontSize( myStyle, 14 );
	StyleConstants.setSpaceAbove( myStyle, 4 );
	StyleConstants.setSpaceBelow( myStyle, 4 );
	attributes.put( "normal", myStyle ); 
	
	// Big
	myStyle = sc.addStyle( null, null );
	StyleConstants.setFontFamily( myStyle, "Dialog" );
	StyleConstants.setFontSize( myStyle, 28 );
	attributes.put( "big", myStyle ); 
	
	// Bold
	myStyle = sc.addStyle( null, null );
	StyleConstants.setBold( myStyle, true );
	attributes.put( "bold", myStyle ); 
    }

    // THE API
    public void clearBuffer() {
	// clear the buffer

    }


    // add text with attributes at some point
    // where: beginning, end, point
    public void addText(String text, String[] theAttributes, String where) {
	
    }
    
    // add text w/o attributes at some point: 
    public void addPlainText(String text, String where) {


    }
    
    // add a new line
    public void addNewline() {
	textComponent$append("\n");
    }

}