package gWidgetsrJava;

import java.beans.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.event.*;
import javax.swing.*;


public class gExpandGroup implements ActionListener {

    // the values
    private JButton expandButton;
    private Boolean expandState;
    private JLabel  theLabel;
    private JPanel  theBox;
    private JPanel  theLayout;	// the component that gets returned
    private JPanel BigBox ;

    private String iconDir;
    //constructor
    public  gExpandGroup(String text, String icondir) {
	
	iconDir = icondir;
	expandButton = new JButton();
	expandButton.setBorderPainted(false);
	expandButton.setContentAreaFilled(false);
	// expandButton.setIcon(new ImageIcon(iconDir+"symbol_dntriangle.gif"));
	expandButton.setText("(+)");
	expandState = false;

	theLabel = new JLabel(text);
	theLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
	theBox = new JPanel();
	theBox.setVisible(false);	    
	theBox.setMaximumSize(new Dimension(Short.MAX_VALUE,
					    Short.MAX_VALUE));

	
	theLayout = new JPanel();
	theLayout.setLayout(new GridBagLayout());
	theLayout.setAlignmentX(Component.LEFT_ALIGNMENT);
	theLayout.setAlignmentY(Component.TOP_ALIGNMENT);


	BigBox = new JPanel();
	BigBox.setLayout(new BoxLayout(BigBox, BoxLayout.PAGE_AXIS));
	BigBox.add(theLayout);
	BigBox.add(Box.createHorizontalGlue());


	GridBagConstraints c = new GridBagConstraints();

	// expandButton
	c.gridwidth = 1;
	c.gridheight = 1;
	c.weightx = 0.0;
	c.weighty = 0.0;
	c.gridx = 0;
	c.gridy = 0;
	theLayout.add(expandButton,c);

	// theLabel
	c.weightx = 1.0;
	c.weighty = 0.0;
	c.gridx = 1;
	c.gridwidth = GridBagConstraints.REMAINDER; //end row
	theLayout.add(theLabel,c);

	// Add in filler that gets the weight if the Box is not visible
	c.weighty = 0.1;
	c.gridx = 1; 
	c.gridwidth=2;
	c.gridy = 1;
	theLayout.add(new Box(0),c);

	// the main Box (How to hide this)
	c.weightx = 1.0;	
	c.weighty = 1.0;	
	c.gridwidth = 2;
	c.gridheight = GridBagConstraints.REMAINDER; //end row
	c.gridx = 2;
	c.gridy = 2;
	c.fill = GridBagConstraints.BOTH; // FILL?
	theLayout.add(theBox,c);


	// action handler for expandButton
	expandButton.addActionListener(this);
	expandButton.setActionCommand("toggle");
	
	
    }

    public JPanel returnContainer() {
	// return container
	//return theLayout;
	return BigBox;
    }


    // action handler for expandButton
    public void actionPerformed(ActionEvent e) {
	if(expandState == false) {
	    // open up
	    setVisibility(true);
	} else {
	    // close up
	    setVisibility(false);
	}
    }
    


    // methods
    // get and set visibility of extra widget
    public Boolean getVisibility() {
	return expandState;
    }

    public void setVisibility(Boolean state) {
	// set open if state == true, othewise close

	if(state == true) {
	    // open up
	    theBox.setVisible(true);
	    //	    expandButton.setIcon(new ImageIcon(iconDir+"symbol_uptriangle.jpg"));
	    expandButton.setText("(-)");
	    expandState = true;
	} else {
	    // close up
	    theBox.setVisible(false);	    
	    //	    expandButton.setIcon(new ImageIcon(iconDir+"symbol_dntriangle.jpg"));
	    expandButton.setText("(+)");
	    expandState = false;
	}
    }

    //get and set label
    public String getValue() {
	return theLabel.getText();
    }
    
    public void setValue(String text) {
	theLabel.setText(text);
    }

    // add a widget to the container
    public void addComponent(Component widget) {
	// how to get this *BIG* (see gDf)
	widget.setMaximumSize(new Dimension(1600,1600));
	theBox.add(widget);
	theBox.revalidate();
    }
}