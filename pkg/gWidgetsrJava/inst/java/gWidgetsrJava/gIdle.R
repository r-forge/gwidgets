/* add idlehandler */

package gWidgetsrJava;

import javax.swing.Timer;

class gIdle implements ActionListener {

  public Timer timer;
  public int ID;
  
  //constructor
  public gIdle(float interval, int id) {

    ID = id;
    timer = new Timer(interval, this);
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
        org.rosuda.JRI.Rengine.getMainEngine().eval("runHandlerFor(\""+sid+"\",type=\"addActionListener\")");
    } catch (Exception ex) {
      System.out.println("Couldn't dispatch event, error: "+ex);
    }
  }
}
