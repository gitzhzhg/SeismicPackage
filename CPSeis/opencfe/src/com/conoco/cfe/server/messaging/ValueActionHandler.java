///
/// ValueActionHandler.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  7. 12-01-2003 SMCook   Turned off KillAppThread.  It worked in test
///                          situations, but is not working properly in the
///                          case of real hangups it was designed to address.
///                         Hopefully filebox_crou.c changes will fix the
///                          problem instead.
///  6. 11-07-2003 SMCook   Excluded WRITEWORKFILES in addition to SJSUBMIT.
///  5. 11-03-2003 SMCook   Changed showDialog flag to false and changed
///                          KILL_MINUTES to 10.
///  4. 10-17-2003 SMCook   Added logic to not use killer in certain
///                          circumstances where slow response is expected
///                          (so far just when keyword is SJSUBMIT).
///  3. 10-08-2003 SMCook   Added KillAppThread logic to deal with dead disks.
///

package com.conoco.cfe.server.messaging;

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

import com.conoco.shared.KillAppThread;

import com.conoco.cfe.utils.URLDecoder;

/**
 * An action handler for request messages related to 
 * fields.
 */
public class ValueActionHandler 
  implements XmlActionHandler {

  /**
   * Variables to allow thread to kill off the application in the event of
   * a lockup (such as a bad disk).
   */
   private KillAppThread killer;
   private final float WARN_MINUTES =   .33f;
   private final float KILL_MINUTES = 10.00f;

  /**
   * Returns the keyword attribute name.
   * 
   * @return the keyword attribute name
   */
  public String getKeywordAttributeName() {
    return KEYWORD;
  }
 
  /**
   * Returns a boolean variable that indicates whether 
   * a keyword attribute is required or not.
   *
   * @return the boolean value for the requirement of a keyword attribute
   */
  public boolean isKeywordRequired() {
    return true;
  }

  /**
   * Returns the value attribute name.
   * 
   * @return the value attribute name
   */
  public String getValueAttributeName() {
    return VALUE;
  }
 
  /**
   * Returns a boolean variable that indicates whether a value attribute
   * is required or not.
   * 
   * @return the boolean that indicates the requirement of a value attribute
   */
  public boolean isValueRequired() {
    return true;
  }

  /**
   * This method is invoked by the message parser.
   * 
   * @param node the DOM document node that is to be processed
   * @param cfeApi the wrapper class that provides access to the CFE API   
   * @throws com.conoco.cfe.server.messaging.MessagingException if the keyword
   * attribute is not found                         
   */
  public void performAction(Node node, CfeApi cfeApi) 
    throws MessagingException {

    boolean useKiller = false;

    if (node != null) {
      NamedNodeMap attributes = node.getAttributes();

      int windowId;
      String keyword = null;
      String value = null;

      Node windowIdNode = attributes.getNamedItem(WINDOW_ID);

      if (windowIdNode == null) {
        windowId = -1;
      } else {
        windowId = Integer.parseInt(windowIdNode.getNodeValue());
      }

      if (isKeywordRequired()) {
        Node keywordNode = attributes.getNamedItem(getKeywordAttributeName());
        if (keywordNode == null) {
          throw (new MessagingException("Could not find '" +
                          getKeywordAttributeName() + "' attribute"));
        } else {
          keyword = keywordNode.getNodeValue();

          // some circumstances where timer is NOT desireable.
          if(keyword.equals("SJSUBMIT") ||
             keyword.equals("WRITEWORKFILES")) {
            //System.out.println("kill: not using killer");
            useKiller = false;
          }
        }
      }

      if (isValueRequired()) {
        Node valueNode = attributes.getNamedItem(getValueAttributeName());
        if (valueNode == null) {
          throw (new MessagingException("Could not find '" +
                          getValueAttributeName() + "' attribute"));
        } else {
          try {
            value = URLDecoder.decode(valueNode.getNodeValue());
          } catch ( Exception en ) {
            value = null;  
          }
        }
      }

      //System.out.println("############## " + node.getNodeName());

      if(useKiller) {
        killer = new KillAppThread(WARN_MINUTES, KILL_MINUTES, false);
        killer.start();
      }

      //
      // make this thread sleep to test/debug the killer thread
      //
      if(false) {
        try {
          Thread.sleep(60000);
          //Thread.sleep(600000);
        }
        catch(InterruptedException e) {
        }
      }

      try {
        cfeApi.setValue(windowId, node.getNodeName(), keyword, value);
      } catch (MessagingException me) {
        System.out.println("Messaging exception: " + me);
        //me.printStackTrace();
      }

      if(useKiller) {
        killer.cancel();
      }
    }
  }
}
