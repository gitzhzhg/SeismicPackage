package com.conoco.cfe.server.messaging;

import com.conoco.xml.StringArray;

import java.util.StringTokenizer;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class XmlTestCfeApi 
  implements CfeApi {

  protected Element _responseTree;
  protected MessageBuilder _messageBuilder;

  public XmlTestCfeApi() {
  }

  public void setResponseTree(Element responseTree) {
    _responseTree = responseTree;
  }

  public MessageBuilder getMessageBuilder() {
    return _messageBuilder;
  }
  
  public void setMessageBuilder(MessageBuilder messageBuilder) {
    _messageBuilder = messageBuilder;
  }

  public void putResponse(String actionName, Element action, int processId, String keyword, String value) {
    System.out.println("----putResponse(): entering ----");
    Node node = action.getFirstChild();

    int childProcessId = processId;
    String childProcessIdString;
    String childAction;
    String childKeyword;
    String childValue;
    
    String childElements;
    int childStartElement = -1;
    int childEndElement = -1;
    String childStartElementString;
    String childEndElementString;

    while (node != null) {
      if (node.getNodeType() == Node.ELEMENT_NODE) {
        Element child = (Element)node;
        childProcessIdString = child.getAttribute("processId");
        if ((childProcessIdString != null) && 
          (childProcessIdString.length() > 0)) {
          childProcessId = Integer.parseInt(childProcessIdString);
        }

        childKeyword = child.getAttribute("keyword");
        if (childKeyword.length() == 0) {
          childKeyword = null;
        }
        childValue = child.getAttribute("value");
        if (childValue.length() == 0) {
          childValue = null;
        }
        childElements = child.getAttribute("elements");
        if (childElements.length() == 0) {
          childElements = null;
        }
        
        childStartElementString = child.getAttribute("startElement");
        if ((childStartElementString != null) &&
          (childStartElementString.length() > 0)) {
          childStartElement = Integer.parseInt(childStartElementString);
        } else {
          childStartElement = 0;
        }
        
        childEndElementString = child.getAttribute("endElement");
        if ((childEndElementString != null) &&
          (childEndElementString.length() > 0)) {
          childEndElement = Integer.parseInt(childEndElementString);
        } else {
          childEndElement = 0;
        }
        
        childAction = child.getNodeName();
        if (actionName.equals(childAction)) {
          if (childProcessIdString == null) {
            childProcessId = processId;
          }
          
          if (childKeyword == null) {
            childKeyword = keyword;
          }
          
          if (childValue == null) {
            childValue = value;
          }
        }

        if (childValue != null) {
          _messageBuilder.putValue(childProcessId, childAction, childKeyword, childValue);
        } else if ((childElements != null) ||
          (childStartElementString != null) ||
          (childEndElementString != null)) {
          _messageBuilder.putArray(childProcessId, childAction, childKeyword,
                       StringArray.parseStringArray(childElements),
                       childStartElement, childEndElement);
        } else {
          _messageBuilder.putValue(childProcessId, childAction, childKeyword, childValue);
        }
      }

      node = node.getNextSibling();
      System.out.println("----getting next sibling----");
    }
    System.out.println("----putResponse(): exiting ----");
  }

  public void putResponse(String actionName, Element action, int processId, 
              String keyword, String[] elements,
              int startElement, int endElement) {
    // TODO: (DSK) - implement this!!!
    System.out.println("----putResponse(): call is not yet implemented ----");
  }

  public boolean performAction(String actionName, Element action,
                 int processId, String keyword, String value) {
    boolean performAction;
    String myProcessId;
    String myKeyword;
    String myValue;

    myProcessId = action.getAttribute("processId");
    myKeyword = action.getAttribute("keyword");
    myValue = action.getAttribute("value");

    performAction = true;
    if ((myKeyword != null) &&
      (myKeyword.length() != 0) &&
      (!myKeyword.equals(keyword))) {
      performAction = false;
    } else if ((myValue != null) &&
           (myKeyword.length() != 0) &&
           (!myValue.equals(value))) {
      performAction = false;
    } else if ((myProcessId != null) &&
           (myProcessId.length() != 0)) {
      StringTokenizer processIdTokenizer = new StringTokenizer(myProcessId);
      performAction = false;
      while (processIdTokenizer.hasMoreTokens()&&
           !performAction) {
        if (processId == Integer.parseInt(processIdTokenizer.nextToken())) { 
          performAction = true;
         }
      }
    }

    if (performAction) {
      System.out.println("myProcessId="+myProcessId);
      putResponse(actionName, action, processId, keyword, value);
    }
    return performAction;
  }

  public boolean performAction(String actionName, Element action, int processId, 
                 String keyword, String[] elements, int startElement, int endElement) {
    boolean performAction;
    String myProcessId;
    String myKeyword;
    String myElements;
    String myStartElement;
    String myEndElement;

    myProcessId = action.getAttribute("processId");
    myKeyword = action.getAttribute("keyword");
    myElements = action.getAttribute("elements");
    myStartElement = action.getAttribute("startElement");
    myEndElement = action.getAttribute("endElement");

    performAction = true;
    if ((myProcessId != null) &&
      (Integer.parseInt(myProcessId) != processId)) {
      performAction = false;
     } else if ((myKeyword != null) &&
      (!myKeyword.equals(keyword))) {
      performAction = false;
    } else if ((myStartElement != null) &&
           (Integer.parseInt(myStartElement) != startElement)) {
      performAction = false;
    } else if ((myEndElement != null) &&
           (Integer.parseInt(myEndElement) != endElement)) {
      performAction = false;
    } else {
      putResponse(actionName, action, processId, keyword, elements, startElement, endElement);
    }

    return performAction;
  }

  public void setValue(int processId, String action, String keyword, String value)
    throws MessagingException {

    System.out.println("** calling setValue()");
    if (_responseTree == null) {
      throw new MessagingException("Must call 'setResponseTree()' before using XmlTestCfeApi.");
    }
    NodeList actionList = _responseTree.getElementsByTagName("Request" + action);

    boolean actionPerformed = false;
    Element myAction;

    System.out.println("** actionList length = " + actionList.getLength());
    System.out.println("   action = [ processId=" + processId +
               ", keyword=" + keyword + 
               ", value=" + value);
    for (int i = 0; i < actionList.getLength(); i++) {
      myAction = (Element)(actionList.item(i));

      System.out.println("   ..myAction = [ processId=" + myAction.getAttribute("processId") + 
                 ", keyword=" + myAction.getAttribute("keyword") + 
                 ", value=" + myAction.getAttribute("value") + " ]");
      if (performAction(action, myAction, processId, keyword, value)) {
        actionPerformed = true;
      }
      System.out.println("   !action performed!");
    }

    if (!actionPerformed) {
      System.out.println("  Could not find response for action " + action);
      throw new MessagingException("Could not find response for action '" + action + "'.");
    }
    System.out.println("** end action list **");
  }

  public void setArray(int processId, String action, String keyword, String[] elements, int start, int end)
    throws MessagingException {

    System.out.println("** calling setValue()");
    NodeList actionList = _responseTree.getElementsByTagName("Request" + action);

    boolean actionPerformed = false;
    Element myAction;

    System.out.println("** actionList length = " + actionList.getLength());
    for (int i = 0; i < actionList.getLength(); i++) {
      myAction = (Element)(actionList.item(i));

      System.out.println("   myAction = [ value=" +
                 myAction.getAttribute("processId") + ", keyword=" +
                 myAction.getAttribute("keyword") + ", value=" +
                 myAction.getAttribute("value") + " ]" + "  processId = "+processId);
      if (performAction(action, myAction, processId, keyword, elements, start, end)) {
        actionPerformed = true;
      }
      System.out.println("   !action performed!");
    }

    if (!actionPerformed) {
      System.out.println("Could not find response for action " + action);
      throw new MessagingException("Could not find response for action '" + action + "'.");
    }
    System.out.println("** end action list **");
  }
}