///
/// CfeXmlMessageParser.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  3. 04-06-2004 SMCook   Now registers new MiscellaneousParameter
///                          ValueActionHandler.  Added to get icps buttons
///                          up and running for the custom code case.
///

package com.conoco.cfe.server.messaging;

/**
 * Message parser to parse the client request messages.
 */
public class CfeXmlMessageParser
  extends XmlMessageParser {

  /**
   * Inner class for implementing an action handler 
   * that handles request messages that do not require a value
   * attribute.
   */
  public class NoValueActionHandler
    extends ValueActionHandler {
    
    /** Return false for the requirement of value attribute node */
    public boolean isValueRequired() {
      return false;
    }
    
  }
  
  /**
   * Inner class for implementing an action handler 
   * that handles request messages that do not require a value
   * attribute as well as a keyword attribute.
   */
  public class NoKeywordNoValueActionHandler
    extends ValueActionHandler {
    
    public boolean isValueRequired() {
      return false;
    }
    
    public boolean isKeywordRequired() {
      return false;
    }
  }
  
  /**
   * Inner class for implementing an action handler 
   * that handles request messages that do not require a keyword
   * attribute but require a value attribute.
   */
  public class NoKeywordActionHandler
    extends ValueActionHandler {
        
    public boolean isKeywordRequired() {
      return false;
    }
  }
  
  /**
   * Constructs a new message parser.
   */
  public CfeXmlMessageParser() {
    super();
    registerXmlAction("ArrayNames",    new ArrayActionHandler());
    registerXmlAction("BackEndLibPath",  new NoKeywordActionHandler());  
    registerXmlAction("ButtonPress",  new NoValueActionHandler());
    registerXmlAction("CloseWindow",  new NoKeywordNoValueActionHandler());  
    registerXmlAction("DoubleClicked",  new ValueActionHandler());
    registerXmlAction("EnterScreen",  new NoValueActionHandler());
    registerXmlAction("EnterWindow",  new NoValueActionHandler());
    registerXmlAction("InitializeApp",  new NoValueActionHandler());
    registerXmlAction("InsertElement",  new ValueActionHandler());    
    registerXmlAction("ItemSelected",  new ArrayActionHandler());
    registerXmlAction("ItemClicked",  new ValueActionHandler());
    registerXmlAction("KeyPress",      new ValueActionHandler());
    registerXmlAction("LeaveArray",    new NoValueActionHandler());
    registerXmlAction("LeaveArraySet",  new NoValueActionHandler());
    registerXmlAction("LeaveScreen",  new NoValueActionHandler());
    registerXmlAction("MiscellaneousParameter",  new ValueActionHandler());  
    registerXmlAction("ModifyArrayElement",   new ModifyArrayElementActionHandler());    
    registerXmlAction("ModifyField",     new ValueActionHandler());
    registerXmlAction("PasteElements",  new ArrayActionHandler());  
    registerXmlAction("RefreshApp",   new NoKeywordNoValueActionHandler());
    registerXmlAction("RefreshProcess", new NoKeywordNoValueActionHandler());
    registerXmlAction("Reset",       new NoKeywordNoValueActionHandler());
    registerXmlAction("RemoveElement",  new ValueActionHandler());
    registerXmlAction("TerminateApp",  new NoKeywordNoValueActionHandler());
  }
}
