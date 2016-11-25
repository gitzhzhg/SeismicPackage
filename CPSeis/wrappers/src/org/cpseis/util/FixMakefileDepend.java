package org.cpseis.util;


/**
<pre>
           Fix makefile.depend by adding $(OBJ_DIR) to all .o files.
------------------------------------------------------------------------------
                           FixMakefileDepend.java
------------------------------------------------------------------------------
 This class is used as a main program.

 Standard-in contains the old makefile.depend.
 Standard-out contains the new makefile.depend.
------------------------------------------------------------------------------
                          RECENT REVISION HISTORY              

     Date        Author       Description
     ----        ------       -----------
  2. 2006-04-11  Stoeckley    Add OBJ_DIR to all .o files (not just those
                               on the left), and make more robust.
  1. 2005-04-26  Stoeckley    Initial version.
------------------------------------------------------------------------------
</pre>
*/


//---------------------------- start of class ----------------------------//
//---------------------------- start of class ----------------------------//
//---------------------------- start of class ----------------------------//


public class FixMakefileDepend
{

//----------------------------- main ---------------------------------------//
//----------------------------- main ---------------------------------------//
//----------------------------- main ---------------------------------------//


// double space before main below to keep it from running with gmake run:

public static void  main(String[] args)
{
  SimpleTextFileReader reader = new SimpleTextFileReader(null);

  while(true)
      {
      String card = reader.readLine();
      if(card == null) break;

      int index = card.indexOf(".o");
      if(index >= 0)
          {
          index = card.substring(0,index).lastIndexOf(" ");
          if(index < 0)
              {
              System.out.println("$(OBJ_DIR)/" + card);
              }
          else
              {
              card = card.substring(0,index+1) + "$(OBJ_DIR)/" + card.substring(index+1);
              System.out.println(card);
              }
          }
      else
          {
          System.out.println(card);
          }
      }
}


//--------------------------- end of class --------------------------------//
//--------------------------- end of class --------------------------------//
//--------------------------- end of class --------------------------------//
                                                                                                                                 
}
                                                                                                                                 
//------------------------------- end ------------------------------------//
//------------------------------- end ------------------------------------//
//------------------------------- end ------------------------------------//

