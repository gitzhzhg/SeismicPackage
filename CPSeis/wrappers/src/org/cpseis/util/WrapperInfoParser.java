package org.cpseis.util;

import org.cpseis.util.SimpleTextFileReader;
import org.cpseis.util.StringUtil;

/**
<pre>
             Main program to parse a CPS process source code file.
------------------------------------------------------------------------------
                          WrapperInfoParser.java
------------------------------------------------------------------------------
 This class parses a CPS process source code file to obtain
 the following information:

  (1) whether a oneset function is present.
  (2) whether a twosets function is present.
  (3) the category of the process.
  (4) the purpose of the process.

 Required command line arguments:

  (1) Name of source code file of the CPS process.
  (2) Name of CPS process (lower case without directory or extension).

 This class writes the following three lines to standard-out:
  (1) code
  (2) category
  (3) purpose

 The code is as follows:
  0 if no oneset and no twosets methods.
  1 if oneset but no twosets method.
  2 if twosets but no oneset method.
  3 if both oneset and twosets methods.

 This class simply writes the word ERROR three times to standard-out
 if the name of the CPS process turns out to be a primitive, or any
 parsing error occurs.

 Standard-out should be redirected to a file for access from a script.
------------------------------------------------------------------------------
</pre>
*/

//---------------------------- start of class ----------------------------//
//---------------------------- start of class ----------------------------//
//---------------------------- start of class ----------------------------//

public class WrapperInfoParser
{

//-------------------------------- main -----------------------------------//
//-------------------------------- main -----------------------------------//
//-------------------------------- main -----------------------------------//

public static void main(String[] args)
{
  if(args.length < 2) { System.out.println("need arguments CPS module sourcefile and name\n"); return; }

  String   sourcefile             = args[0];
  String   name                   = args[1];
  String   INTERFACE_NAME         = "interface" + name;
  String   SUBROUTINE_NAME        = "subroutine" + name + "(";
  String   END_SUBROUTINE_NAME    = "endsubroutine" + name;
  String   purpose                = "ERROR";
  String   category               = "ERROR";
  String[] lines                  = SimpleTextFileReader.readFile(sourcefile, null);
  boolean  am_in_brief_doc        = false;
  boolean  valid                  = false;
  boolean  oneset                 = false;
  boolean  twosets                = false;
  boolean  finished_oneset        = false;
  boolean  finished_twosets       = false;
  int      kount                  = 0;
  int      searching_for_optional = 0;

  if(name.equals("select"))   // special treatment for the select module.
      {
      INTERFACE_NAME         = "interface" + name + "junkjunk";
      SUBROUTINE_NAME        = "subroutine" + name + "_private(";
      END_SUBROUTINE_NAME    = "endsubroutine" + name + "_private";
      }

//-------------------------- start looping through file ------------------------//
//-------------------------- start looping through file ------------------------//
//-------------------------- start looping through file ------------------------//

  for(String line : lines)
      {

//--------------------------- examine one line of file -------------------------//
//--------------------------- examine one line of file -------------------------//
//--------------------------- examine one line of file -------------------------//

      kount++;
      String trimmed  = line.trim();          // remove leading and trailing blanks.
      String upper    = trimmed.toUpperCase();
      String squeezed = StringUtil.squeezeBlanks(line).toLowerCase();

//---------------------- check process validity --------------------------//
//---------------------- check process validity --------------------------//
//---------------------- check process validity --------------------------//

      if(!valid)
          {
          if(kount > 3)
              {
              break;
              }
          else if(squeezed.equals("!<cps_v1type=\"process\"/>"))
              {
              valid = true;
              continue;
              }
          }

//------------------- get purpose and category -----------------------//
//------------------- get purpose and category -----------------------//
//------------------- get purpose and category -----------------------//

      if(line.contains("<brief_doc>"))  { am_in_brief_doc = true;  continue; }
      if(line.contains("</brief_doc>")) { am_in_brief_doc = false; continue; }

      if(am_in_brief_doc)
          {
          if(line.contains("Purpose"))
              {
              int index = line.indexOf(":");
              purpose = line.substring(index+1).trim();
              purpose = StringUtil.addBackslash(purpose);
              }
          if(line.contains("Category"))
              {
              int index = line.indexOf(":");
              category = line.substring(index+1).trim().toLowerCase();
              }
          continue;
          }

//------------------ decide whether to break or continue --------------------//
//------------------ decide whether to break or continue --------------------//
//------------------ decide whether to break or continue --------------------//

      if(finished_oneset && finished_twosets) break;
      if(squeezed.length() == 0)   continue;  // skip blank line.
      if(squeezed.startsWith("!")) continue;  // skip comment line.

//-------------------------- get oneset and twosets --------------------------//
//-------------------------- get oneset and twosets --------------------------//
//-------------------------- get oneset and twosets --------------------------//

      if (squeezed.equals(INTERFACE_NAME))
           {
           oneset = true;
           twosets = true;
           finished_oneset = true;
           finished_twosets = true;
           break;
           }

      if (squeezed.startsWith(SUBROUTINE_NAME))
           {
           int ncommas = 0;
           for(int indx = 0; indx < squeezed.length(); indx++)
                {
                if (squeezed.charAt(indx) == ',') ncommas += 1;
                }
           if (ncommas == 3)
                {
                oneset = true;
                finished_oneset = true;
                continue;
                }
           else if (ncommas == 5)
                {
                twosets = true;
                finished_twosets = true;
                if(finished_oneset) break;
                searching_for_optional = 1;
                }
           }

      //////////// oneset might be true even if twosets is true
      //////////// if the last two arguments are optional:

      if (searching_for_optional == 0) continue;

      searching_for_optional += 1;

      if (searching_for_optional > 50)
           {
           searching_for_optional = 0;
           continue;
           }

      if (squeezed.startsWith(END_SUBROUTINE_NAME))
           {
           searching_for_optional = 0;
           continue;
           }

      if (squeezed.startsWith("real,") == false) continue;

      int indx = squeezed.indexOf(",optional");
      if (indx >= 0)
           {
           int indx2 = squeezed.indexOf('!');
           if (indx2 < 0 || indx2 > indx)
                {
                oneset = true;
                finished_oneset = true;
                searching_for_optional = 0;
                continue;
                }
           }

//------------------------ finish looping through file --------------------//
//------------------------ finish looping through file --------------------//
//------------------------ finish looping through file --------------------//

      }

//------------------------- end of main method -----------------------------//
//------------------------- end of main method -----------------------------//
//------------------------- end of main method -----------------------------//

  int code = 0;
  if(oneset) code++;
  if(twosets) code += 2;

  if(category.equals("ERROR") || purpose.equals("ERROR"))
      {
      System.out.println("ERROR");
      }
  else
      {
      System.out.println("" + code);
      }
  System.out.println(category);
  System.out.println(purpose);
}

//--------------------------- end of class ----------------------------------//
//--------------------------- end of class ----------------------------------//
//--------------------------- end of class ----------------------------------//

}

//------------------------------- end --------------------------------------//
//------------------------------- end --------------------------------------//
//------------------------------- end --------------------------------------//
