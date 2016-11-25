package org.cpseis.util;

/**
<pre>
     Main program for generating Fortran-90 dependencies for a makefile.
------------------------------------------------------------------------------
                          FortranDependencies.java
------------------------------------------------------------------------------
 This program reads a list of Fortran-90 source files from standard in and
 writes the dependencies to standard out.  It scans each file looking for
 information on USE and MODULE statements.  It works for upper and lower and
 mixed case code.

 In order for this program to work correctly, the name of a module on a
 USE statement must be related to the name of the source file where the
 module resides as follows:

       USE XXXX_MODULE     (upper or lower or mixed case)
       xxxx.f90            (lower case)

 There are two hard-coded exceptions to the above rule:

       USE SWAP_MODULE    (should have been SWAP_FROU_MODULE)
       swap_frou.f90

       USE LGC_SWAP_MODULE    (should have been LGC_SWAP_FROU_MODULE)
       lgc_swap_frou.f90
------------------------------------------------------------------------------
                            ADVICE FOR USERS

 Limitations:

 This program should work correctly if a USE statement contains qualifiers
 such as ONLY after the module name, but it will not pick up more than
 one module name on the same USE statement.  There may be occasional
 additional misbehavior which should be easy to fix in the Fortran-90
 source code, such as using the word MODULE as the name of a variable.
------------------------------------------------------------------------------
                         RECENT REVISION HISTORY

     Date        Author     Description
     ----        ------     -----------
 11. 2008-12-03  Stoeckley  Allow semicolons after "use" statements, and remove
                             excess consecutive copies of the same dependency.
 10. 2006-10-16  Stoeckley  Add special handling for lgc_swap.
                             lgc_swap_module should be lgc_swap_frou_module.
  9. 2006-04-11  Stoeckley  Converted to java from sps_fortran_dependencies.f90.
  8. 2003-08-11  Goodger    Add special handling for swap.  swap_module should
                             be swap_frou_module.
  7. 2003-08-08  Stoeckley  Increase array sizes.
------------------------------------------------------------------------------
</pre>
*/


//----------------------------- start of class ------------------------------//
//----------------------------- start of class ------------------------------//
//----------------------------- start of class ------------------------------//


public class FortranDependencies
{


//--------------------------- write dependencies -------------------------//
//--------------------------- write dependencies -------------------------//
//--------------------------- write dependencies -------------------------//


private static void writeDependencies
                        (String filename, String root, int num, String hello[])
{
//---------- get list of dependencies:

  String[] module_names = new String[2000];
  String[] deps         = new String[2000];

  int kount = 0;
  int ndeps = 0;

  SimpleTextFileReader reader = new SimpleTextFileReader(filename);

  if(reader.getStatus() == SimpleTextFileReader.ERROR)
      {
      System.err.println(reader.getMessage());
      return;
      }

  while(true)
      {
      String line = reader.readLine();
      if(line == null) break;
      line = line.trim().toLowerCase();

      if(line.startsWith("module "))
          {
          String module_name = line.substring(6).trim();
          if(module_name.startsWith("procedure")) continue;
          if(module_name.startsWith("swap_module"))
                                             module_name = "swap_frou_module";
          if(module_name.startsWith("lgc_swap_module"))
                                             module_name = "lgc_swap_frou_module";
          int indx = module_name.indexOf("!");
          if(indx >= 0) module_name = module_name.substring(0, indx).trim();
                                              // remove "!xxxx" from name.
          module_names[kount] = module_name;
          kount++;
          }
      else if(line.startsWith("use "))
          {
          String module_name = line.substring(3).trim();
          if(module_name.startsWith("swap_module"))
                                             module_name = "swap_frou_module";
          if(module_name.startsWith("lgc_swap_module"))
                                             module_name = "lgc_swap_frou_module";
          int indx = module_name.indexOf(",");
          if(indx >= 0) module_name = module_name.substring(0, indx).trim();
                                              // remove ",xxxx" from name.
          indx = module_name.indexOf(";");
          if(indx >= 0) module_name = module_name.substring(0, indx).trim();
                                              // remove ";xxxx" from name.
          indx = module_name.indexOf("!");
          if(indx >= 0) module_name = module_name.substring(0, indx).trim();
                                              // remove "!xxxx" from name.
          module_name = module_name.trim();
          boolean continue_outer_loop = false;
          for(int i = 0; i < kount; i++)
              {
              if(module_name.equals(module_names[i]))
                  {
                  continue_outer_loop = true;
                  break;
                  }
              }
          if(continue_outer_loop) continue;
          if(module_name.endsWith("_module"))
              {
              int length = module_name.length();
              module_name = module_name.substring(0,length-7);
                                              // remove "_module" from name.
              }
          boolean found = false;
          for(int i = 0; i < num; i++)
              {
              if(module_name.equals(hello[i]))
                  {
                  found = true;
                  break;
                  }
              }
          if(found)
              {
               String newdep = module_name.trim() + ".o";
               if(ndeps == 0 || !newdep.equals(deps[ndeps-1]))
                   {
                   deps[ndeps] = newdep;
                   ndeps++;
                   }
              }
          }
      }

  reader.close();

//---------- write dependencies to file:

/******
  String line = "$(OBJ_DIR)/" + root.trim() + ".o:   \t\t\t\t\t\\";
******/
  String line = root.trim() + ".o:   \t\t\t\t\t\\";
  System.out.println(line);
  line = "                         " + root.trim() + ".f90";
  if(ndeps > 0) line += "  \t\t\t\\";
  System.out.println(line);
  for(int i = 0; i < ndeps; i++)
      {
      line = "                         " + deps[i];
      if(i < ndeps-1) line += "  \t\t\t\\";
      System.out.println(line);
      }
  System.out.println("");
}


//------------------------------ main ---------------------------------------//
//------------------------------ main ---------------------------------------//
//------------------------------ main ---------------------------------------//


// double space before main below to keep it from running with gmake run:

public static void  main(String[] args)
{
  SimpleTextFileReader reader = new SimpleTextFileReader(null);  // standard in.

  String full [] = new String[2000];
  String hello[] = new String[2000];

//---------- get list of potential dependencies:

  int num = 0;
  while(true)
      {
      String line = reader.readLine();
      if(line == null) break;
      int indx = line.lastIndexOf(".");
      if(indx < 0) continue;
      String root = line.substring(0,indx);
      String ext  = line.substring(indx);
      if(!ext.equals(".f90")) continue;
      indx = root.lastIndexOf("/");
      if(indx >= 0) root = root.substring(indx+1);
      int length = root.length();
      full [num] = line;
      hello[num] = root;
      num++;
      }

//---------- write fortran object file dependencies:

  for(int i = 0; i < num; i++)
      {
      String root = hello[i];
      writeDependencies(full[i],root,num,hello);
      }
}


//----------------------------- end of class ------------------------------//
//----------------------------- end of class ------------------------------//
//----------------------------- end of class ------------------------------//

}

//--------------------------------- end ----------------------------------//
//--------------------------------- end ----------------------------------//
//--------------------------------- end ----------------------------------//

