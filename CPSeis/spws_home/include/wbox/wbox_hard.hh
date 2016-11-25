/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/

//----------------------- wbox_hard.hh -------------------------//
//----------------------- wbox_hard.hh -------------------------//
//----------------------- wbox_hard.hh -------------------------//

//              header file for the WboxHard class
//                  not derived from any class
//                       subdirectory wbox

             // This class creates a hardcopy file
             // containing the contents of the table.

    // There are four methods to save a hardcopy file:
    //  (1) call saveTable(filename).
    //  (2) call saveTable(NULL).
    //  (3) press the HARDCOPY endkey within the table.
    //  (4) reset endkey to "HARDCOPY" within a trap.

    // Method (1) saves the file right away.
    // Method (2) does either (A) or (B) below.
    // Method (3) does either (A) or (B) below.
    // Method (4) does either (A) or (B) below.

    // (A) If a WboxHardcopy function is NOT registered:
    //   This class will make up a filename and save the file right away.
    //
    // (B) If a WboxHardcopy function IS registered:
    //   This class will make up a default filename, calculate approximately
    //   how many lines the file will contain, and then will call the
    //   WboxHardcopy function.  The WboxHardcopy function should
    //   then do one of the following:
    //     (a) return a pointer to a filename to save right away, or
    //     (b) return NULL if a file should not be saved, or
    //     (c) return NULL if a file might be saved later by calling
    //           saveTable(filename) after checking with the user via
    //           a dialog box.


#ifndef _WBOX_HARD_HH_
#define _WBOX_HARD_HH_

#include <stdio.h>


class WboxHard
{

//--------------------------- typedefs ----------------------------//
//--------------------------- typedefs ----------------------------//
//--------------------------- typedefs ----------------------------//

  typedef const char *WboxHardcopy
                 (void *data, int numlines, const char *default_filename);

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class WboxBox     *_box;
  WboxHardcopy      *_hardcopy;

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

           WboxHard (WboxBox *box);
  virtual ~WboxHard ();

  void  registerHardcopy (WboxHardcopy *hardcopy)  { _hardcopy = hardcopy; }

public:  // to be called by the windowbox routines when the user presses
         // the HARDCOPY key, or a trap resets endkey to "HARDCOPY".

  void  maybeSaveTable (const char *endkey)   const;

public:  // to be called at any time by the user.

  void  saveTable  (const char *filename = NULL) const;

private:

  const char *privateGetFilename ()                      const;
  int         privateGetNumlines ()                      const;
  void        privateSaveTable   (const char *filename)  const;
  void        privateSaveImage   (FILE *stream)          const;
  void        privateSaveArrays  (FILE *stream)          const;

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//


