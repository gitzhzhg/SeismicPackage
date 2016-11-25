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

//-------------------------- vf_edit_headers.hh -------------------------//
//-------------------------- vf_edit_headers.hh -------------------------//
//-------------------------- vf_edit_headers.hh -------------------------//

//                 header file for the VfEditHeaders class
//                  derived from the VfEditBase class
//                          subdirectory vf


      // This class contains the algorithm for resetting velocity
      // function headers, and the parameters needed for controlling
      // this algorithm.


//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//

#ifndef _VF_EDIT_HEADERS_HH_
#define _VF_EDIT_HEADERS_HH_

#include "vf/vf_edit_base.hh"


class VfEditHeaders : public VfEditBase
{

//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//

private:

  char   *_default_project;
  char   *_default_line;
  char   *_default_rdate;
  char   *_default_pdate;
  char   *_default_userid;
  char   *_default_comment;
  int     _edit_project;         // TRUE/FALSE.
  int     _edit_line;            // TRUE/FALSE.
  int     _edit_rdate;           // TRUE/FALSE.
  int     _edit_pdate;           // TRUE/FALSE.
  int     _edit_userid;          // TRUE/FALSE.
  int     _edit_comment;         // TRUE/FALSE.
  int     _set_comment_to_vfid;  // TRUE/FALSE.


//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//

public:    // constructor and destructor.

           VfEditHeaders ();
  virtual ~VfEditHeaders ();

public:    // get values.

  const char *getDefaultProject   ()  const  { return _default_project; }
  const char *getDefaultLine      ()  const  { return _default_line; }
  const char *getDefaultRdate     ()  const  { return _default_rdate; }
  const char *getDefaultPdate     ()  const  { return _default_pdate; }
  const char *getDefaultUserid    ()  const  { return _default_userid; }
  const char *getDefaultComment   ()  const  { return _default_comment; }
  int         getEditProject      ()  const  { return _edit_project; }
  int         getEditLine         ()  const  { return _edit_line; }
  int         getEditRdate        ()  const  { return _edit_rdate; }
  int         getEditPdate        ()  const  { return _edit_pdate; }
  int         getEditUserid       ()  const  { return _edit_userid; }
  int         getEditComment      ()  const  { return _edit_comment; }
  int         getSetCommentToVfid ()  const  { return _set_comment_to_vfid; }

public:    // set values.

  void   setDefaultProject   (const char *value);
  void   setDefaultLine      (const char *value);
  void   setDefaultRdate     (const char *value);
  void   setDefaultPdate     (const char *value);
  void   setDefaultUserid    (const char *value);
  void   setDefaultComment   (const char *value);
  void   setEditProject      (int value)  { _edit_project = value; }
  void   setEditLine         (int value)  { _edit_line    = value; }
  void   setEditRdate        (int value)  { _edit_rdate   = value; }
  void   setEditPdate        (int value)  { _edit_pdate   = value; }
  void   setEditUserid       (int value)  { _edit_userid  = value; }
  void   setEditComment      (int value)  { _edit_comment = value;
                               if(_edit_comment) _set_comment_to_vfid = 0; }
  void   setSetCommentToVfid (int value)  { _set_comment_to_vfid = value;
                               if(_set_comment_to_vfid) _edit_comment = 0; }

public:   // overriding virtual functions.

  virtual int  virtualCheck   (class VfKernal *kernal, char *msg);
  virtual int  virtualEdit    (class VfKernal *kernal, char *msg);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
