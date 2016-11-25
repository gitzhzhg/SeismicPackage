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
#ifndef CUBE_SELECT_HH
#define CUBE_SELECT_HH

#include "wproc.h"
#include "file_choice.h"
#include "sl/sl_form_pop.hh"
#include "cube/cube_inform.hh"
#include "sl/sl_push_box.hh"

class CubeDisplay;
class CubeSectionGui;
class CubeSelectPush;
class CubeAnnotationGui;
class CubeAmplitudeGui;
class SLTextBox;
class SLpOption;
class SLpFile;
class NetEnv;
class DoAbortWithStatus;

class CubeSelect : public SLFPopSep, public CubeInform {

friend class CubeSelectPush;

public:
  enum {
    NONE,                                       // cube file is missing
    GOOD,                                       // cube file is good
    BAD,                                        // cube file is invalid
    NOTSET,                                     // cube has not been set
    REPEATED                                    // cube file is not unique
  };

  CubeSelect					// constructor
    (Widget p,					//   parent widget
     char *name,				//   name of popup
     HelpCtx hctx,				//   context sensitive help
     CubeDisplay *cube_display,			//   current cube to select fr
     CubeSectionGui *cube_section,		//   cube section gui
     CubeAnnotationGui *cube_annotation, 	//   cube annotation gui
     CubeAmplitudeGui *cube_amplitude);         //   cube amplitude gui

  ~CubeSelect();				// destructor

  virtual Widget make				// make cube selection popup
    (Widget p);					//   parent widget

  virtual void manage();			// manage cube select widget

  virtual void cubeIsNolongerCurrent		// called when current cube chgs
    (Cube *cube,				//   previous current cube
     Cube *newcube);				//   new current cube

  void destroyed				// called when cube destroyed
    (Cube *cube);				//   cube being destroyed

  void change ();				// change to current cube

  void set					// set labels based on
    (Cube *cube = 0);				//   given cube

  enum {
    LOCAL,					// local network connection
    CRAY_SP					// Cray-sp network connection
  };
  
  Boolean netConnectOk ();                      // is remote connection valid

  void getDataFrom                              // data from remote or local
    (int where);

  void deleteFile ();

  void changingFile(Boolean changing){_changing_file = changing;}

protected:
  void filein					// called after file is input
    (long ident,				//   identifier
     char *oldvar,				//   previous char string
     char *newvar);				//   new char string

  virtual void UndoInput () {}			// undo input override

  virtual Boolean ValidInput ();		// check validity of input

  virtual Boolean ValidSize			// check validity of size
    (Cube *cube);				//   given cube

  void setParams ();				// set cube coord header wrds

  void setNetEnvPtr                             // setup the AltFileAccess
    (NetEnv **netenv);

  Boolean netConnectGood ();                    // still connected to remote

  void cancelButton();

  Boolean deletingCube ();                      // is cube being deleted

  virtual void DoAction ();			// do when OK or APPLY pushed

  Boolean unique				// check uniqueness of filename
    (char *filename);				//   given filename to check
 
  int cubeTrcioIsOk				// check for aux Z-slice file
    (Cube *cube);				//   given cube

  CubeDisplay
    *_cube_display;				// cube display list

  CubeSectionGui
    *_cube_section;				// cube section GUI obj

  CubeAnnotationGui
    *_cube_annotation;				// cube annotation GUI obj

  CubeAmplitudeGui                              // cube amplitude gui obj
  *_cube_amplitude;

  DoAbortWithStatus
    *_do_abort_w_status;			// user abort and status obj

  SLpFile
    *_infile;					// input file

  Boolean
    _first_time,				// true before selected
    _changing_file,                             // adding or changing cube
    _deleting_file;                             // deleting last cube

  Widget
    _hstot,					// total horizontal slices
    _lntot,					// total lines
    _xltot,					// total crosslines
    _lav;					// largest absolute val

  char
    _cube_file[300],				// name of input cube file
    _fail_str[360];                             // failure string

  int
    _lnhw,					// line coordinate header word
    _xlhw,					// crossline coord header word
    _cube_file_state;				// state of cube file

private:
  Cube
    *_added_cube,
    *_previous_cube;

  static void fileCallback			// file selection call back
    (void *data,				//   object data pointer
     long ident,				//   identifier
     char *oldvar,				//   previous char string
     char *newvar);				//   new char string

  CubeSelectPush
    *_but;					// push buttons

  SLpOption
    *_host_ops;					// where to get data from

  SLTextBox
    *_coord_hwds;				// coord hdr word text box

  AltFileAccess                                 // struct for file choice to 
     _alt_access;                               // use the NetEnv class

  NetEnv                                        // class that allows remote
    *_netenv;                                   // access

};


class CubeSelectPush : public SLPushBox {

public:
  CubeSelectPush				// constructor
    (SLDelay *contain,				//   container object
     char *name,				//   name of push button box
     HelpCtx hctx,				//   context sensitive help
     SLPushAry pushary,				//   array of push buttons
     long arycnt,				//   # of push buttons
     CubeSelect *cube_select,			//   given cube select obj
     Boolean dotitle=False) :			//   title flag
    SLPushBox (contain, name, hctx, pushary, (unsigned int)arycnt, dotitle),
    _cube_select  (cube_select)  {}

  virtual void pushAction			// called on push action
    (long ident);				//   push button identifier

private:
  CubeSelect
    *_cube_select;				// given cube select obj
};

#endif
