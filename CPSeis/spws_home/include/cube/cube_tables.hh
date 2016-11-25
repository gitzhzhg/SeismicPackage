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
#ifndef CUBE_TABLES_HH
#define CUBE_TABLES_HH

#include "sl/sl_databox.hh"
#include "cube/cube_inform.hh"

#define TABLE_SIZE      30
#define FILENAME_LENGTH 30

class Cube;
class CubeDisplay;
class SLErrorPop;

class CubeTable : public SLDatabox, public CubeInform {

public:
  static void selectionDo			// call back when selectn made
    (void *box,					//   window box pointer
     long *ident,				//   which row selectns column
     long *index,				//   which index
     char *text,				//   text field
     long *nread,				//   number read?
     char *endkey);				//   end key?

  virtual void cubeIsCurrent			// called by informer when
    (Cube * /* cube */)				//   given cube is made current
    { setCurrentCube (); }

  virtual void newCubeCreated			// called by informer when
    (Cube *cube)				//   given cube is created
    { insertNewCube (cube); }

  virtual void destroyed			// called by informer when
    (Cube *cube)				//   given cube is destroyed
    { removeOldCube (cube); }

  virtual void newFilename			// called by informer when
    (Cube *cube)				//   given cube's fname changs
    { changeCubeName (cube); }

protected:
  CubeTable					// constructor
    (SLDelay *slparent,				//   SL delay parent
     char *name);				//   name of table

  CubeTable					// constructor
    (Widget parent,				//   Gui widget parent
     char *name);				//   name of table

  virtual ~CubeTable ();			// destructor

  virtual void setCurrentCube () {}		// set current cube

  virtual void insertNewCube			// insert just added
    (Cube *cube) = 0;				//   given cube

  virtual void removeOldCube			// remove just deleted
    (Cube *cube) = 0;				//   given cube

  virtual void makeHelper ();			// helps make function

  virtual long tableButton () = 0;		// sets button type on table

  virtual void tableMessage () = 0;		// writes a table message

  virtual long selectionState			// set selection flag
    (Cube *cube,				//   given Cube and
     CubeDisplay *cube_display) = 0;		//   given CubeDisplay

  void constructorHelper ();			// helps constructors

  void set					// sets up cube table
    (Cube *deleted_cube = 0);			//  given a cube being deleted

  virtual void selectEntry			// called for select when
    (int index) = 0;				//   given index is selected

  void removeEntry				// called for delete when
    (int index);				//   given index is removed

  int getCountForACube				// finde the # of cubes
    (Cube *cube);				//   given a cube to search for

  int getIndex					// return index of
    (Cube *cube,				//   given cube with the
     int which_one);				//   #(zero-rel) of which one

  int getIndex					// return index of
    (Cube *cube,				//   given cube with the
     CubeDisplay *cube_display);		//   given cube display

  void changeCubeName				// cube name just changed
    (Cube *changed_cube);			//   given cube

  enum Identifier {
    SELECTION = 50, WINDOW, FILENAME
  };

  CubeDisplay
    **_cube_displays;				// array of cube displays

  Cube
    **_cubes;					// array of cubes

  SLErrorPop
    *_errpop;					// error popup

  Boolean
    _first_time;				// False after constructor runs

  long
    _button,					// select button style on table
    _nrows,					// current # of rows in table
    _nmax,					// maximum # of rows in table
    *_selections,				// radio/toggle array
    *_windows;					// which cube display array

  char
    *_blankfilename,				// a filename of all blanks
    *_fnames,					// filenames array
    **_filenames;				// filename pointer array
};

#endif








class CubeTableShow : public CubeTable {

public:
  CubeTableShow					// constructor
    (Widget p,					//   Gui widget parent
     char *name);				//   name of table

  CubeTableShow					// constructor
    (SLDelay *slp,				//   Gui widget parent
     char *name);				//   name of table

  virtual ~CubeTableShow ();			// destructor

private:
  virtual void setCurrentCube ()		// to set current cube
    { set (); }

  virtual void insertNewCube			// insert just added
    (Cube *cube)				//   the given cube
    { if (cube && !find(cube)) addCube (cube); set (); }

  virtual void removeOldCube			// remove just deleted
    (Cube *cube)				//   given cube
    { /*delCube (cube) Done by informer!*/; set (cube); }

  virtual void selectEntry			// do when table entry selected
    (int index);				//   the given index

  virtual long tableButton ()			// returns table button to use
    { return 3; }

  virtual void tableMessage ();			// writes a table message

  virtual long selectionState			// return selection flag
    (Cube *cube,				//   given Cube and
     CubeDisplay *cube_display);		//   given CubeDisplay
};





class CubeTableSelectGui;

class CubeTableSelect : public CubeTable {

public:
  CubeTableSelect				// constructor
    (Widget p,					//   Gui widget parent
     char *name,				//   name of table
     CubeTableSelectGui *ctsg);			//   given GUI

  CubeTableSelect				// constructor
    (SLDelay *slp,				//   SL Delay parent
     char *name,				//   name of table
     CubeTableSelectGui *ctsg);			//   given GUI

  virtual ~CubeTableSelect ();			// destructor

private:
  virtual void insertNewCube			// insert just added
    (Cube *cube);				//   given cube

  virtual void removeOldCube			// remove just deleted
    (Cube *cube);				//   given cube

  virtual void selectEntry			// do when table entry selected
    (int index);				//   the given index

  virtual long tableButton ()			// returns table button to use
    { return 4; }

  virtual void tableMessage ();			// writes a table message

  virtual long selectionState			// set selection flag
    (Cube *cube,				//   given Cube and
     CubeDisplay *cube_display);		//   given CubeDisplay

  CubeTableSelectGui
    *_ctsg;					// given GUI

  Cube
    *_selected_cube;				// currently selected cube

  CubeDisplay
    *_selected_cube_display;			// currently selected cube disp
};
