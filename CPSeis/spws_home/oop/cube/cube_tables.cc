#include "cube/cube_tables.hh"
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
#include "cube/cube_table_guis.hh"
#include "cube/cube_display.hh"
#include "cube/cube.hh"
#include "cube/cube_master.hh"
#include "cube/cube_master_iterator.hh"
#include "sl/sl_error_pop.hh"
#include "sl/prim_support.hh"
#include "wbox.h"
#include "wproc.h"
#include "cprim.h"

#define MAXCUBES "Exceeded maximum allowable\nnumber of cubes."
#define MINCUBES "Each cube display window\nmust contain at least\none cube."

void CubeTable::selectionDo (void *box, long * /* ident */, long *index,
  char * /* text */, long * /* nread */, char *endkey)
{
  CubeTable *cube_table = (CubeTable*)SLDatabox::getUserData(box);
  if (*index > 0 && *index <= cube_table->_nrows) {
    if (!strcmp("RETURN", endkey)) {
      cube_table->selectEntry ((int)(*index-(long)1));
    }
    else if (!strcmp("REMOVE",endkey)) {
      cube_table->removeEntry ((int)(*index-(long)1));
      strcpy (endkey, " ");
    }
    else if (!strcmp("INSERT", endkey)) {
      strcpy (endkey, " ");
    }
  }
}

CubeTable::CubeTable (Widget parent, char *name) :
  SLDatabox (parent, name, this),
  CubeInform (),
  _first_time     (True),
  _selections     (0),
  _windows        (0),
  _blankfilename  (0),
  _fnames         (0),
  _filenames      (0),
  _cube_displays  (0),
  _cubes          (0)
{
  constructorHelper ();
}

CubeTable::CubeTable (SLDelay *slparent, char *name) :
  SLDatabox (slparent, name, this),
  CubeInform (),
  _first_time     (True),
  _selections     (0),
  _windows        (0),
  _blankfilename  (0),
  _fnames         (0),
  _filenames      (0),
  _cube_displays  (0),
  _cubes          (0)
{
  constructorHelper ();
}

CubeTable::~CubeTable ()
{
  CubeMaster::instance()->delInformer (this);
  if (_selections)    delete [] _selections;
  if (_windows)       delete [] _windows;
  if (_blankfilename) delete [] _blankfilename;
  if (_fnames)        delete [] _fnames;
  if (_filenames)     delete [] _filenames;
  if (_cube_displays) delete [] _cube_displays;
  if (_cubes)         delete [] _cubes;
}

void CubeTable::makeHelper ()
{
  set ();

  static long zero = 0, five = 5;

  _button = tableButton ();

  tableMessage ();

  _nmax = TABLE_SIZE;

         //  TRAP ID VARIABLE SWITCH ROW COL NCHAR NDEC

//------------------------- display a set of linked arrays:

         //     N        NMAX    ROW    MAXROWS
  wbox_rega   (&_nrows, &_nrows,  0,  TABLE_SIZE);

         //      TRAP         ID        LABEL     SWITCH  VARIABLE    SWITCH
  wbox_irega (selectionDo, SELECTION,  "Select",  &zero, _selections, &_button,
	 //  COL     NCHAR       NDEC
              0,       2,         0);
  wbox_irega (selectionDo,  WINDOW,    "Window",  &zero,  _windows,    &five,
              0,       2,         0);
  wbox_crega (selectionDo, FILENAME, "Cube File", &zero,  _fnames,     &five,
              0, FILENAME_LENGTH, 0);
}

void CubeTable::constructorHelper ()
{
  _selections = new long [TABLE_SIZE];
  _windows    = new long [TABLE_SIZE];
  _fnames     = new char [TABLE_SIZE*FILENAME_LENGTH+1];
  _filenames  = new char*[TABLE_SIZE];
  int k2;
  int offset = 0;
  for (k2 = 0; k2 < TABLE_SIZE; k2++) {
    _filenames[k2] = &_fnames[offset];
    offset += FILENAME_LENGTH;
  }
  _blankfilename = new char[FILENAME_LENGTH+1];
  _cube_displays = new CubeDisplay*[TABLE_SIZE];
  _cubes         = new Cube*[TABLE_SIZE];

  CubeMaster::instance()->addInformer (this);
}

void CubeTable::set (Cube *deleted_cube)
{
  char blank[4];
  strcpy (blank, " ");
  int iblnk = (int)blank[0];
  int iterm = (int)blank[1];
  memset ((void *)_blankfilename, (unsigned char)iblnk,
    (size_t)FILENAME_LENGTH);
  memset ((void *)_blankfilename, (unsigned char)iterm, (size_t)1);
  memset ((void *)(&_fnames[TABLE_SIZE*FILENAME_LENGTH]),
                                  (unsigned char)iterm, (size_t)1);
  
  int k2;
// initialize arrays
  for (k2 = 0; k2 < TABLE_SIZE; k2++) {
    _selections[k2] = 0;
    _windows[k2] = 0;
    memcpy ((void *)_filenames[k2], (const void *)_blankfilename,
      (size_t)FILENAME_LENGTH);
    _cube_displays[k2] = 0;
    _cubes[k2] = 0;
  }

  _nrows = 0;
  {
    for (CubeMasterIterator cmi; cmi.currentCube(); cmi.nextCube()) {
      if (cmi.currentCube() != deleted_cube) _nrows++;
    }
  }

  if (_nrows >= TABLE_SIZE) {
    _errpop = new SLErrorPop (topWidget(), "Warning", MAXCUBES);
  }

  Cube *cube;
  if (_first_time) {
    _nrows = 0;
    for (CubeMasterIterator cmi; cmi.currentCube() && _nrows < TABLE_SIZE;
      cmi.nextCube()) {
      cube = cmi.currentCube ();
      if (!find(cube) && cube != deleted_cube) addCube (cube);
      _nrows++;
    }
    _first_time = False;
  }

  void *x, *y;
  CubeDisplay *cube_display;
  _nrows = 0;
  long window = 0;
  size_t slen;
  int offset;
  for (cube_display = CubeMaster::instance()->top(&x); cube_display && 
    _nrows < TABLE_SIZE; cube_display = CubeMaster::instance()->next(&x)) {
    for (cube = cube_display->top(&y); (cube) && _nrows < TABLE_SIZE;
      cube = cube_display->next(&y)) {
      if (cube != deleted_cube) {
        _cube_displays[_nrows] = cube_display;
        _cubes[_nrows] = cube;
        _selections[_nrows] = selectionState (cube, cube_display);
        slen = strlen(cube->primaryFilename());
        if (slen > (size_t)FILENAME_LENGTH) {
          offset = (int)slen - (int)FILENAME_LENGTH;
          slen = (size_t)FILENAME_LENGTH;
          memcpy ((void *)_filenames[_nrows],
            (const void *)(cube->primaryFilename()+offset), slen); 
        }
        else if (slen == (size_t)FILENAME_LENGTH) {
          memcpy ((void *)_filenames[_nrows],
            (const void *)cube->primaryFilename(), slen); 
        }
        else /* if (slen < (size_t)FILENAME_LENGTH) */ {
          memcpy ((void *)_filenames[_nrows], (const void *)_blankfilename,
            (size_t)(FILENAME_LENGTH));
          memcpy ((void *)_filenames[_nrows],
            (const void *)cube->primaryFilename(), slen); 
        }
        _windows[_nrows] = window;
        _nrows++;
      }
    }
    window++;
  }
  PrimSupport::updateEverything ();
}

void CubeTable::removeEntry (int index)
{
  if (_cube_displays[index]->count() < 2) {
    _errpop = new SLErrorPop (topWidget(), "Warning", MINCUBES);
  }
  else {
    Cube *cube = _cubes[index];
    delete cube;  // This is powerful access by the user...
  }
}

int CubeTable::getCountForACube (Cube *cube)
{
  int count = 0;
  if (!cube) return count;
  int k2;
  for (k2 = 0; k2 < _nrows; k2++) {
    if (_cubes[k2] == cube) count++;
  }
  return count;
}

int CubeTable::getIndex (Cube *cube, int which_one)
{
  int retval = -1;
  int count = -1;
  if (!cube || which_one < 0) return retval;
  int k2;
  for (k2 = 0; k2 < _nrows && retval == -1; k2++) {
    if (_cubes[k2] == cube) count++;
    if (count == which_one) retval = k2;
  }
  return retval;
}

int CubeTable::getIndex (Cube *cube, CubeDisplay *cube_display)
{
  int retval = -1;
  if (!cube) return retval;
  int k2;
  for (k2 = 0; k2 < _nrows; k2++) {
    if (_cubes[k2] == cube && _cube_displays[k2] == cube_display) return k2;
  }
  return retval;
}

void CubeTable::changeCubeName (Cube *changed_cube)
{
  int count = getCountForACube (changed_cube);
  if (count > -1) {
    char blank[4];
    strcpy (blank, " ");
    int iblnk = (int)blank[0];
    int iterm = (int)blank[1];
    memset ((void *)_blankfilename, (unsigned char)iblnk,
      (size_t)FILENAME_LENGTH);
    memset ((void *)_blankfilename, (unsigned char)iterm, (size_t)1);

    int index, k2, offset;
    size_t slen;
    for (k2 = 0; k2 < count; k2++) {
      index = getIndex (changed_cube, k2);
      if (index > -1) {
        memcpy ((void *)_filenames[index], (const void *)_blankfilename,
          (size_t)FILENAME_LENGTH);
        slen = strlen(_cubes[index]->primaryFilename());
        if (slen > (size_t)FILENAME_LENGTH) {
          offset = (int)slen - (int)FILENAME_LENGTH;
          slen = (size_t)FILENAME_LENGTH;
          memcpy ((void *)_filenames[index],
            (const void *)(_cubes[index]->primaryFilename()+offset), slen); 
        }
        else if (slen == (size_t)FILENAME_LENGTH) {
          memcpy ((void *)_filenames[index],
            (const void *)_cubes[index]->primaryFilename(), slen); 
        }
        else /* if (slen < (size_t)FILENAME_LENGTH) */ {
          memcpy ((void *)_filenames[index], (const void *)_blankfilename,
            (size_t)(FILENAME_LENGTH));
          memcpy ((void *)_filenames[index],
            (const void *)_cubes[index]->primaryFilename(), slen); 
        }
      }
    }
  }
  PrimSupport::updateEverything ();
}






CubeTableShow::CubeTableShow (Widget p, char *name) :
  CubeTable (p, name)
{
}

CubeTableShow::CubeTableShow (SLDelay *slp, char *name) :
  CubeTable (slp, name)
{
}

CubeTableShow::~CubeTableShow ()
{
}

void CubeTableShow::selectEntry (int index)
{
  _cube_displays[index]->displayCube(_cubes[index]);
}

void CubeTableShow::tableMessage ()
{
  wbox_message ("Push toggle button to display cube.");
  wbox_blank_line ();
}

long CubeTableShow::selectionState (Cube *cube, CubeDisplay *cube_display)
{
  return (long)(cube == cube_display->currentDisplayedCube ());
}





CubeTableSelect::CubeTableSelect (Widget p, char *name,
  CubeTableSelectGui *ctsg) :
  CubeTable (p, name),
  _ctsg                  (ctsg),
  _selected_cube         (0),
  _selected_cube_display (0)
{
}

CubeTableSelect::CubeTableSelect (SLDelay *slp, char *name,
  CubeTableSelectGui *ctsg) :
  CubeTable (slp, name),
  _ctsg                  (ctsg),
  _selected_cube         (0),
  _selected_cube_display (0)
{
}

CubeTableSelect::~CubeTableSelect ()
{
}

void CubeTableSelect::insertNewCube (Cube *cube)
{
  if (cube && !find(cube)) addCube (cube);
  set ();
  int index = getIndex (_selected_cube, _selected_cube_display);
  if (index > -1) _selections[index] = 1;
  PrimSupport::updateEverything ();
}

void CubeTableSelect::removeOldCube (Cube *cube)
{
/*if (cube && find(cube)) delCube (cube); This is done by the informer! */
  set (cube);
  int index = getIndex (_selected_cube, _selected_cube_display);
  if (index > -1) _selections[index] = 1;
  PrimSupport::updateEverything ();
}

void CubeTableSelect::selectEntry (int index)
{
  set ();
  _selections[index]     = 1;
  _selected_cube         = _cubes[index];
  _selected_cube_display = _cube_displays[index];
  _ctsg->displaySectionNumbers (_cubes[index]);
  PrimSupport::updateEverything ();
}

void CubeTableSelect::tableMessage ()
{
  wbox_message ("Push radio buttons to select a section.");
  wbox_blank_line ();
}

long CubeTableSelect::selectionState (Cube * /*cube*/,
  CubeDisplay * /*cube_display*/)
{
  return 0;
}
