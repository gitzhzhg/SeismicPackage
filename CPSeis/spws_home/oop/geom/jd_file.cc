
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
//---------------------- jd_file.cc ------------------------//
//---------------------- jd_file.cc ------------------------//
//---------------------- jd_file.cc ------------------------//

//            implementation file for the JdFile class
//                 derived from the FileBase class
//                 derived from the FgInform class
//                        subdirectory geom


#include "geom/jd_file.hh"
#include "geom/fg_constants.hh"
#include "geom/field_geometry.hh"
#include "geom/geomio_wrapper.hh"
#include "oprim/grid_transform.hh"
#include "cprim.h"
#include "str.h"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


static const int SMART_ALLOC = TRUE;

#define FILETYPE   "JD file"
#define EXTENSION  "jd"
#define NBUF       200


//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//
//-------------------- public functions ---------------------------//




//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//


JdFile::JdFile(FieldGeometry *fg, Intent intent)
       : FileBase(FILETYPE, EXTENSION, intent, TRUE),
         FgInform(fg),
          _ld_read_flag   (TRUE),
          _rp_read_flag   (TRUE),
          _pp_read_flag   (TRUE),
         _zt1_read_flag   (TRUE),
         _zt2_read_flag   (TRUE),
         _zt3_read_flag   (TRUE),
         _zt4_read_flag   (TRUE),
        _grid_read_flag   (TRUE),

         _ld_clear_flag   (TRUE),
         _rp_clear_flag   (TRUE),
         _pp_clear_flag   (TRUE),
        _zt1_clear_flag   (TRUE),
        _zt2_clear_flag   (TRUE),
        _zt3_clear_flag   (TRUE),
        _zt4_clear_flag   (TRUE),

          _ld_save_flag   (TRUE),
          _rp_save_flag   (TRUE),
          _pp_save_flag   (TRUE),
         _zt1_save_flag   (TRUE),
         _zt2_save_flag   (TRUE),
         _zt3_save_flag   (TRUE),
         _zt4_save_flag   (TRUE),
        _grid_save_flag   (TRUE),
        _repsel           (FALSE),
        _repany           (FALSE)
{
  _geomio[0] = new GeomioWrapper(0);           // input.
  _geomio[1] = new GeomioWrapper(1);           // output overwrite.
  _geomio[2] = new GeomioWrapper(1);           // output parameters.
  _geomio[2]->initializeOutputParameters();
//_geomio[2]->setEncoding("oldcps");           // removed 2003-09-23.
}



//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//


JdFile::~JdFile()
{
  delete _geomio[0];
  delete _geomio[1];
  delete _geomio[2];
}



//--------------------- work message -----------------------------//
//--------------------- work message -----------------------------//
//--------------------- work message -----------------------------//

static void work_message(FieldGeometry *fg, char *prefix, long i, long n)
{
  static char msg[80];
  if(i == 0 || i == 1000 * (i / 1000) || i == n-1)
      {
      if(n >= 0) sprintf(msg, "%s card %ld of %ld", prefix, i+1, n);
      else       sprintf(msg, "%s card %ld"       , prefix, i+1   );
      fg->showMessage(msg);
      }
}



//-------------------- virtual validate -------------------------//
//-------------------- virtual validate -------------------------//
//-------------------- virtual validate -------------------------//

    // protected virtual function overriding FileBase.
    // stores information about the file into member variables.
    // sets info to a string of information about the file (if valid).

FileBase::Validity
JdFile::virtualValidate(const char *filename, char *info)
{
  int io = ioIndex();
  assert(io == 0 || io == 1);
  int error = _geomio[io]->validateFile(filename, info);
  if(io == 0) skipRead();
  if(error) return VALID_NOBUT;
  return VALID_YES;
}



//---------------pre and post new filename ------------------------//
//---------------pre and post new filename ------------------------//
//---------------pre and post new filename ------------------------//

    // protected virtual function overriding FileBase.

void JdFile::preNewFilename()
{
  _fg->preSlowOperations();
}


void JdFile::postNewFilename()
{
  _fg->showMessage("JD file validation completed");
  _fg->postSlowOperations();
}



//----------------------- phrase -------------------------------//
//----------------------- phrase -------------------------------//
//----------------------- phrase -------------------------------//

static void phrase2(long nlines, int repsel, int repany,
                    long num, int cflag, char *spot, char *msg, int *yes)
{
  if(nlines == 0) return;
  char piece[150];
  if(cflag)
      {
      if(repsel)
          sprintf(piece,
           "selected matching lines or flags on %ld %s cards (%ld lines)\n",
                                                 num, spot, nlines);
      else if(repany)
          sprintf(piece,
           "any matching lines or flags on %ld %s cards (%ld lines)\n",
                                                 num, spot, nlines);
      else
          sprintf(piece, "%ld %s cards (%ld lines)\n", num, spot, nlines);
      strcat(msg, piece);
      *yes = TRUE;
      }
}

static void phrase(long num, int cflag, char *spot, char *msg, int *yes)
{
  if(num == 0) return;
  char piece[50];
  if(cflag)
      {
      sprintf(piece, "%ld %s cards\n", num, spot);
      strcat(msg, piece);
      *yes = TRUE;
      }
}



//----------------- virtual prepare read -------------------------//
//----------------- virtual prepare read -------------------------//
//----------------- virtual prepare read -------------------------//

    // protected virtual function overriding FileBase.
    // this function does not put anything into FieldGeometry.
    // does not store any information into member variables.
    // this function currently does not use the file name, but could
    //   do so by checking data on the file (by calling validateJdFile),
    //   or data in memory, or the read/clear flags in this object.


FileBase::Prepare
JdFile::virtualPrepareRead(const char* /*filename*/, char *errmsg)
{
  int io = ioIndex();
  assert(io == 0);
  if(_fg->getDataLock() > LOCK_DEL)
      {
      strcpy(errmsg,   "JD file cannot be input\nwhile data are locked");
      _fg->showMessage("JD file cannot be input while data are locked");
      _fg->ringBell();
      return PROHIBIT;
      }

  if(_fg->getDataLock() >= LOCK_DEL)
      {
      if( (_ld_read_flag  && _ld_clear_flag  && _fg->totNumFlags() > 0) ||
          (_rp_read_flag  && _rp_clear_flag  && _fg->numRpCards () > 0) ||
          (_pp_read_flag  && _pp_clear_flag  && _fg->numPpCards () > 0) ||
          (_zt1_read_flag && _zt1_clear_flag && _fg->numZt1Cards() > 0) ||
          (_zt2_read_flag && _zt2_clear_flag && _fg->numZt2Cards() > 0) ||
          (_zt3_read_flag && _zt3_clear_flag && _fg->numZt3Cards() > 0) ||
          (_zt4_read_flag && _zt4_clear_flag && _fg->numZt4Cards() > 0) )
           {
           strcpy(errmsg, "Cards cannot be replaced\n");
           strcat(errmsg, "from the JD file\n");
           strcat(errmsg, "while data deletions are locked.");
           _fg->showMessage
   ("cannot replace cards from JD file while data deletions are locked");
           _fg->ringBell();
           return PROHIBIT;
           }
      }

  int error = _geomio[io]->verifyParameters(errmsg);
  if(error)
      {
      return PROHIBIT;
      }

  int chaining = getInputChaining();

  if(_ld_read_flag && chaining == INIL)
      {
      strcpy(errmsg, "illegal value of CHAINING on input file");
      return PROHIBIT;
      }

  if(_fg->totNumFlags() > 0 && _fg->getChaining() != chaining
           && _ld_read_flag && !_ld_clear_flag && !_repsel && !_repany)
      {
      strcpy(errmsg, "You may not read in LD cards\n");
      strcat(errmsg, "and append them\n");
      strcat(errmsg, "to existing LD cards\n");
      strcat(errmsg, "when the chaining parameter\n");
      strcat(errmsg, "does not match.");
      return PROHIBIT;
      }

  if(_fg->totNumFlags() > 0 && _fg->getChaining() != chaining
           && _ld_read_flag && _repsel)
      {
      strcpy(errmsg, "You may not read in LD cards\n");
      strcat(errmsg, "and use them to replace information\n");
      strcat(errmsg, "on selected matching lines or flags\n");
      strcat(errmsg, "when the chaining parameter\n");
      strcat(errmsg, "does not match.");
      return PROHIBIT;
      }

  if(_fg->totNumFlags() > 0 && _fg->getChaining() != chaining
           && _ld_read_flag && _repany)
      {
      strcpy(errmsg, "You may not read in LD cards\n");
      strcat(errmsg, "and use them to replace information\n");
      strcat(errmsg, "on any matching lines or flags\n");
      strcat(errmsg, "when the chaining parameter\n");
      strcat(errmsg, "does not match.");
      return PROHIBIT;
      }

  if( (!  _ld_read_flag || numInputLdCards () == 0) &&
      (!  _rp_read_flag || numInputRpCards () == 0) &&
      (!  _pp_read_flag || numInputPpCards () == 0) &&
      (! _zt1_read_flag || numInputZt1Cards() == 0) &&
      (! _zt2_read_flag || numInputZt2Cards() == 0) &&
      (! _zt3_read_flag || numInputZt3Cards() == 0) &&
      (! _zt4_read_flag || numInputZt4Cards() == 0) &&
      (!_grid_read_flag || inputGridTransformStatus() <  0) )
       {
       strcpy(errmsg, "You have not selected\nanything to read\n");
       strcat(errmsg, "from the JD file.\n----\nTry again!");
       _fg->showMessage("nothing selected to read from JD file");
       _fg->ringBell();
       return PROHIBIT;
       }

  int yes = FALSE;
  strcpy(errmsg, "The following field geometry data\n");
  strcat(errmsg, "will disappear\nby being replaced from the file\n");
  strcat(errmsg, "if you continue with this action:\n----\n");
  phrase2(_fg->numLines  (), _repsel, _repany,
         _fg->totNumFlags(), _ld_read_flag  && _ld_clear_flag , "LD" ,
                                                              errmsg, &yes);
  phrase(_fg->numRpCards (), _rp_read_flag  && _rp_clear_flag , "RP" ,
                                                              errmsg, &yes);
  phrase(_fg->numPpCards (), _pp_read_flag  && _pp_clear_flag , "PP" ,
                                                              errmsg, &yes);
  phrase(_fg->numZt1Cards(), _zt1_read_flag && _zt1_clear_flag, "ZT1",
                                                              errmsg, &yes);
  phrase(_fg->numZt2Cards(), _zt2_read_flag && _zt2_clear_flag, "ZT2",
                                                              errmsg, &yes);
  phrase(_fg->numZt3Cards(), _zt3_read_flag && _zt3_clear_flag, "ZT3",
                                                              errmsg, &yes);
  phrase(_fg->numZt4Cards(), _zt4_read_flag && _zt4_clear_flag, "ZT4",
                                                              errmsg, &yes);
  if(_grid_read_flag && inputGridTransformStatus() > 0 && _fg->numLines() > 0)
              { strcat(errmsg, "the grid transform\n"); yes = TRUE; }
  if(!yes) return GODSPEED;
  strcat(errmsg, "----\n");
  if(_fg->dataNeedsSaving())
      {
      strcat(errmsg, "You have data which\nhas NOT been saved.\n----\n");
      }
  strcat(errmsg, "Are you sure you want\nto replace this data\n");
  strcat(errmsg, "while reading this file?");
  return CAUTION;
}



//----------------- virtual prepare save -------------------------//
//----------------- virtual prepare save -------------------------//
//----------------- virtual prepare save -------------------------//

    // protected virtual function overriding FileBase.
    // currently not needed.




//--------------------- virtual read -----------------------------//
//--------------------- virtual read -----------------------------//
//--------------------- virtual read -----------------------------//

    // protected virtual function overriding FileBase.

FileBase::Result
JdFile::virtualRead(const char *filename, char *errmsg)
{
  int io = ioIndex();
  assert(io == 0);
  int error = _geomio[io]->openInputFile(filename, errmsg);
  if(error)
      {
      _fg->showMessage(errmsg);
      _fg->ringBell();
      return FAILURE;
      }

  int all_new_data =
      (( _ld_read_flag &&  _ld_clear_flag && !_repsel && !_repany)
                                           || _fg->numLines   () == 0) &&
      (( _rp_read_flag &&  _rp_clear_flag) || _fg->numRpCards () == 0) &&
      (( _pp_read_flag &&  _pp_clear_flag) || _fg->numPpCards () == 0) &&
      ((_zt1_read_flag && _zt1_clear_flag) || _fg->numZt1Cards() == 0) &&
      ((_zt2_read_flag && _zt2_clear_flag) || _fg->numZt2Cards() == 0) &&
      ((_zt3_read_flag && _zt3_clear_flag) || _fg->numZt3Cards() == 0) &&
      ((_zt4_read_flag && _zt4_clear_flag) || _fg->numZt4Cards() == 0) &&
      (_grid_read_flag);

  _fg->preSlowOperations();

  if(_grid_read_flag)
      {
      float ve      = _geomio[io]->getScalarFloat("ve");
      float ref     = _geomio[io]->getScalarFloat("datum");
      float fixdist = _geomio[io]->getScalarFloat("fixdist");
      if(ve      != FNIL && ve > 0.0) _fg->setVe     (ve);
      if(ref     != FNIL)             _fg->setRef    (ref);
      if(fixdist != FNIL)             _fg->setFixdist(fixdist);
      if(_geomio[io]->keywordPresent("grid"))
          {
          GridTransform *grid = new GridTransform();
          _geomio[io]->getGridTransform(grid);
          _fg->resetTestingGridTransformValues();
          _fg->setTestingXorigin      (grid->getXorigin());
          _fg->setTestingYorigin      (grid->getYorigin());
          _fg->setTestingRotationAngle(grid->getRotationAngle());
          _fg->setTestingXgridWidth   (grid->getXgridWidth());
          _fg->setTestingYgridWidth   (grid->getYgridWidth());
          if(grid->isRightHanded()) _fg->setTestingRightHanded();
          else                      _fg->setTestingLeftHanded();
          delete grid;
          _fg->setGridTransformValues();
          }
      }

  int chaining = getInputChaining();
  int nld      = numInputLdCards ();
  int nrp      = numInputRpCards ();
  int npp      = numInputPpCards ();
  int nzt1     = numInputZt1Cards();
  int nzt2     = numInputZt2Cards();
  int nzt3     = numInputZt3Cards();
  int nzt4     = numInputZt4Cards();

  if(strcmp(_geomio[io]->getEncoding(), "oldcps") != 0)
      {
      if(!_ld_read_flag ) nld  = 0;
      if(!_rp_read_flag ) nrp  = 0;
      if(!_pp_read_flag ) npp  = 0;
      if(!_zt1_read_flag) nzt1 = 0;
      if(!_zt2_read_flag) nzt2 = 0;
      if(!_zt3_read_flag) nzt3 = 0;
      if(!_zt4_read_flag) nzt4 = 0;
      }

  if(_ld_read_flag && chaining != _fg->getChaining())
      {
      if(!_fg->allowChangeChaining()) _fg->resumeDependentUpdates();
      assert(_fg->allowChangeChaining());
      _fg->setChaining(chaining);
      }

  _fg->preMajorChanges();

  if(!error) error = readLdCards       (nld , errmsg);
  if(!error) error = readRpCards       (nrp , errmsg);
  if(!error) error = readPpCards       (npp , errmsg);
  if(!error) error = readZt1Cards      (nzt1, errmsg);
  if(!error) error = readZt2Cards      (nzt2, errmsg);
  if(!error) error = readZt3Cards      (nzt3, errmsg);
  if(!error) error = readZt4Cards      (nzt4, errmsg);

  _geomio[io]->closeFile();

  _fg->sortByLineNumber();                   // inserted 9/24/96
  _fg->sortReceiverPatterns();               // inserted 9/24/96
  _fg->postMajorChanges();
  if(error)
      {
      _fg->showMessage(errmsg);
      _fg->ringBell();
      }
  else
      {
      strcpy(errmsg, "JD file successfully read");
      _fg->showMessage(errmsg);
      char newmsg[200];
      strcpy(newmsg,   "JD file last read = ");
      strcat(newmsg,   filename);
      _fg->sendMessage(newmsg, 0, 0, 0, 0, 0);
      if(all_new_data) _fg->turnOffDataNeedsSavingFlag();
      }
  _fg->postSlowOperations();
  if(error) return FAILURE;
  return SUCCESS;
}



//--------------------- virtual save -----------------------------//
//--------------------- virtual save -----------------------------//
//--------------------- virtual save -----------------------------//

    // protected virtual function overriding FileBase.
    // also stores information about the file into member variables.

FileBase::Result
JdFile::virtualSave(const char *filename, char *errmsg)
{
  int io = ioIndex();
  assert(io == 1);  // io == 1 is for existing output files to be overwritten.
  io = 2;           // io == 2 is for parameters to write to output files.

  _fg->preSlowOperations();
  _fg->showMessage("saving JD file...");

  updatePjar();

/*
  int chaining = _fg->getChaining();
  const char *chainstr = getChainingString(chaining);
  assert(chainstr);

  long nld  = 0;
  long nrp  = 0;
  long npp  = 0;
  long nzt1 = 0;
  long nzt2 = 0;
  long nzt3 = 0;
  long nzt4 = 0;
  if(_ld_save_flag)
      {
      long nlines = _fg->numLines();
      for(long ixl = 0; ixl < nlines; ixl++)
          {
          nld += _fg->numFlagsOnLine(ixl);
          }
      }
  if(_rp_save_flag ) nrp  = _fg->numRpCards();
  if(_pp_save_flag ) npp  = _fg->numPpCards();
  if(_zt1_save_flag) nzt1 = _fg->numZt1Cards();
  if(_zt2_save_flag) nzt2 = _fg->numZt2Cards();
  if(_zt3_save_flag) nzt3 = _fg->numZt3Cards();
  if(_zt4_save_flag) nzt4 = _fg->numZt4Cards();

  _geomio[io]->setScalarString ("chaining", chainstr);
  _geomio[io]->setScalarInteger("nld" , nld );
  _geomio[io]->setScalarInteger("nrp" , nrp );
  _geomio[io]->setScalarInteger("npp" , npp );
  _geomio[io]->setScalarInteger("nzt1", nzt1);
  _geomio[io]->setScalarInteger("nzt2", nzt2);
  _geomio[io]->setScalarInteger("nzt3", nzt3);
  _geomio[io]->setScalarInteger("nzt4", nzt4);

  if(_grid_save_flag)
      {
      float  ve      = _fg->getVe();
      float  ref     = _fg->getRef();
      float  fixdist = _fg->getFixdist();
      double xorigin = _fg->getXorigin();
      double yorigin = _fg->getYorigin();
      double angle   = _fg->getRotationAngle();
      double xwidth  = _fg->getXgridWidth();
      double ywidth  = _fg->getYgridWidth();
      int    right   = _fg->isRightHanded();
      GridTransform *grid = new GridTransform();
      if(right) grid->setRightHandedTransform(xorigin, yorigin,
                                              angle, xwidth, ywidth);
      else      grid->setLeftHandedTransform (xorigin, yorigin,
                                              angle, xwidth, ywidth);
      _geomio[io]->setScalarFloat  ("ve"     , ve);
      _geomio[io]->setScalarFloat  ("datum"  , ref);
      _geomio[io]->setScalarFloat  ("fixdist", fixdist);
      _geomio[io]->setGridTransform(grid);
      delete grid;
      }
*/

  int error = _geomio[io]->openOutputFile(filename, errmsg);
  if(error)
      {
      _fg->showMessage(errmsg);
      _fg->ringBell();
      _fg->postSlowOperations();
      return FAILURE;
      }

  if(!error) error = saveLdCards ();
  if(!error) error = saveRpCards ();
  if(!error) error = savePpCards ();
  if(!error) error = saveZt1Cards();
  if(!error) error = saveZt2Cards();
  if(!error) error = saveZt3Cards();
  if(!error) error = saveZt4Cards();

  _geomio[io]->closeFile();

  if(error)
      {
      strcpy(errmsg,   "JD file write error");
      _fg->showMessage("JD file write error");
      _fg->ringBell();
      _fg->postSlowOperations();
      return FAILURE;
      }

   if( _ld_save_flag &&
       _rp_save_flag &&
       _pp_save_flag &&
      _zt1_save_flag &&
      _zt2_save_flag &&
      _zt3_save_flag &&
      _zt4_save_flag &&
      _grid_save_flag) _fg->turnOffDataNeedsSavingFlag();

  _fg->showMessage("JD file successfully saved");
  char newmsg[200];
  strcpy(newmsg,   "JD file last saved = ");
  strcat(newmsg,   filename);
  _fg->sendMessage(newmsg, 0, 0, 0, 0, 0);
  _fg->postSlowOperations();
  return SUCCESS;
}


//----------------------- update pjar -----------------------------------//
//----------------------- update pjar -----------------------------------//
//----------------------- update pjar -----------------------------------//

            // call this prior to saving the file,
            // or when something in the field geometry object changes,
            // or any time the user requests.


void JdFile::updatePjar ()
{
  int io = 2;       // io == 2 is for parameters to write to output files.

  int chaining = _fg->getChaining();
  const char *chainstr = getChainingString(chaining);
  assert(chainstr);

  long nld  = 0;
  long nrp  = 0;
  long npp  = 0;
  long nzt1 = 0;
  long nzt2 = 0;
  long nzt3 = 0;
  long nzt4 = 0;
  if(_ld_save_flag)
      {
      long nlines = _fg->numLines();
      for(long ixl = 0; ixl < nlines; ixl++)
          {
          nld += _fg->numFlagsOnLine(ixl);
          }
      }
  if(_rp_save_flag ) nrp  = _fg->numRpCards();
  if(_pp_save_flag ) npp  = _fg->numPpCards();
  if(_zt1_save_flag) nzt1 = _fg->numZt1Cards();
  if(_zt2_save_flag) nzt2 = _fg->numZt2Cards();
  if(_zt3_save_flag) nzt3 = _fg->numZt3Cards();
  if(_zt4_save_flag) nzt4 = _fg->numZt4Cards();

  _geomio[io]->setScalarString ("chaining", chainstr);
  _geomio[io]->setScalarInteger("nld" , nld );
  _geomio[io]->setScalarInteger("nrp" , nrp );
  _geomio[io]->setScalarInteger("npp" , npp );
  _geomio[io]->setScalarInteger("nzt1", nzt1);
  _geomio[io]->setScalarInteger("nzt2", nzt2);
  _geomio[io]->setScalarInteger("nzt3", nzt3);
  _geomio[io]->setScalarInteger("nzt4", nzt4);

  if(_grid_save_flag)
      {
      float  ve      = _fg->getVe();
      float  ref     = _fg->getRef();
      float  fixdist = _fg->getFixdist();
      double xorigin = _fg->getXorigin();
      double yorigin = _fg->getYorigin();
      double angle   = _fg->getRotationAngle();
      double xwidth  = _fg->getXgridWidth();
      double ywidth  = _fg->getYgridWidth();
      int    right   = _fg->isRightHanded();
      GridTransform *grid = new GridTransform();
      if(right) grid->setRightHandedTransform(xorigin, yorigin,
                                              angle, xwidth, ywidth);
      else      grid->setLeftHandedTransform (xorigin, yorigin,
                                              angle, xwidth, ywidth);
      _geomio[io]->setScalarFloat  ("ve"     , ve);
      _geomio[io]->setScalarFloat  ("datum"  , ref);
      _geomio[io]->setScalarFloat  ("fixdist", fixdist);
      _geomio[io]->setGridTransform(grid);
      delete grid;
      }
  else
      {
      _geomio[io]->removeKeyword  ("ve"     );
      _geomio[io]->removeKeyword  ("datum"  );
      _geomio[io]->removeKeyword  ("fixdist");
      _geomio[io]->removeKeyword  ("grid"   );
      }
}



//------------------- private helper functions ---------------------//
//------------------- private helper functions ---------------------//
//------------------- private helper functions ---------------------//
//------------------- private helper functions ---------------------//
//------------------- private helper functions ---------------------//
//------------------- private helper functions ---------------------//
//------------------- private helper functions ---------------------//
//------------------- private helper functions ---------------------//
//------------------- private helper functions ---------------------//
//------------------- private helper functions ---------------------//
//------------------- private helper functions ---------------------//
//------------------- private helper functions ---------------------//
//------------------- private helper functions ---------------------//
//------------------- private helper functions ---------------------//
//------------------- private helper functions ---------------------//




//------------------- check append status ----------------------//
//------------------- check append status ----------------------//
//------------------- check append status ----------------------//

         // private.

int JdFile::checkAppendStatus(long index, const char *type,
                                           long i, long n, char *errmsg)
{
  if(index == -1)
      {
      if(n >= 0)
          {
          sprintf(errmsg, "error trying to append %s card# %ld of %ld",
                                     type, i+1, n);
          }
      else
          {
          sprintf(errmsg, "error trying to append %s card# %ld",
                                     type, i+1);
          }
      return TRUE;
      }
  return FALSE;
}



//--------------- get chaining or chaining string ----------------//
//--------------- get chaining or chaining string ----------------//
//--------------- get chaining or chaining string ----------------//

         // private.

static const char * const chaining_HORI = "HORI";
static const char * const chaining_SLOP = "SLOP";
static const char * const chaining_NONE = "NONE";


int JdFile::getChaining(const char *buffer, char *errmsg)
{
  int chaining = INIL;
  if     (strings_equal((char*)buffer, (char*)chaining_HORI))
                                              chaining = HORIZONTAL_CHAINING;
  else if(strings_equal((char*)buffer, (char*)chaining_SLOP))
                                              chaining = SLOPE_CHAINING;
  else if(strings_equal((char*)buffer, (char*)chaining_NONE))
                                              chaining = NO_CHAINING;
  else
      {
      strcpy(errmsg, "bad chaining parameter in JD file");
      }
  return chaining;
}


const char *JdFile::getChainingString(int chaining)
{
  switch(chaining)
      {
      case HORIZONTAL_CHAINING: return chaining_HORI;
      case SLOPE_CHAINING     : return chaining_SLOP;
      case NO_CHAINING        : return chaining_NONE;
      default: break;
      }
  return NULL;
}



//--------------- get rp flag or rp flag string ------------------//
//--------------- get rp flag or rp flag string ------------------//
//--------------- get rp flag or rp flag string ------------------//

         // private.

static const char * const rpflag_X    = "X";
static const char * const rpflag_Y    = "Y";
static const char * const rpflag_SKIP = "SKIP";
static const char * const rpflag_DUP  = "DUP";


int JdFile::getRpFlag(const char *buffer, long i, long n, char *errmsg)
{
  int rpflag = INIL;
  if     (strings_equal((char*)buffer, (char*)rpflag_X   ))
                                              rpflag = RP_FLAG_X;
  else if(strings_equal((char*)buffer, (char*)rpflag_Y   ))
                                              rpflag = RP_FLAG_Y;
  else if(strings_equal((char*)buffer, (char*)rpflag_SKIP))
                                              rpflag = RP_FLAG_SKIP;
  else if(strings_equal((char*)buffer, (char*)rpflag_DUP ))
                                              rpflag = RP_FLAG_DUP;
  else if(n >= 0)
      {
      sprintf(errmsg, "bad flag %s on RP card# %ld of %ld", buffer, i+1, n);
      }
  else
      {
      sprintf(errmsg, "bad flag %s on RP card# %ld", buffer, i+1);
      }
  return rpflag;
}



const char *JdFile::getRpFlagString(int rpflag)
{
  switch(rpflag)
      {
      case RP_FLAG_X   : return rpflag_X   ;
      case RP_FLAG_Y   : return rpflag_Y   ;
      case RP_FLAG_SKIP: return rpflag_SKIP;
      case RP_FLAG_DUP : return rpflag_DUP ;
      default: break;
      }
  return NULL;
}



//-------------- get zt code or zt code string -------------------//
//-------------- get zt code or zt code string -------------------//
//-------------- get zt code or zt code string -------------------//

         // private.

static const char * const ztcode_ZERO = "ZERO";
static const char * const ztcode_REV  = "REV";
static const char * const ztcode_MISS = "MISS";


int JdFile::getZtCode(const char *buffer, const char *type,
                                          long i, long n, char *errmsg)
{
  int ztcode = INIL;
  if     (strings_equal((char*)buffer, (char*)ztcode_ZERO))
                                              ztcode = ZT_CODE_ZERO;
  else if(strings_equal((char*)buffer, (char*)ztcode_REV ))
                                              ztcode = ZT_CODE_REV;
  else if(strings_equal((char*)buffer, (char*)ztcode_MISS))
                                              ztcode = ZT_CODE_MISS;
  else if(n >= 0)
      {
      sprintf(errmsg, "bad code %s on %s card# %ld of %ld",
                                           buffer, type, i+1, n);
      }
  else
      {
      sprintf(errmsg, "bad code %s on %s card# %ld",
                                           buffer, type, i+1);
      }
  return ztcode;
}



const char *JdFile::getZtCodeString(int ztcode)
{
  switch(ztcode)
      {
      case ZT_CODE_ZERO: return ztcode_ZERO;
      case ZT_CODE_REV : return ztcode_REV ;
      case ZT_CODE_MISS: return ztcode_MISS;
      default: break;
      }
  return NULL;
}



//------------------- private read functions -----------------------//
//------------------- private read functions -----------------------//
//------------------- private read functions -----------------------//
//------------------- private read functions -----------------------//
//------------------- private read functions -----------------------//
//------------------- private read functions -----------------------//
//------------------- private read functions -----------------------//
//------------------- private read functions -----------------------//
//------------------- private read functions -----------------------//
//------------------- private read functions -----------------------//
//------------------- private read functions -----------------------//
//------------------- private read functions -----------------------//
//------------------- private read functions -----------------------//
//------------------- private read functions -----------------------//
//------------------- private read functions -----------------------//



//--------------------- read LD cards --------------------------//
//--------------------- read LD cards --------------------------//
//--------------------- read LD cards --------------------------//

//  showMessage is placed also at bottom of first time thru the loop
//  in order to come after the message "dependent values out of date".

int JdFile::readLdCards(long nld, char *errmsg)
{
  if(_ld_read_flag && _ld_clear_flag && _fg->numLines() > 0
                        && !_repsel && !_repany) _fg->deleteAllLines();
  if(nld == 0) return FALSE;
  long   ixl_keep = -1;
  long   nadd     = 10000;
  for(long i = 0; i < nld || nld < 0; i++)
      {
      int err;
      float shot, dist2, xloc2, yloc2, elev, hd, tuh;
      float rstat, sstat, xskid, yskid, eskid;
      int line;

      _geomio[0]->readLdCard   (i, &err, errmsg,
                     &shot, &dist2, &xloc2, &yloc2, &elev, &hd, &tuh,
                     &rstat, &sstat, &xskid, &yskid, &eskid, &line);

      if(err == GeomioWrapper::STATUS_EOF && nld < 0) break;
      if(err != GeomioWrapper::STATUS_OK) return TRUE;
      work_message(_fg, "reading LD (line description)", i, nld);
      if(!_ld_read_flag) continue;

      double dist = DNIL;
      double xloc = DNIL;
      double yloc = DNIL;

      if(dist2 != FNIL) dist = (double)dist2;
      if(xloc2 != FNIL) xloc = (double)xloc2;
      if(yloc2 != FNIL) yloc = (double)yloc2;

      long ixl = _fg->findMatchingLineNumber(line);
      long ixf;
      int gotoit = TRUE;
      int either = FALSE;
      if(_repsel || _repany)
          {
          either = TRUE;
          if(ixl == -1)
              {
              gotoit = FALSE;
              }
          else
              {
              ixf = _fg->findMatchingShotpointOnLine(ixl, shot);
              if(ixf == -1)
                  {
                  gotoit = FALSE;
                  }
              else if(_repsel && !_fg->lineIsSelected(ixl) &&
                                 !_fg->flagIsSelected(ixl, ixf))
                  {
                  gotoit = FALSE;
                  }
              }
          }
      else
          {
          if(ixl == -1)
              {
              if(SMART_ALLOC && ixl_keep >= 0)
                  {
                  _fg->freeSpaceForLine(ixl_keep);
                  nadd = _fg->numFlagsOnLine(ixl_keep);
                  }
              ixl = _fg->placeNewLine(line);
              int error = checkAppendStatus(ixl, "line for LD", i, nld, errmsg);
              if(error) return TRUE;
              if(SMART_ALLOC)
                  {
                  _fg->allocateSpaceForLine(ixl, nadd);
                  ixl_keep = ixl;
                  }
              }
          ixf = _fg->appendNewFlagToLine(ixl);
          int error = checkAppendStatus(ixf, "flag for LD", i, nld, errmsg);
          if(error) return TRUE;
          }
      if(gotoit)
          {
          if(shot  != FNIL || either) _fg->setShotpoint     (ixl, ixf, shot);
          if(_fg->allowSettingIncrDistance()
          && (dist != DNIL || either))_fg->setIncrDistance  (ixl, ixf, dist);
          if(_fg->allowSettingXloc()
          && (xloc != DNIL || either))_fg->setXloc          (ixl, ixf, xloc);
          if(yloc  != DNIL || either) _fg->setYloc          (ixl, ixf, yloc);
          if(elev  != FNIL || either) _fg->setElevation     (ixl, ixf, elev);
          if(hd    != FNIL || either) _fg->setHoleDepth     (ixl, ixf, hd);
          if(tuh   != FNIL || either) _fg->setUpholeTime    (ixl, ixf, tuh);
          if(rstat != FNIL || either) _fg->setReceiverStatic(ixl, ixf, rstat);
          if(sstat != FNIL || either) _fg->setSourceStatic  (ixl, ixf, sstat);
          if(xskid != FNIL || either) _fg->setReceiverXskid (ixl, ixf, xskid);
          if(yskid != FNIL || either) _fg->setReceiverYskid (ixl, ixf, yskid);
          if(eskid != FNIL || either) _fg->setReceiverEskid (ixl, ixf, eskid);
          }
      if(!either && SMART_ALLOC && i == nld-1) _fg->freeSpaceForLine(ixl);
      }
  return FALSE;
}



//--------------------- read RP cards --------------------------//
//--------------------- read RP cards --------------------------//
//--------------------- read RP cards --------------------------//


int JdFile::readRpCards(long nrp, char *errmsg)
{
  if(_rp_read_flag && _rp_clear_flag && _fg->numRpCards() > 0)
                                              _fg->deleteAllRpCards();
  if(nrp == 0) return FALSE;
  if(SMART_ALLOC && nrp > 0) _fg->allocateSpaceForRpCards(nrp);
  for(long i = 0; i < nrp || nrp < 0; i++)
      {
      int err;
      int pat, line, nx, xinc, ny, yinc;
      char buffer[55];
      float shot, xskid, yskid, eskid;

      _geomio[0]->readRpCard   (i, &err, errmsg,
                     &pat, buffer, &shot, &line, &nx, &xinc, &ny,
                     &yinc, &xskid, &yskid, &eskid);

      if(err == GeomioWrapper::STATUS_EOF && nrp < 0) break;
      if(err != GeomioWrapper::STATUS_OK) return TRUE;
      work_message(_fg, "reading RP (receiver pattern)", i, nrp);
      if(!_rp_read_flag) continue;

      int rpflag = getRpFlag(buffer, i, nrp, errmsg);
      if(rpflag == INIL) return TRUE;
      long ixrp = _fg->appendNewRpCard();
      int error = checkAppendStatus(ixrp, "RP", i, nrp, errmsg);
      if(error) return TRUE;
      _fg->setRpPatternNumber(ixrp, pat);
      _fg->setRpFlag         (ixrp, rpflag);
      _fg->setRpShotpoint    (ixrp, shot);
      _fg->setRpLineNumber   (ixrp, line);
      _fg->setRpNumX         (ixrp, nx);
      _fg->setRpXinc         (ixrp, xinc);
      _fg->setRpNumY         (ixrp, ny);
      _fg->setRpYinc         (ixrp, yinc);
      _fg->setRpXskid        (ixrp, xskid);
      _fg->setRpYskid        (ixrp, yskid);
      _fg->setRpEskid        (ixrp, eskid);
      }
  return FALSE;
}



//--------------------- read PP cards --------------------------//
//--------------------- read PP cards --------------------------//
//--------------------- read PP cards --------------------------//


int JdFile::readPpCards(long npp, char *errmsg)
{
  if(_pp_read_flag && _pp_clear_flag && _fg->numPpCards() > 0)
                                              _fg->deleteAllPpCards();
  if(npp == 0) return FALSE;
  if(SMART_ALLOC && npp > 0) _fg->allocateSpaceForPpCards(npp);
  long remember = 0;
  for(long i = 0; i < npp || npp < 0; i++)
      {
      int err;
      int sline, rline, pat, hold, smove, rmove, thru, ffile;
      float sshot, rshot, xskid, yskid, elev, hd, tuh;

      _geomio[0]->readPpCard   (i, &err, errmsg,
                     &sshot, &sline, &rshot, &rline, &pat,
                     &xskid, &yskid, &hold, &elev, &hd, &tuh,
                     &smove, &rmove, &thru);

      if(err == GeomioWrapper::STATUS_EOF && npp < 0) break;
      if(err != GeomioWrapper::STATUS_OK) return TRUE;
      work_message(_fg, "reading PP (profile pattern)", i, npp);
      if(!_pp_read_flag) continue;

      ffile = remember + 1;   // new 3/27/02.

      long ixpp = _fg->appendNewPpCard();
      int error = checkAppendStatus(ixpp, "PP", i, npp, errmsg);
      if(error) return TRUE;
      if(sshot != FNIL) _fg->setSourceShotpoint  (ixpp, sshot);
      if(sline != INIL) _fg->setSourceLine       (ixpp, sline);
      if(rshot != FNIL) _fg->setReceiverShotpoint(ixpp, rshot);
      if(rline != INIL) _fg->setReceiverLine     (ixpp, rline);
      if(pat   != INIL) _fg->setPatternNumber    (ixpp, pat);
      if(xskid != FNIL) _fg->setSourceXskid      (ixpp, xskid);
      if(yskid != FNIL) _fg->setSourceYskid      (ixpp, yskid);
      if(hold  != INIL) _fg->setSkidHold         (ixpp, hold);
      if(elev  != FNIL) _fg->setNewElevation     (ixpp, elev );
      if(hd    != FNIL) _fg->setNewHoleDepth     (ixpp, hd   );
      if(tuh   != FNIL) _fg->setNewUpholeTime    (ixpp, tuh  );
      if(smove != INIL) _fg->setSourceMove       (ixpp, smove);
      if(rmove != INIL) _fg->setReceiverMove     (ixpp, rmove);
      if(ffile != INIL) _fg->setFirstFileNumber  (ixpp, ffile);
      if(thru  == INIL) thru = remember + 1;
      long num = thru - remember;
      _fg->setNumGroupsOnCard(ixpp, num);
      remember = thru;
      }
  return FALSE;
}



//--------------------- read ZT1 cards --------------------------//
//--------------------- read ZT1 cards --------------------------//
//--------------------- read ZT1 cards --------------------------//


int JdFile::readZt1Cards(long nzt1, char *errmsg)
{
  if(_zt1_read_flag && _zt1_clear_flag && _fg->numZt1Cards() > 0)
                                              _fg->deleteAllZt1Cards();
  if(nzt1 == 0) return FALSE;
  if(SMART_ALLOC && nzt1 > 0) _fg->allocateSpaceForZt1Cards(nzt1);
  for(long i = 0; i < nzt1 || nzt1 < 0; i++)
      {
      int err;
      float from_shot, to_shot;
      int line;
      char buffer[55];

      _geomio[0]->readZt1Card   (i, &err, errmsg,
                                 buffer, &from_shot, &to_shot, &line);

      if(err == GeomioWrapper::STATUS_EOF && nzt1 < 0) break;
      if(err != GeomioWrapper::STATUS_OK) return TRUE;
      work_message(_fg, "reading ZT1 (zero sources)", i, nzt1);
      if(!_zt1_read_flag) continue;

      int ztcode = getZtCode(buffer, "ZT1", i, nzt1, errmsg);
      if(ztcode == INIL) return TRUE;
      long ixzt1 = _fg->appendNewZt1Card();
      int error = checkAppendStatus(ixzt1, "ZT1", i, nzt1, errmsg);
      if(error) return TRUE;
      _fg->setZt1Code                (ixzt1, ztcode);
      _fg->setZt1FromSourceShotpoint (ixzt1, from_shot);
      _fg->setZt1ToSourceShotpoint   (ixzt1, to_shot);
      _fg->setZt1SourceLineNumber    (ixzt1, line);
      }
  return FALSE;
}



//--------------------- read ZT2 cards --------------------------//
//--------------------- read ZT2 cards --------------------------//
//--------------------- read ZT2 cards --------------------------//


int JdFile::readZt2Cards(long nzt2, char *errmsg)
{
  if(_zt2_read_flag && _zt2_clear_flag && _fg->numZt2Cards() > 0)
                                              _fg->deleteAllZt2Cards();
  if(nzt2 == 0) return FALSE;
  if(SMART_ALLOC && nzt2 > 0) _fg->allocateSpaceForZt2Cards(nzt2);
  for(long i = 0; i < nzt2 || nzt2 < 0; i++)
      {
      int err;
      float from_shot, to_shot;
      int line;
      char buffer[55];

      _geomio[0]->readZt2Card   (i, &err, errmsg,
                                 buffer, &from_shot, &to_shot, &line);

      if(err == GeomioWrapper::STATUS_EOF && nzt2 < 0) break;
      if(err != GeomioWrapper::STATUS_OK) return TRUE;
      work_message(_fg, "reading ZT2 (zero receivers)", i, nzt2);
      if(!_zt2_read_flag) continue;

      int ztcode = getZtCode(buffer, "ZT2", i, nzt2, errmsg);
      if(ztcode == INIL) return TRUE;
      long ixzt2 = _fg->appendNewZt2Card();
      int error = checkAppendStatus(ixzt2, "ZT2", i, nzt2, errmsg);
      if(error) return TRUE;
      _fg->setZt2Code                  (ixzt2, ztcode);
      _fg->setZt2FromReceiverShotpoint (ixzt2, from_shot);
      _fg->setZt2ToReceiverShotpoint   (ixzt2, to_shot);
      _fg->setZt2ReceiverLineNumber    (ixzt2, line);
      }
  return FALSE;
}



//--------------------- read ZT3 cards --------------------------//
//--------------------- read ZT3 cards --------------------------//
//--------------------- read ZT3 cards --------------------------//


int JdFile::readZt3Cards(long nzt3, char *errmsg)
{
  if(_zt3_read_flag && _zt3_clear_flag && _fg->numZt3Cards() > 0)
                                              _fg->deleteAllZt3Cards();
  if(nzt3 == 0) return FALSE;
  if(SMART_ALLOC && nzt3 > 0) _fg->allocateSpaceForZt3Cards(nzt3);
  for(long i = 0; i < nzt3 || nzt3 < 0; i++)
      {
      int err;
      int from_group, to_group;
      int from_trace, to_trace;
      char buffer[55];

      _geomio[0]->readZt3Card   (i, &err, errmsg,
                                 buffer, &from_group, &to_group,
                                 &from_trace, &to_trace);

      if(err == GeomioWrapper::STATUS_EOF && nzt3 < 0) break;
      if(err != GeomioWrapper::STATUS_OK) return TRUE;
      work_message(_fg, "reading ZT3 (zero traces in groups)", i, nzt3);
      if(!_zt3_read_flag) continue;

      int ztcode = getZtCode(buffer, "ZT3", i, nzt3, errmsg);
      if(ztcode == INIL) return TRUE;
      long ixzt3 = _fg->appendNewZt3Card();
      int error = checkAppendStatus(ixzt3, "ZT3", i, nzt3, errmsg);
      if(error) return TRUE;
      _fg->setZt3Code                (ixzt3, ztcode);
      _fg->setZt3FromGroupNumber     (ixzt3, from_group);
      _fg->setZt3ToGroupNumber       (ixzt3, to_group);
      _fg->setZt3FromTraceNumber     (ixzt3, from_trace);
      _fg->setZt3ToTraceNumber       (ixzt3, to_trace);
      }
  return FALSE;
}



//--------------------- read ZT4 cards --------------------------//
//--------------------- read ZT4 cards --------------------------//
//--------------------- read ZT4 cards --------------------------//


int JdFile::readZt4Cards(long nzt4, char *errmsg)
{
  if(_zt4_read_flag && _zt4_clear_flag && _fg->numZt4Cards() > 0)
                                              _fg->deleteAllZt4Cards();
  if(nzt4 == 0) return FALSE;
  if(SMART_ALLOC && nzt4 > 0) _fg->allocateSpaceForZt4Cards(nzt4);
  for(long i = 0; i < nzt4 || nzt4 < 0; i++)
      {
      int err;
      float from_sshot, to_sshot;
      float from_rshot, to_rshot;
      int sline, rline;
      char buffer[55];

      _geomio[0]->readZt4Card   (i, &err, errmsg,
                                 buffer, &from_sshot, &to_sshot, &sline,
                                 &from_rshot, &to_rshot, &rline);

      if(err == GeomioWrapper::STATUS_EOF && nzt4 < 0) break;
      if(err != GeomioWrapper::STATUS_OK) return TRUE;
      work_message(_fg, "reading ZT4 (zero selected ranges)", i, nzt4);
      if(!_zt4_read_flag) continue;

      int ztcode = getZtCode(buffer, "ZT4", i, nzt4, errmsg);
      if(ztcode == INIL) return TRUE;
      long ixzt4 = _fg->appendNewZt4Card();
      int error = checkAppendStatus(ixzt4, "ZT4", i, nzt4, errmsg);
      if(error) return TRUE;
      _fg->setZt4Code                  (ixzt4, ztcode);
      _fg->setZt4FromSourceShotpoint   (ixzt4, from_sshot);
      _fg->setZt4ToSourceShotpoint     (ixzt4, to_sshot);
      _fg->setZt4SourceLineNumber      (ixzt4, sline);
      _fg->setZt4FromReceiverShotpoint (ixzt4, from_rshot);
      _fg->setZt4ToReceiverShotpoint   (ixzt4, to_rshot);
      _fg->setZt4ReceiverLineNumber    (ixzt4, rline);
      }
  return FALSE;
}




//-------------------- private write functions ----------------------//
//-------------------- private write functions ----------------------//
//-------------------- private write functions ----------------------//
//-------------------- private write functions ----------------------//
//-------------------- private write functions ----------------------//
//-------------------- private write functions ----------------------//
//-------------------- private write functions ----------------------//
//-------------------- private write functions ----------------------//
//-------------------- private write functions ----------------------//
//-------------------- private write functions ----------------------//
//-------------------- private write functions ----------------------//
//-------------------- private write functions ----------------------//
//-------------------- private write functions ----------------------//
//-------------------- private write functions ----------------------//
//-------------------- private write functions ----------------------//

                // these return FALSE if no error


//--------------------- save LD cards --------------------------//
//--------------------- save LD cards --------------------------//
//--------------------- save LD cards --------------------------//


int JdFile::saveLdCards()
{
  if(!_ld_save_flag)
      {
      _fg->showMessage("skipping LD cards");
      return FALSE;
      }
  long nlines = _fg->numLines();
  long nld = 0;
  long ixl;
  for(ixl = 0; ixl < nlines; ixl++)
      {
      nld += _fg->numFlagsOnLine(ixl);
      }
  if(nld == 0) return FALSE;

  long  kount = 0;
  for(ixl = 0; ixl < nlines; ixl++)
      {
      long line   = _fg->getLineNumber (ixl);
      long nflags = _fg->numFlagsOnLine(ixl);
      for(long ixf = 0; ixf < nflags; ixf++)
          {
          float  shot  = _fg->getShotpoint     (ixl, ixf);
          double dist  = _fg->getIncrDistance  (ixl, ixf);
          double xloc  = _fg->getXloc          (ixl, ixf);
          double yloc  = _fg->getYloc          (ixl, ixf);
          float  elev  = _fg->getElevation     (ixl, ixf);
          float  hd    = _fg->getHoleDepth     (ixl, ixf);
          float  tuh   = _fg->getUpholeTime    (ixl, ixf);
          float  rstat = _fg->getReceiverStatic(ixl, ixf);
          float  sstat = _fg->getSourceStatic  (ixl, ixf);
          float  xskid = _fg->getReceiverXskid (ixl, ixf);
          float  yskid = _fg->getReceiverYskid (ixl, ixf);
          float  eskid = _fg->getReceiverEskid (ixl, ixf);
  /////   if(_fg->flagValueIsDependent(ixl, ixf, FG_SHOT )) shot  = FNIL;
          if(_fg->flagValueIsDependent(ixl, ixf, FG_DIST )) dist  = DNIL;
          if(_fg->flagValueIsDependent(ixl, ixf, FG_XLOC )) xloc  = DNIL;
          if(_fg->flagValueIsDependent(ixl, ixf, FG_YLOC )) yloc  = DNIL;
          if(_fg->flagValueIsDependent(ixl, ixf, FG_ELEV )) elev  = FNIL;
          if(_fg->flagValueIsDependent(ixl, ixf, FG_HD   )) hd    = FNIL;
          if(_fg->flagValueIsDependent(ixl, ixf, FG_TUH  )) tuh   = FNIL;
          if(_fg->flagValueIsDependent(ixl, ixf, FG_RSTAT)) rstat = FNIL;
          if(_fg->flagValueIsDependent(ixl, ixf, FG_SSTAT)) sstat = FNIL;
          if(_fg->flagValueIsDependent(ixl, ixf, FG_XSKID)) xskid = FNIL;
          if(_fg->flagValueIsDependent(ixl, ixf, FG_YSKID)) yskid = FNIL;
          if(_fg->flagValueIsDependent(ixl, ixf, FG_ESKID)) eskid = FNIL;

          int err;
          char errmsg[99];
          _geomio[2]->writeLdCard   (kount, &err, errmsg,
                  shot, (float)dist, (float)xloc, (float)yloc, elev, hd, tuh,
                  rstat, sstat, xskid, yskid, eskid, line);
          if(err != GeomioWrapper::STATUS_OK) return TRUE;
          work_message(_fg, "saving LD (line description)", kount, nld);
          kount++;
          }
      }
  return FALSE;
}



//--------------------- save RP cards --------------------------//
//--------------------- save RP cards --------------------------//
//--------------------- save RP cards --------------------------//


int JdFile::saveRpCards()
{
  if(!_rp_save_flag)
      {
      _fg->showMessage("skipping RP cards");
      return FALSE;
      }
  long nrp = _fg->numRpCards();
  if(nrp == 0) return FALSE;
  for(long ixrp = 0; ixrp < nrp; ixrp++)
      {
      long  pat    = _fg->getRpPatternNumber(ixrp);
      int   rpflag = _fg->getRpFlag         (ixrp);
      float shot   = _fg->getRpShotpoint    (ixrp);
      long  line   = _fg->getRpLineNumber   (ixrp);
      long  nx     = _fg->getRpNumX         (ixrp);
      long  xinc   = _fg->getRpXinc         (ixrp);
      long  ny     = _fg->getRpNumY         (ixrp);
      long  yinc   = _fg->getRpYinc         (ixrp);
      float xskid  = _fg->getRpXskid        (ixrp);
      float yskid  = _fg->getRpYskid        (ixrp);
      float eskid  = _fg->getRpEskid        (ixrp);
      const char *rpstring = getRpFlagString(rpflag);
      if(!rpstring) return TRUE;

      int err;
      char errmsg[99];
      _geomio[2]->writeRpCard   (ixrp, &err, errmsg,
                  pat, rpstring, shot, line, nx, xinc, ny,
                  yinc, xskid, yskid, eskid);
      if(err != GeomioWrapper::STATUS_OK) return TRUE;
      work_message(_fg, "saving RP (receiver pattern)", ixrp, nrp);
      }
  return FALSE;
}



//--------------------- save PP cards --------------------------//
//--------------------- save PP cards --------------------------//
//--------------------- save PP cards --------------------------//


int JdFile::savePpCards()
{
  if(!_pp_save_flag)
      {
      _fg->showMessage("skipping PP cards");
      return FALSE;
      }
  long npp = _fg->numPpCards();
  if(npp == 0) return FALSE;
  for(long ixpp = 0; ixpp < npp; ixpp++)
      {
      float  sshot  = _fg->getSourceShotpoint  (ixpp);
      long   sline  = _fg->getSourceLine       (ixpp);
      float  rshot  = _fg->getReceiverShotpoint(ixpp);
      long   rline  = _fg->getReceiverLine     (ixpp);
      long   pat    = _fg->getPatternNumber    (ixpp);
      float  xskid  = _fg->getSourceXskid      (ixpp);
      float  yskid  = _fg->getSourceYskid      (ixpp);
      long   hold   = _fg->getSkidHold         (ixpp);
      float  elev   = _fg->getNewElevation     (ixpp);
      float  hd     = _fg->getNewHoleDepth     (ixpp);
      float  tuh    = _fg->getNewUpholeTime    (ixpp);
      long   smove  = _fg->getSourceMove       (ixpp);
      long   rmove  = _fg->getReceiverMove     (ixpp);
      long   thru   = _fg->getThruGroupNumber  (ixpp);
      long   ffile  = _fg->getFirstFileNumber  (ixpp);
      if(_fg->ppValueIsDependent(ixpp, PP_SSHOT  )) sshot = FNIL;
      if(_fg->ppValueIsDependent(ixpp, PP_SLINE  )) sline = INIL;
      if(_fg->ppValueIsDependent(ixpp, PP_RSHOT  )) rshot = FNIL;
      if(_fg->ppValueIsDependent(ixpp, PP_RLINE  )) rline = INIL;
      if(_fg->ppValueIsDependent(ixpp, PP_PAT    )) pat   = INIL;
      if(_fg->ppValueIsDependent(ixpp, PP_XSKID  )) xskid = FNIL;
      if(_fg->ppValueIsDependent(ixpp, PP_YSKID  )) yskid = FNIL;
      if(_fg->ppValueIsDependent(ixpp, PP_HOLD   )) hold  = INIL;
      if(_fg->ppValueIsDependent(ixpp, PP_ELEV   )) elev  = FNIL;
      if(_fg->ppValueIsDependent(ixpp, PP_HD     )) hd    = FNIL;
      if(_fg->ppValueIsDependent(ixpp, PP_TUH    )) tuh   = FNIL;
      if(_fg->ppValueIsDependent(ixpp, PP_SMOVE  )) smove = INIL;
      if(_fg->ppValueIsDependent(ixpp, PP_RMOVE  )) rmove = INIL;
 //// if(_fg->ppValueIsDependent(ixpp, PP_THRU_GR)) thru  = INIL;
      if(_fg->ppValueIsDependent(ixpp, PP_FILE   )) ffile = INIL;

      int err;
      char errmsg[99];
      _geomio[2]->writePpCard   (ixpp, &err, errmsg,
                  sshot, sline, rshot, rline, pat, xskid, yskid,
                  hold, elev, hd, tuh, smove, rmove, thru);
      if(err != GeomioWrapper::STATUS_OK) return TRUE;
      work_message(_fg, "saving PP (profile pattern)", ixpp, npp);
      }
  return FALSE;
}



//--------------------- save ZT1 cards --------------------------//
//--------------------- save ZT1 cards --------------------------//
//--------------------- save ZT1 cards --------------------------//


int JdFile::saveZt1Cards()
{
  if(!_zt1_save_flag)
      {
      _fg->showMessage("skipping ZT1 cards");
      return FALSE;
      }
  long nzt1 = _fg->numZt1Cards();
  if(nzt1 == 0) return FALSE;
  for(long ixzt1 = 0; ixzt1 < nzt1; ixzt1++)
      {
      int   ztcode    = _fg->getZt1Code               (ixzt1);
      float from_shot = _fg->getZt1FromSourceShotpoint(ixzt1);
      float to_shot   = _fg->getZt1ToSourceShotpoint  (ixzt1);
      long  line      = _fg->getZt1SourceLineNumber   (ixzt1);
      const char *ztstring = getZtCodeString(ztcode);
      if(!ztstring) return TRUE;

      int err;
      char errmsg[99];
      _geomio[2]->writeZt1Card   (ixzt1, &err, errmsg,
                  ztstring, from_shot, to_shot, line);
      if(err != GeomioWrapper::STATUS_OK) return TRUE;
      work_message(_fg, "saving ZT1 (zero sources)", ixzt1, nzt1);
      }
  return FALSE;
}



//--------------------- save ZT2 cards --------------------------//
//--------------------- save ZT2 cards --------------------------//
//--------------------- save ZT2 cards --------------------------//


int JdFile::saveZt2Cards()
{
  if(!_zt2_save_flag)
      {
      _fg->showMessage("skipping ZT2 cards");
      return FALSE;
      }
  long nzt2 = _fg->numZt2Cards();
  if(nzt2 == 0) return FALSE;
  for(long ixzt2 = 0; ixzt2 < nzt2; ixzt2++)
      {
      int   ztcode    = _fg->getZt2Code                 (ixzt2);
      float from_shot = _fg->getZt2FromReceiverShotpoint(ixzt2);
      float to_shot   = _fg->getZt2ToReceiverShotpoint  (ixzt2);
      long  line      = _fg->getZt2ReceiverLineNumber   (ixzt2);
      const char *ztstring = getZtCodeString(ztcode);
      if(!ztstring) return TRUE;

      int err;
      char errmsg[99];
      _geomio[2]->writeZt2Card   (ixzt2, &err, errmsg,
                  ztstring, from_shot, to_shot, line);
      if(err != GeomioWrapper::STATUS_OK) return TRUE;
      work_message(_fg, "saving ZT2 (zero receivers)", ixzt2, nzt2);
      }
  return FALSE;
}



//--------------------- save ZT3 cards --------------------------//
//--------------------- save ZT3 cards --------------------------//
//--------------------- save ZT3 cards --------------------------//


int JdFile::saveZt3Cards()
{
  if(!_zt3_save_flag)
      {
      _fg->showMessage("skipping ZT3 cards");
      return FALSE;
      }
  long nzt3 = _fg->numZt3Cards();
  if(nzt3 == 0) return FALSE;
  for(long ixzt3 = 0; ixzt3 < nzt3; ixzt3++)
      {
      int   ztcode     = _fg->getZt3Code            (ixzt3);
      long  from_group = _fg->getZt3FromGroupNumber (ixzt3);
      long  to_group   = _fg->getZt3ToGroupNumber   (ixzt3);
      long  from_trace = _fg->getZt3FromTraceNumber (ixzt3);
      long  to_trace   = _fg->getZt3ToTraceNumber   (ixzt3);
      const char *ztstring = getZtCodeString(ztcode);
      if(!ztstring) return TRUE;

      int err;
      char errmsg[99];
      _geomio[2]->writeZt3Card   (ixzt3, &err, errmsg,
                  ztstring, from_group, to_group, from_trace, to_trace);
      if(err != GeomioWrapper::STATUS_OK) return TRUE;
      work_message(_fg, "saving ZT3 (zero traces in groups)", ixzt3, nzt3);
      }
  return FALSE;
}



//--------------------- save ZT4 cards --------------------------//
//--------------------- save ZT4 cards --------------------------//
//--------------------- save ZT4 cards --------------------------//


int JdFile::saveZt4Cards()
{
  if(!_zt4_save_flag)
      {
      _fg->showMessage("skipping ZT4 cards");
      return FALSE;
      }
  long nzt4 = _fg->numZt4Cards();
  if(nzt4 == 0) return FALSE;
  for(long ixzt4 = 0; ixzt4 < nzt4; ixzt4++)
      {
      int   ztcode     = _fg->getZt4Code                  (ixzt4);
      float from_sshot = _fg->getZt4FromSourceShotpoint   (ixzt4);
      float to_sshot   = _fg->getZt4ToSourceShotpoint     (ixzt4);
      long  sline      = _fg->getZt4SourceLineNumber      (ixzt4);
      float from_rshot = _fg->getZt4FromReceiverShotpoint (ixzt4);
      float to_rshot   = _fg->getZt4ToReceiverShotpoint   (ixzt4);
      long  rline      = _fg->getZt4ReceiverLineNumber    (ixzt4);
      const char *ztstring = getZtCodeString(ztcode);
      if(!ztstring) return TRUE;

      int err;
      char errmsg[99];
      _geomio[2]->writeZt4Card   (ixzt4, &err, errmsg,
                  ztstring, from_sshot, to_sshot, sline,
                            from_rshot, to_rshot, rline);
      if(err != GeomioWrapper::STATUS_OK) return TRUE;
      work_message(_fg, "saving ZT4 (zero selected ranges)", ixzt4, nzt4);
      }
  return FALSE;
}



//-------------------------- skip read ------------------------------------//
//-------------------------- skip read ------------------------------------//
//-------------------------- skip read ------------------------------------//


void JdFile::skipRead()
{
  _geomio[0]->skipLdCards (! _ld_read_flag);
  _geomio[0]->skipRpCards (! _rp_read_flag);
  _geomio[0]->skipPpCards (! _pp_read_flag);
  _geomio[0]->skipZt1Cards(!_zt1_read_flag);
  _geomio[0]->skipZt2Cards(!_zt2_read_flag);
  _geomio[0]->skipZt3Cards(!_zt3_read_flag);
  _geomio[0]->skipZt4Cards(!_zt4_read_flag);
}



//-------------------------- set values ----------------------------------//
//-------------------------- set values ----------------------------------//
//-------------------------- set values ----------------------------------//


void   JdFile::setInputVe          (float ve)
{
  _geomio[0]->setScalarFloat("ve", ve);
}


void   JdFile::setInputRef          (float ref)
{
  _geomio[0]->setScalarFloat("datum", ref);
}


void   JdFile::setInputFixdist          (float fixdist)
{
  _geomio[0]->setScalarFloat("fixdist", fixdist);
}


void   JdFile::setInputChaining          (int chaining)
{
  const char *buffer = getChainingString(chaining);
  _geomio[0]->setScalarString("chaining", buffer);
}



//-------------------------- get values ----------------------------------//
//-------------------------- get values ----------------------------------//
//-------------------------- get values ----------------------------------//


float  JdFile::getInputVe          ()  const  
{
  return _geomio[0]->getScalarFloat("ve");
}


float  JdFile::getInputRef         ()  const  
{
  return _geomio[0]->getScalarFloat("datum");
}


float  JdFile::getInputFixdist          ()  const  
{
  return _geomio[0]->getScalarFloat("fixdist");
}


int    JdFile::getInputChaining          ()  const  
{
  char info[NBUF];
  const char *buffer = _geomio[0]->getScalarString("chaining");
  return getChaining(buffer, info);
}

                                  ////////////////


long   JdFile::numInputLdCards           ()  const  
{
  int value = _geomio[0]->getScalarInteger("nld");
  if(value == INIL) value = 0;
  return value;
}


long   JdFile::numInputRpCards           ()  const  
{
  int value = _geomio[0]->getScalarInteger("nrp");
  if(value == INIL) value = 0;
  return value;
}


long   JdFile::numInputPpCards           ()  const  
{
  int value = _geomio[0]->getScalarInteger("npp");
  if(value == INIL) value = 0;
  return value;
}


long   JdFile::numInputZt1Cards          ()  const  
{
  int value = _geomio[0]->getScalarInteger("nzt1");
  if(value == INIL) value = 0;
  return value;
}


long   JdFile::numInputZt2Cards          ()  const  
{
  int value = _geomio[0]->getScalarInteger("nzt2");
  if(value == INIL) value = 0;
  return value;
}


long   JdFile::numInputZt3Cards          ()  const  
{
  int value = _geomio[0]->getScalarInteger("nzt3");
  if(value == INIL) value = 0;
  return value;
}


long   JdFile::numInputZt4Cards          ()  const  
{
  int value = _geomio[0]->getScalarInteger("nzt4");
  if(value == INIL) value = 0;
  return value;
}


int    JdFile::inputGridTransformStatus  ()  const  
{
  if(_geomio[0]->keywordPresent("grid")) return 1;
  return -1;
}

                         ////////////////////


int    JdFile::getOutputChaining         ()  const  
{
  char info[NBUF];
  const char *buffer = _geomio[1]->getScalarString("chaining");
  return getChaining(buffer, info);
}

                         ////////////////////


long   JdFile::numOutputLdCards          ()  const  
{
  int value = _geomio[1]->getScalarInteger("nld");
  if(value == INIL) value = 0;
  return value;
}


long   JdFile::numOutputRpCards          ()  const  
{
  int value = _geomio[1]->getScalarInteger("nrp");
  if(value == INIL) value = 0;
  return value;
}


long   JdFile::numOutputPpCards          ()  const  
{
  int value = _geomio[1]->getScalarInteger("npp");
  if(value == INIL) value = 0;
  return value;
}


long   JdFile::numOutputZt1Cards         ()  const  
{
  int value = _geomio[1]->getScalarInteger("nzt1");
  if(value == INIL) value = 0;
  return value;
}


long   JdFile::numOutputZt2Cards         ()  const  
{
  int value = _geomio[1]->getScalarInteger("nzt2");
  if(value == INIL) value = 0;
  return value;
}


long   JdFile::numOutputZt3Cards         ()  const  
{
  int value = _geomio[1]->getScalarInteger("nzt3");
  if(value == INIL) value = 0;
  return value;
}


long   JdFile::numOutputZt4Cards         ()  const  
{
  int value = _geomio[1]->getScalarInteger("nzt4");
  if(value == INIL) value = 0;
  return value;
}


int    JdFile::outputGridTransformStatus ()  const  
{
  if(_geomio[1]->keywordPresent("grid")) return 1;
  return -1;
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
