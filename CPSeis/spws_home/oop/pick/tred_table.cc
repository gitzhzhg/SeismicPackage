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
//------------------- tred_table.cc ----------------------//
//         implementation file for the TredTable class
//                 derived from the SLDatabox class

#include <stdio.h>
#include <string.h>
#include <iostream.h>
#include <X11/Xlib.h>
#include "sp/seis_plot.hh"
#include "vect/ll_seis_vect.hh"
#include "sl/prim_support.hh"
#include "pick/custom_resources.hh"
#include "pick/tred_table.hh"
#include "sl/sl_error_pop.hh"
#include "wbox.h"
#include "wproc.h"
#include "cprim.h"

#define MARKER_LENGTH 11
#define MARKER_OFFSET 6
#define DEFAULT_DO    "DO  "
#define DEFAULT_CODE  "KILL"

#define TF_MAXIMUM "TRace EDit (TRED) file is full.\nFurther selections will be ignored"

//------------------- constructor -----------------//
//------------------- constructor -----------------//
//------------------- constructor -----------------//

TredTable::TredTable (SLDelay *slparent, char *name, TredTablePop *pop)
        : SLDatabox (slparent, name, this),
     _vect_ll (0),
     _nmax  (TRED_TABLE_NMAX),
     _size  (TRED_CODES_SIZE),
     _pop   (pop)
{
//cout << "TredTable SLDelay constructor start" << endl;
  CustomResources::startup (slparent);
  constructorHelper ();
//cout << "TredTable SLDelay constructor end" << endl;
}

TredTable::TredTable (Widget parent, char *name, TredTablePop *pop)
        : SLDatabox (parent, name, this),
     _vect_ll (0),
     _nmax  (TRED_TABLE_NMAX),
     _size  (TRED_CODES_SIZE),
     _pop   (pop)
{
//cout << "TredTable Widget constructor start" << endl;
  CustomResources::startup (parent);
  constructorHelper ();
//cout << "TredTable Widget constructor end" << endl;
}

void TredTable::constructorHelper()
{
  _tf = tredfile_create (); // intialize _tf
  fileClear ();  // initialize _ivara[], _doptr[], _codptr[]
}

//------------------- destructor -----------------//
//------------------- destructor -----------------//
//------------------- destructor -----------------//

TredTable::~TredTable(void)
{
  _tf = tredfile_destroy (_tf);
  deleteVectors ();
}

void TredTable::doInitRowAndPost (char *next_do, long *idptr, long idxm1)
{
  memcpy(_tf->_dos[idxm1], next_do, (size_t)_size);
  initRowAndPost (idptr, idxm1+1, verifyOne (idxm1) || verifyTwo (idxm1) ||
                  verifyThree (idxm1));
}

void TredTable::codeInitRowAndPost (char *next_code, long *idptr, long idxm1)
{
  memcpy(_tf->_codes[idxm1], next_code, (size_t)_size);
  initRowAndPost (idptr, idxm1+1, verifyOne (idxm1) || verifyTwo (idxm1) ||
                  verifyThree (idxm1));
}

void TredTable::initRowAndPost (long *idptr, long index, Bool post_ok)
{
  if (index > _tf->_nrecs)
  {
// A new row has been added, establish defaults
    _ivara[_tf->_nrecs] = 10 * _tf->_nrecs;

    if (*idptr != DO && _tf->_nrecs > 0)
      memcpy (_tf->_dos[_tf->_nrecs], _tf->_dos[_tf->_nrecs-1],
        (size_t)_size);
    else if (*idptr != DO)
      memcpy (_tf->_dos[_tf->_nrecs], DEFAULT_DO, (size_t)_size);

    if (*idptr != CODE && _tf->_nrecs > 0)
      memcpy (_tf->_codes[_tf->_nrecs], _tf->_codes[_tf->_nrecs-1],
        (size_t)_size);
    else if (*idptr != CODE)
      memcpy (_tf->_codes[_tf->_nrecs], DEFAULT_CODE, (size_t)_size);

    if (*idptr != HDRWRD1 ) _tf->_hdr_wrd_1 [_tf->_nrecs] = 0 ;
    if (*idptr != STRTVLU1) _tf->_strt_vlu_1[_tf->_nrecs] = 0.;
    if (*idptr != ENDVLU1 ) _tf->_end_vlu_1 [_tf->_nrecs] = 0.;
    if (*idptr != HDRWRD2 ) _tf->_hdr_wrd_2 [_tf->_nrecs] = 0 ;
    if (*idptr != STRTVLU2) _tf->_strt_vlu_2[_tf->_nrecs] = 0.;
    if (*idptr != ENDVLU1 ) _tf->_end_vlu_2 [_tf->_nrecs] = 0.;
    if (*idptr != HDRWRD3 ) _tf->_hdr_wrd_3 [_tf->_nrecs] = 0 ;
    if (*idptr != STRTVLU3) _tf->_strt_vlu_3[_tf->_nrecs] = 0.;
    if (*idptr != ENDVLU3 ) _tf->_end_vlu_3 [_tf->_nrecs] = 0.;
    _tf->_nrecs++;
  }
  else if (post_ok)
// An existing row has been modified and posting is permitted
    PrimSupport::updateEverything(); // This needs to update the vectors...
    doFilePut (); //need to update vectors and file... open to better ideas
}

void TredTable::doFilePut (void)
{
  _pop->filePut ();
}

//------------------------ get TRED file pointer ---------------------------//

TredFile *TredTable::getTredFile()
{
  return _tf;
}

SeisVectLinkedList *TredTable::createVectors (SeisPlot *sp)
{
  long k2;
  static const char *_named_color[TRED_CODES_NVAR] =
    {NULL, NULL, NULL, NULL};

  if(_named_color[0] == NULL)
     {
     _named_color[0] = CustomResources::getColorNominalRed   ();
     _named_color[1] = CustomResources::getColorNominalBlue  ();
     _named_color[2] = CustomResources::getColorNominalGreen ();
     _named_color[3] = CustomResources::getColorNominalOrange();
     }
/*
      The following changed to the above by Tom Stoeckley 12/22/94 for HP
  static const char *_named_color[TRED_CODES_NVAR] =
    { CustomResources::getColorNominalRed   (),
      CustomResources::getColorNominalBlue  (),
      CustomResources::getColorNominalGreen (),
      CustomResources::getColorNominalOrange() };
*/

//cout << "TredVector constructor start" << endl;
//cout << "now create the data users" << endl;

  _vect_ll = new SeisVectLinkedList();
  _vect_ll->addPlot(sp);

// Set up the vectors to display the diamond markers
  for (k2 = 0; k2 < TRED_CODES_NVAR; k2++)
    {
//    cout << "      now make base data and trace edit data users "
//         << k2 << endl;
      _numpoints[k2] = 0; // initialize
      _xdata[k2] = 0; // initialize (NULL)
      _ydata[k2] = MARKER_OFFSET + MARKER_LENGTH * k2; // initialize
      _tred_data[k2] = new TredData ((int)_numpoints[k2], _xdata[k2],
          _ydata[k2]);
      _V[k2] = _vect_ll->add(_tred_data[k2], _named_color[k2], 2, False,
         Vector::NoLine, Vector::DiamondMarker, MARKER_LENGTH, 2,
         0, "fixed");
   /* _V[k2]->setXYOffsets (FALSE, TRUE); // use pixels in Y-direc. */
      // use pixels in Y-direc.
      _V[k2]->setXYOffsets (Vector::IsNotOffset, Vector::IsOffset);
      _V[k2]->makeVisible();
    }

// Set up the vectors to display the solid vertical line markers
  for (k2 = TRED_CODES_NVAR; k2 < 2*TRED_CODES_NVAR; k2++)
    {
      _numpoints[k2] = 0; // initialize
      _xdata[k2] = 0; // initialize (NULL)
      _ydata[k2] = MARKER_OFFSET + MARKER_LENGTH * k2 + 5000; // initialize
      _tred_data[k2] = new TredData ((int)_numpoints[k2], _xdata[k2],
          _ydata[k2]);
      _V[k2] = _vect_ll->add(_tred_data[k2], _named_color[k2%4], 1, False,
         Vector::NoLine, Vector::VerticalLineMarker, 10000, 1, 0, "fixed");
      // use pixels in Y-direc.
      _V[k2]->setXYOffsets (Vector::IsNotOffset, Vector::IsOffset);
      _V[k2]->makeVisible();
    }
//cout << "created the data users" << endl;
  return _vect_ll;
}

void TredTable::post (SeisPlot *sp)
{
  long start_trace, end_trace, numWordsPerHeader, k2, k3, image_height;
  unsigned int marker_size;
  static long data_count [TRED_VECT_COUNT] = {0};
  static long disp_tr[TRED_CODES_NVAR] = {0};

//cout << "Tred_Pick post" << endl;

  if (!sp->imageIsDisplayed() || !_vect_ll) return;

// Determine the starting and ending trace currently displayed
  start_trace       = 0;                 // initialize
  end_trace         = sp->displayedTraces(sp->currentFrame());// initialize
  numWordsPerHeader = sp->numHeaders();  // initialize
  image_height      = sp->imageHeight(); // initialize

// allocate x and y data arrays for each TRED code (twice)
  for (k2 = 0; k2 < TRED_VECT_COUNT; k2++)
    {
      if (_xdata[k2] == 0)
        {
          _xdata[k2] =
            (float*) calloc (1,
                 (unsigned int)
                    (sp->displayedTraces(sp->currentFrame())*sizeof(float)));
// record how big the arrays are now
          data_count[k2] = sp->displayedTraces(sp->currentFrame());
        }

      else if (data_count[k2] != sp->displayedTraces(sp->currentFrame()))
        {
          _xdata[k2] =
            (float *) realloc (_xdata[k2],
                   (unsigned int)
                      (sp->displayedTraces(sp->currentFrame())*sizeof(float)));
// record how big the arrays are now
          data_count[k2] = sp->displayedTraces(sp->currentFrame());
        }

// initialize the number of points to post      
      _numpoints[k2] = 0;

    }

// Set up the the solid vertical line markers so as not to exceed the 
// trace lengths
  for (k2 = TRED_CODES_NVAR; k2 < 2*TRED_CODES_NVAR; k2++)
    {
      marker_size = (image_height > MARKER_OFFSET + MARKER_LENGTH * (k2%4)) ?
        (unsigned int)(image_height - (MARKER_OFFSET + MARKER_LENGTH
        * (k2%4))) : 0;
      _ydata[k2] = MARKER_OFFSET + MARKER_LENGTH * (k2%4) + marker_size/2;
      _tred_data[k2]->setYConst (_ydata[k2]);
      _V[k2]->setMarker (Vector::VerticalLineMarker,
        (unsigned int) marker_size, 1);
    }

// Examine every trace and build up the x data vectors for posting
//   on the seismic plot
  for (k2 = start_trace; k2 < end_trace; k2++)
    {

// Take each trace through the TRED table entries to see how (if at all)
// to post it
      asgn_code_to_hdr_spws (
        sp->firstMemoryHeaderData()+k2*numWordsPerHeader,
        &numWordsPerHeader,
        &_tf->_nrecs, _doptr, _codptr,
        _tf->_hdr_wrd_1, _tf->_strt_vlu_1, _tf->_end_vlu_1,
        _tf->_hdr_wrd_2, _tf->_strt_vlu_2, _tf->_end_vlu_2,
        _tf->_hdr_wrd_3, _tf->_strt_vlu_3, _tf->_end_vlu_3,
        &disp_tr[0], &disp_tr[1], &disp_tr[2], &disp_tr[3]);
      
      for (k3 = 0; k3 < TRED_CODES_NVAR; k3++)
        { 
          if (disp_tr[k3])
	    {
              _numpoints[k3]++;
              _xdata[k3][_numpoints[k3]-1] = k2+1; // k2 is 0-rel.
	    }
	}
// Only allow one colored vertical line to show which indicates the
// edit operation that actually takes place.  Note:  FLAG is somewhat
// different because it can occur coincidently with KILL and REV.
// The ambiguity is resolved by ignoring FLAG unless it is the only
// indication made to the trace.
      if (disp_tr[0]) // Delete the trace
	{
          _numpoints[4]++;
          _xdata[4][_numpoints[4]-1] = k2+1; // k2 is 0-rel.
	}
      else if (disp_tr[1]) // Kill (or zero) the trace
	{
          _numpoints[5]++;
          _xdata[5][_numpoints[5]-1] = k2+1; // k2 is 0-rel.
	}
      else if (disp_tr[2]) // Reverse the polarity of the trace
	{
          _numpoints[6]++;
          _xdata[6][_numpoints[6]-1] = k2+1; // k2 is 0-rel.
	}
      else if (disp_tr[3]) // Flag the trace
	{
          _numpoints[7]++;
          _xdata[7][_numpoints[7]-1] = k2+1; // k2 is 0-rel.
	}
    }   
// Replace the x data arrays for each TRED code (twice)
  for (k2 = 0; k2 < TRED_VECT_COUNT; k2++)
    _tred_data[k2]->replace ((int)_numpoints[k2], _xdata[k2]);
}

void TredTable::deleteVectors (void)
{
  long k2;

  if (_vect_ll)
    {
      delete _vect_ll;
      for (k2 = 0; k2 < TRED_VECT_COUNT; k2++)
        {
          free (_xdata[k2]);
          _xdata[k2] = 0;
          delete _tred_data[k2];
          _tred_data[k2] = 0;
        }
      _vect_ll = 0;
    }
}

// 

void TredTable::updatePick (SeisPlot *sp, const Bool undo, const char *code,
  const Bool use_hwd1, const long hwd1, const float min1, const float max1,
  const Bool use_hwd2, const long hwd2, const float min2, const float max2,
  const Bool use_hwd3, const long hwd3, const float min3, const float max3)
{
  if (_tf->_nrecs > (long)(_nmax-1)) {
    _errpop = new SLErrorPop (_pop, "Warning!!!", TF_MAXIMUM);
    return;
  }

  if (undo)
    memcpy (_tf->_dos[_tf->_nrecs], "UNDO", (size_t)_size);
  else
    memcpy (_tf->_dos[_tf->_nrecs], "DO  ", (size_t)_size);
    
  memcpy (_tf->_codes[_tf->_nrecs], code,   (size_t)_size);

  if (use_hwd1)
    {
      _tf->_hdr_wrd_1 [_tf->_nrecs] = hwd1;
      _tf->_strt_vlu_1[_tf->_nrecs] = min1;
      _tf->_end_vlu_1 [_tf->_nrecs] = max1;
    }

  if (use_hwd2)
    {
      _tf->_hdr_wrd_2 [_tf->_nrecs] = hwd2;
      _tf->_strt_vlu_2[_tf->_nrecs] = min2;
      _tf->_end_vlu_2 [_tf->_nrecs] = max2;
    }

  if (use_hwd3)
    {
      _tf->_hdr_wrd_3 [_tf->_nrecs] = hwd3;
      _tf->_strt_vlu_3[_tf->_nrecs] = min3;
      _tf->_end_vlu_3 [_tf->_nrecs] = max3;
    }
 
  _tf->_nrecs++;
  PrimSupport::updateEverything();
  post (sp);
}
                           
void TredTable::findPickMinMax (SeisPlot *sp, const long first_trace,
  const long last_trace, const long header_word, float *min, float *max)
{
  long numWordsPerHeader, k2  /* , k3  not used */;
  float *ptr;

  if (!sp->imageIsDisplayed()) return;

// Find the range of header word values given the range of traces and the
// header word index
    numWordsPerHeader = sp->numHeaders();

    ptr = (float *) sp->firstMemoryHeaderData() + (first_trace - 1)
    * numWordsPerHeader + (int) header_word - 1;
    *min = *ptr;
    *max = *ptr;
    for (k2 = first_trace; k2 < last_trace; k2++)
      {
        ptr = (float *) sp->firstMemoryHeaderData() + k2 * numWordsPerHeader
          + (int) header_word - 1;
        if (*ptr <= *min) *min = *ptr;
        if (*ptr >= *max) *max = *ptr;
      }
}

Bool TredTable::verifyOne (long idxm1)
{
  return _tf->_strt_vlu_1[idxm1] <= _tf->_end_vlu_1[idxm1] &&
         _tf->_hdr_wrd_1[idxm1] >= 0 && _tf->_hdr_wrd_1[idxm1] < 65;
}

Bool TredTable::verifyTwo (long idxm1)
{
  return _tf->_strt_vlu_2[idxm1] <= _tf->_end_vlu_2[idxm1] &&
         _tf->_hdr_wrd_2[idxm1] >= 0 && _tf->_hdr_wrd_2[idxm1] < 65;
}

Bool TredTable::verifyThree (long idxm1)
{
  return _tf->_strt_vlu_3[idxm1] <= _tf->_end_vlu_3[idxm1] &&
         _tf->_hdr_wrd_3[idxm1] >= 0 && _tf->_hdr_wrd_3[idxm1] < 65;
}

//--------------------------- traps ----------------------------//
//--------------------------- traps ----------------------------//

#define ARGUMENTS   void *box, long *ident, long *index,   \
                    char * /*text*/, long * /*nread*/, char *endkey

#define ARGUMENTS2  void *box, long *ident, long *index,   \
                    char * /*text*/, long *nread, char *endkey

// trap for change of do/undo directive.
void TredTable::trapDo (ARGUMENTS)
{ 
  if (*ident == DO)
  {

//Only by entering RETURN, REMOVE, or INSERT will the method function
    if (strcmp ("RETURN", endkey) && strcmp ("REMOVED", endkey)
      && strcmp ("INSERTED", endkey)) return;

// Get the pointer to the Databox
    TredTable *data = (TredTable*)SLDatabox::getUserData(box);

//After entering REMOVE do the data posting
  if (!strcmp ("REMOVED", endkey))
    {
      data->doFilePut ();
      return;
    }

// for a index that is too large, do nothing and send warning
    if (*index > data->_nmax) {
      data->_errpop = new SLErrorPop (data->_pop, "Warning!!!", TF_MAXIMUM);
      return;
    }

// Cycle through the do's with each button release or <return> key press.
    switch( data->_tf->_dos[*index-1][0])
    {
      case 'D':
        data->doInitRowAndPost ("UNDO", ident, *index-1);
        break;
      case 'U':
        data->doInitRowAndPost ("DO  ", ident, *index-1);
        break;
      default:
        data->doInitRowAndPost (DEFAULT_DO, ident, *index-1);
        break;
    }
  }
}

// trap for change of code value.
void TredTable::trapCode (ARGUMENTS)
{ 
  if (*ident == CODE)
  { 

//Only by entering RETURN or REMOVE will the method function
    if (strcmp ("RETURN", endkey) && strcmp ("REMOVED", endkey)
      && strcmp ("INSERTED", endkey)) return;

// Get the pointer to the Databox
    TredTable *data = (TredTable*)SLDatabox::getUserData(box);

//After entering REMOVE do the data posting
  if (!strcmp ("REMOVED", endkey))
    {
      data->doFilePut ();
      return;
    }

// for a index that is too large, do nothing and send warning
    if (*index > data->_nmax) {
      data->_errpop = new SLErrorPop (data->_pop, "Warning!!!", TF_MAXIMUM);
      return;
    }

// Cycle through the codes with each button release or <return> key press.
    switch( data->_tf->_codes[*index-1][0])
    {
      case 'D':
        data->codeInitRowAndPost ("KILL", ident, *index-1);
        break;
      case 'K':
        data->codeInitRowAndPost ("REV ", ident, *index-1);
        break;
      case 'R':
        data->codeInitRowAndPost ("FLAG", ident, *index-1);
        break;
      case 'F':
        data->codeInitRowAndPost ("DEL ", ident, *index-1);
        break;
      default:
        data->codeInitRowAndPost (DEFAULT_CODE, ident, *index-1);
        break;
    }
  }
}

// trap for change of header word.
void TredTable::trapHdr (ARGUMENTS2)
{

//Only a modified header word or by entering REMOVE or INSERT will the
//  function operate
  if (*nread <= 0 && strcmp ("REMOVED", endkey)
    && strcmp ("INSERTED", endkey)) return;

// Get the pointer to the Databox
    TredTable *data = (TredTable *)SLDatabox::getUserData(box);

//After entering REMOVE do the data posting
  if (!strcmp ("REMOVED", endkey))
    {
      data->doFilePut ();
      return;
    }

// for a index that is too large, do nothing and send warning
    if (*index > data->_nmax) {
      data->_errpop = new SLErrorPop (data->_pop, "Warning!!!", TF_MAXIMUM);
      return;
    }

// Only permit header words from 2 

  switch (*ident)
  {
    case (HDRWRD1):
      if (data->_tf->_hdr_wrd_1[*index-1] <= 1)
        data->_tf->_hdr_wrd_1[*index-1] = 2;
      //else if (data->_tf->_hdr_wrd_1[*index-1] >= 65)
      //  data->_tf->_hdr_wrd_1[*index-1] = 64;
      data->initRowAndPost (ident, *index, data->verifyOne (*index-1));
      break;
    case (HDRWRD2):
      if (data->_tf->_hdr_wrd_2[*index-1] <= 1)
        data->_tf->_hdr_wrd_2[*index-1] = 2;
      //else if (data->_tf->_hdr_wrd_2[*index-1] >= 65)
      //  data->_tf->_hdr_wrd_2[*index-1] = 64;
      data->initRowAndPost (ident, *index, data->verifyTwo (*index-1));
      break;
    case (HDRWRD3):
      if (data->_tf->_hdr_wrd_3[*index-1] <= 1)
        data->_tf->_hdr_wrd_3[*index-1] = 2;
      //else if (data->_tf->_hdr_wrd_3[*index-1] >= 65)
      //  data->_tf->_hdr_wrd_3[*index-1] = 64;
      data->initRowAndPost (ident, *index, data->verifyThree (*index-1));
      break;
    default:
      cout << "TredTable::trapHdr called errantly " << *ident << endl;
      return;
  }
}

// trap for change of start or end trace value.
void TredTable::trapVlu (ARGUMENTS2)
{

//Only a modified header word or by entering REMOVE or INSERT will the
//  function operate
  if (*nread <= 0 && strcmp ("REMOVED", endkey)
    && strcmp ("INSERTED", endkey)) return;

// Get the pointer to the Databox
    TredTable *data = (TredTable*)SLDatabox::getUserData(box);

//After entering REMOVE do the data posting
  if (!strcmp ("REMOVED", endkey))
    {
      data->doFilePut ();
      return;
    }

// for a index that is too large, do nothing and send warning
    if (*index > data->_nmax) {
      data->_errpop = new SLErrorPop (data->_pop, "Warning!!!", TF_MAXIMUM);
      return;
    }

  switch (*ident)
  {
    case STRTVLU1:
//    cout << "First start value is " << data->_tf->_strt_vlu_1[*index-1]
//         << endl;
      data->initRowAndPost (ident, *index, data->verifyOne (*index-1));
      break;
    case ENDVLU1:
//    cout << "First end value is " << data->_tf->_end_vlu_1[*index-1]
//         << " now post" << endl;
      data->initRowAndPost (ident, *index, data->verifyOne (*index-1));
      break;
    case STRTVLU2:
//    cout << "Second start value is " << data->_tf->_strt_vlu_2[*index-1]
//         << endl;
      data->initRowAndPost (ident, *index, data->verifyTwo (*index-1));
      break;
    case ENDVLU2:
//    cout << "Second end value is " << data->_tf->_end_vlu_2[*index-1]
//         << " now post" << endl;
      data->initRowAndPost (ident, *index, data->verifyTwo (*index-1));
      break;
    case STRTVLU3:
//    cout << "Third start value is " << data->_tf->_strt_vlu_3[*index-1]
//         << endl;
      data->initRowAndPost (ident, *index, data->verifyThree (*index-1));
      break;
    case ENDVLU3:
//    cout << "Third end value is " << data->_tf->_end_vlu_3[*index-1]
//         << " now post" << endl;
      data->initRowAndPost (ident, *index, data->verifyThree (*index-1));
      break;
    default:
      cout << "TredTable::trapVlu called errantly " << *ident << endl;
      return;
  }
}

int TredTable::fileReset ()
{
  deleteVectors ();
  if (tredfile_clear(_tf)) return 1;
  return fileClear ();
}

int TredTable::fileClear ()
{
  long k2;

  if (_tf)
    {
      for (k2 = 0; k2 < _nmax; k2++)
        {
          _ivara[k2] = 10 * k2;
          _doptr[k2] = _tf->_dos[k2];
          _codptr[k2] = _tf->_codes[k2];
	}
      return 0;
    }
  return 1;
}

//--------------- constants and static variables ------------------//
  static long zero = 0, one = -1, two = -2;
  static long M44  = -44;

void TredTable::sensitizeTable ()
{
  one = 1;
  two = 2;
}

void TredTable::desensitizeTable()
{
  one = -1;
  two = -2;
}

void TredTable::makeHelper()
{
  wbox_message("Press Code buttons for KILL, DEL, REV, and FLAG options.");
  wbox_blank_line();
  wbox_message("                Hdr   Value        Hdr    Value       Hdr     Value");


         //  TRAP ID VARIABLE SWITCH ROW COL NCHAR NDEC

//------------------------- display a set of linked arrays:

         //     N             NMAX    ROW  MAXROWS
  wbox_rega   (&_tf->_nrecs, &_nmax,   0,    30);

         //   TRAP  ID  LABEL    SWITCH  VARIABLE  SWITCH  COL NCHAR NDEC
  wbox_xrega(NULL,  50, " ",      &zero,  _ivara,   &M44,   0,   3,   0);
  wbox_crega(trapDo,  DO,  " Do ", &zero, *_tf->_dos, &two, 0, 4, (int)_size);
  wbox_crega(trapCode, CODE, "Code  ", &zero, *_tf->_codes, &two, 0, 4,
                                                                  (int)_size);
  wbox_irega(trapHdr, HDRWRD1, "Word", &zero, _tf->_hdr_wrd_1,  &one, 0, 4, 0);
  wbox_frega(trapVlu, STRTVLU1,"start",&zero, _tf->_strt_vlu_1, &one, 0, 6, 2);
  wbox_frega(trapVlu, ENDVLU1, "end    ",&zero, _tf->_end_vlu_1,&one, 0, 6, 2);
  wbox_irega(trapHdr, HDRWRD2, "Word", &zero, _tf->_hdr_wrd_2,  &one, 0, 4, 0);
  wbox_frega(trapVlu, STRTVLU2,"start",&zero, _tf->_strt_vlu_2, &one, 0, 6, 2);
  wbox_frega(trapVlu, ENDVLU2, "end    ",&zero, _tf->_end_vlu_2,&one, 0, 6, 2);
  wbox_irega(trapHdr, HDRWRD3, "Word", &zero, _tf->_hdr_wrd_3,  &one, 0, 4, 0);
  wbox_frega(trapVlu, STRTVLU3,"start",&zero, _tf->_strt_vlu_3, &one, 0, 6, 2);
  wbox_frega(trapVlu, ENDVLU3, "end",  &zero, _tf->_end_vlu_3,  &one, 0, 6, 2);
}
                                  
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
