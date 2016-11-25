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
//========================= COPYRIGHT NOTICE ================================
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================

//===========================================================================
//========== Class that handles trace selection by headers        ===========
//========== Each PlotImage class has a TraceSelector and a pass  ===========
//========== thru public method to have it get selections         ===========
//========== Michael L. Sherrill 02/26/01                         ===========
//===========================================================================

#include "trciof77.h"
#include "jsfiles.h"
#include "tfdefs.h"
#include "net/net_env.hh"
#include "read_data.h"
#include "cube.h"
#include "tf_global.h"
#include "trace_selector.hh"

#include <sys/stat.h>
#include <limits.h>

#define MAXTRACE_READ 1000

PatternReader::PatternReader (int pixmap_number)
{
  _trace_numbers = NULL;
  _num_alloc = 0;
  _num_traces = 0;
  _pixmap_number = pixmap_number;
}

PatternReader::~PatternReader ()
{
  if (_trace_numbers != NULL) free (_trace_numbers);
}

#define INCR 100
Boolean PatternReader::assignTraceNumbers (long trace_index)
{
  if (_num_traces == _num_alloc && !incrementTraceNumbers(INCR)) return False;
  _trace_numbers[_num_traces++] = trace_index;
  return True;
}

Boolean PatternReader::incrementTraceNumbers (long count)
{
  long *trace_numbers;
  if (_num_alloc == 0) {
    _num_alloc += count;
    trace_numbers = (long *)malloc (_num_alloc*sizeof(long));
  }
  else {
    _num_alloc += count;
    trace_numbers = (long *)realloc (_trace_numbers, _num_alloc*sizeof(long));
  }
  if (trace_numbers != NULL) {
    _trace_numbers = trace_numbers;
    return True;
  }
  else {
    return False;
  }
}

long PatternReader::getNumTraces()
{
  return _num_traces;
}




TraceSelector::TraceSelector ()
{

  _pattern_readers = 
    (PatternReader **) calloc (1, MAX_PIXMAP *sizeof(PatternReader));

  for (int i = 0; i < MAX_PIXMAP; i++) {
    _pattern_readers[i] = new PatternReader (i);
  }

  _abort_data     = NULL;
  _abort_function = NULL;
}

TraceSelector::~TraceSelector ()
{
  for (int i = 0; i < MAX_PIXMAP; i++) delete _pattern_readers[i];
  free (_pattern_readers);
}



long *TraceSelector::getTraceNumbers (int which_pixmap)
{
  return _pattern_readers[which_pixmap]->getTraceNumbers ();
}

long TraceSelector::getNumTraces (int which_pixmap)
{
  return _pattern_readers[which_pixmap]->getNumTraces ();
}

void TraceSelector::resetNumTraces (int pixmap)
{
  _pattern_readers[pixmap]->resetNumTraces ();
}


//=============================================================================
//== Search a file by a specified read pattern and determine what =============
//== traces fall within the select object specs. The matching     =============
//== traces are stored in the PatternReader class for further use.=============
//=============================================================================
long TraceSelector::findTraces (NDoTraceSelection *select, long nplt,
  long iskp, long ndo, long nskp, int pixmap)
{
  long retval;

  long *traces;
  // Here the user can specify an iskp, nplt, ndo, nskp pattern for each
  //   individual header!
  long count = select->getList (&traces);
  long k2, k3;
  // Here the user can specify an iskp, nplt, ndo, nskp pattern for above
  //   set of traces! That is too much flexibility!! 
  for (k2 = iskp, retval = 0; k2 < count && retval > -1 && retval < nplt;) {
    for (k3 = 0; retval > -1 && k2 < count && k3 < ndo && retval < nplt;
      k3++) {
      if (!_pattern_readers[pixmap]->assignTraceNumbers(traces[k2++]-1)) {
	retval = -1; // memory allocation error
      }
      else {
	retval++;
      }
    }
    k2 += nskp;
  }
  free (traces);
  return retval;
}


//=============================================================================
//== Search a file by a specified read pattern and determine what =============
//== traces fall within up to 3 header word/values. The matching  =============
//== traces are stored in the PatternReader class for further use.=============
//== Note you must call tf_close_ with the lun received from this =============
//== method when you are done.                                    =============
//=============================================================================
long TraceSelector::findTraces(char *filename,       int primary_header,
                               float primary_min,    float primary_max,
                               float primary_inc,    int secondary_header, 
                               float secondary_min,  float secondary_max,
                               float secondary_inc,  int tertiary_header,
                               float tertiary_min,   float tertiary_max,
                               float tertiary_inc,   long nplt,       
                               long iskp,            long ndo,
                               long nskp,            long still_need, 
                               int pixmap,           int open_file,
                               int nhdrs,            int *user_aborted,   
                               int *lun)
{
int stat, istat;
struct Cntrl Cl;
float *hd, *tr, *tf;
Boolean have_a_trace = False;
Boolean ok;
float val;
int primary_index;
int secondary_index;
int tertiary_index;
long num_found = 0;
Boolean aborted = False;

  *user_aborted = 0;

  if(primary_header < 1) return -1;

  Cl.ntot   = nplt;
  Cl.iskp   = iskp;
  Cl.ndo    = ndo;
  Cl.nskp   = nskp;
  Cl.nsamp  = 1;
  Cl.samp1  = 1;
  Cl.sdec   = 1;
  Cl.trnsps = 0;
  Cl.cnvrt  = 0;
  Cl.axis   = 0;
  Cl.index  = 0;

  hd = (float *) malloc( (nhdrs * Cl.ntot) * sizeof(double));
  if(hd == NULL)
    return -1;

  tr = (float *) malloc( Cl.ntot * sizeof(float));
  if(tr == NULL)
    {
    free (hd);
    return -1;
    }

  tf = (float *) malloc((Cl.ntot + 1)  * sizeof(float));
  if(tf == NULL)
    {
    free (hd);
    free(tr);
    return -1;
    }


  stat= read_data_(filename, &istat, &Cl, (char *)hd, (char *)tr, tf, lun,
                   open_file); 

  if(istat != 0)
    {
    free(tr); free(hd); free(tf);
    return -1;
    }


  if(open_file)
    {
    _primary_header   = primary_header;
    _primary_min      = primary_min;
    _primary_max      = primary_max;
    _primary_inc      = primary_inc;
    _secondary_header = secondary_header;
    _secondary_min    = secondary_min;
    _secondary_max    = secondary_max;
    _secondary_inc    = secondary_inc;
    _tertiary_header  = tertiary_header;
    _tertiary_min     = tertiary_min;
    _tertiary_inc     = tertiary_inc;
    _tertiary_max     = tertiary_max;
    }
  

  if(primary_min   < _primary_min   || primary_max   > _primary_max   ||
     secondary_min < _secondary_min || secondary_max > _secondary_max ||
     tertiary_min  < _tertiary_min  || tertiary_max  > _tertiary_max    )
    {
    free(tr); free(hd); free(tf);
    return 0;
    }

  for(long i = 0; i < nplt; i++)
    {

    //Make sure the user has not given up and aborted this process
    if( (_abort_data) && (_abort_function) ) 
      {
      aborted = _abort_function( _abort_data );
      if( aborted ) 
        {
        *user_aborted = USER_ABORTED;
        free(tr); free(hd); free(tf);
        return 0;
        }
      }



    primary_index   = i * nhdrs + primary_header   - 1;
    secondary_index = i * nhdrs + secondary_header - 1;
    tertiary_index  = i * nhdrs + tertiary_header  - 1;
    val = hd[primary_index];    

    //Try to find primary header
    if( val >= primary_min &&  val <= primary_max )
      {
      have_a_trace = True;
      primary_min += primary_inc;
      //Try to find secondary if requested
      if(secondary_header)
        {
        val = hd[secondary_index];
        if( val >= secondary_min && val <= secondary_max &&
            isBin(secondary_min, secondary_max, secondary_inc, val))
          {
          have_a_trace = True;
          //Try to find tertiary if requested
          if(tertiary_header)
            {
            val = hd[tertiary_index];
            if( val >= tertiary_min && val <= tertiary_max &&
                isBin(tertiary_min, tertiary_max, tertiary_inc, val))
              {
              have_a_trace = True;
              }
            else
              {
              have_a_trace = False;
              }
            }//End tertiary requested
          }
        else//Could not find secondary requested
          {
          have_a_trace = False;
          }
        }//End secondary requested
      if(have_a_trace)
        {
        //The following assumes header 1 is set as it should be
        long trace_index = (long)hd[i*nhdrs] - 1;
        ok= _pattern_readers[pixmap]->assignTraceNumbers(trace_index);
        if(!ok)
          {
          free(tr); free(hd); free(tf);
          return -1;
          }
        ++num_found;
        if(num_found == still_need) ++pixmap;
        }
      }//End primary found

     if(primary_min   < _primary_min   || primary_max   > _primary_max   ||
        secondary_min < _secondary_min || secondary_max > _secondary_max ||
        tertiary_min  < _tertiary_min  || tertiary_max  > _tertiary_max    )
        {
        free(tr); free(hd); free(tf);
        *user_aborted = PAST_USER_LIMITS;
        return num_found; 
        }

    }//End all traces

  

  free(tr); free(hd); free(tf);

  return num_found;    
}

//=============================================================================
//== Search a file and group the number of traces found by a header change ====
//== For instance one use of this is to search files for VA and find how   ====
//== many traces are in each gather of traces.                             ====
//=============================================================================
long TraceSelector::findTracesByPattern(char *filename,     int xheader, 
                                        int yheader,        long num_traces,
                                        long                nhdrs,
                                        long *num_gathers,  int *user_aborted, 
                                        float *xloc,        float *yloc,
                                        float *vel_min,     float *vel_max,
                                        float *min_x,       float *max_x)
{
float *hd, *tr, *tf;
struct Cntrl Cl;
int pixmap;
int stat, istat;
float current_group = 0.0F, previous_group = -1.0F;
Boolean ok, aborted = False;
int cstat, lun, open_file = 1;
int len = 256;
char msg[256];
long num_read;
Boolean not_done = True;
Boolean last_try = False;
long can_read;
long *request_array;
long total_traces = 0;

 

  num_read = min(num_traces, MAXTRACE_READ);

  Cl.ntot = num_read;
  Cl.iskp = 0;
  Cl.ndo  = num_read;
  Cl.nskp = 0;
  Cl.nsamp= 1;
  Cl.samp1  = 1;
  Cl.sdec   = 1;
  Cl.trnsps = 0;
  Cl.cnvrt  = 0;
  Cl.axis   = 0;
  Cl.index  = 0;
  

  //May need to bust these up into smaller chunks and make multiple reads
  hd = (float *) 
      malloc( (nhdrs * sizeof(double)) * (int)num_read);
  if(hd == NULL)
    return 0;

  tr = (float *) malloc( sizeof(float) * (int)num_read);
  if(tr == NULL)
    {
    free (hd);
    return 0;
    }

  tf = (float *) malloc( sizeof(float) * ((int)num_read + 1));
  if(tf == NULL)
    {
    free (hd);
    free(tr);
    return 0;
    }

  pixmap = 0;
  while(not_done)
    {
    if(last_try) not_done = False;

    stat= read_data_(filename, &istat, &Cl, (char *)hd, (char *)tr, tf, 
                     &lun, open_file); 
    open_file = 0;
    if(istat != 0)
      {
      cstat = locClose (lun, msg, len);
      free (hd);
      free(tr);
      return 0;
      }

    //Make sure the user has not given up and aborted this process
    if( (_abort_data) && (_abort_function) ) 
      {
      *user_aborted = _abort_function( _abort_data );
      if( *user_aborted ) 
        {
        cstat = locClose (lun, msg, len);
        free (hd);
        free(tr);
        return 0;
        }
      }


    //Store the traces just read
    for(long i = 0; i < num_read; i++)
      {
      ++total_traces;
      current_group = hd[i * nhdrs + (xheader - 1)];

      *vel_min = *vel_min > hd[i * nhdrs + PlotImage::VELHDR] 
              ? hd[i * nhdrs + PlotImage::VELHDR]
              : *vel_min;

      *vel_max = *vel_max < hd[i * nhdrs + PlotImage::VELHDR] 
              ? hd[i * nhdrs + PlotImage::VELHDR]
              : *vel_max;

      *min_x   = *min_x > hd[i * nhdrs + (xheader - 1)] 
              ? hd[i * nhdrs + (xheader - 1)]
              : *min_x;

      *max_x   = *max_x < hd[i * nhdrs + (xheader - 1)] 
              ? hd[i * nhdrs + (xheader - 1)]
              : *max_x;

      if(i == 0 && Cl.iskp == 0)//Initialize
        {
        if(xloc != NULL)
          xloc[pixmap] = hd[xheader - 1];
        if(yloc != NULL)
          yloc[pixmap] = hd[yheader - 1];
        previous_group = current_group; 
        }

      if(previous_group != current_group )
        {
        ++pixmap;
        if(xloc != NULL)
          xloc[pixmap] = hd[i * nhdrs + xheader - 1];
        if(yloc != NULL)
          yloc[pixmap] = hd[i * nhdrs + yheader - 1];
        previous_group = current_group;
        }

      ok = _pattern_readers[pixmap]->assignTraceNumbers(i + Cl.iskp);
      if(!ok)
        {
        cstat = locClose (lun, msg, len);
        free (hd);
        free(tr);
        return 0;
        }
      }

       

    //Increment to read next batch of traces
    Cl.iskp += num_read;
    if(Cl.iskp + Cl.ndo >= num_traces) /*end of file need to redo this later*/
      {
      if(Cl.iskp >= num_traces)
        {
        not_done = False; 
        }
      else
        {
        can_read = num_traces - Cl.iskp;
        num_read = (int)can_read;
        Cl.ntot = Cl.ndo = num_read;
        last_try = True;
        if(Cl.ntot <= 0) not_done = False; 
        }
      }

    }//End of file i/o



  cstat = locClose (lun, msg, len);

  *num_gathers = pixmap + 1;

  free (hd);
  free(tr);

  return total_traces;
}


Boolean TraceSelector::setTrace(int pixmap, long trace)
{
  return _pattern_readers[pixmap]->assignTraceNumbers(trace);
}

   
Boolean TraceSelector::isBin(float min, float max, float inc, float val)
{
long num;
Boolean is_bin = False;
float check_val;

  assert(max > min);

  num = (max - min) / inc + 1.5;

  //May need to add a width to this later
  for(long i = 0; i < num; i++)
    {
    if(inc > 0.0F)
      check_val = min + (i * inc);
    else
      check_val = max - (i * inc);
    if(check_val == val) return True;    
    }

  return is_bin;
}


int TraceSelector::locClose (int lun, char *msg, int len)
{
  int retval;

  /*printf ("TraceSelector::locClose: calling jsfiles_close\n");*/
  if (jsfiles_close(lun) == 1) {
    retval = 0;
    strcpy (msg, "");
  }
  else {
    tf_close_ (&lun, &retval, msg);
  }
  return retval;
}




NDoTraceSelection::NDoTraceSelection (int max_count) :
  _rows            (NULL),
  _abort_function  (NULL),
  _abort_data      (NULL),
  _tlists          (NULL),
  _nplots          (NULL),
  _disp_order      (NULL),
  _data_order      (NULL),
  _irows           (NULL),
  _list            (NULL),
  _list_size       (0L),
  _max_count       (0),
  _count           (0)
{
  constructorHelper (max_count);
  initialize ();
}

NDoTraceSelection::~NDoTraceSelection ()
{
  int k2;
  for (k2 = 0; k2 < _max_count; k2++) {
    if (_rows[k2] != NULL) delete _rows[k2];
  }
  delete [] _rows;
  delete [] _disp_order;
  delete [] _data_order;
  delete [] _irows;

  if (_list != NULL) delete [] _list;
}

NDoTraceSelection::NDoTraceSelection (NDoTraceSelection *select) :
  _rows            (NULL),
  _abort_function  (NULL),
  _abort_data      (NULL),
  _tlists          (NULL),
  _nplots          (NULL),
  _disp_order      (NULL),
  _data_order      (NULL),
  _irows           (NULL),
  _list            (NULL),
  _list_size       (0),
  _max_count       (0),
  _count           (0)
{
  constructorHelper (select->_max_count);
  set (select);
}

void NDoTraceSelection::set (NDoTraceSelection *select)
{
  initialize ();
  if (select == NULL) return;
  // collapse any rows that are not used and use the row runs to sort
  //   the rows from fastest to slowest
  int *irows;
  Boolean order_needed;
  _count = select->getCount (&order_needed, &irows, True);
  if (!order_needed) order_needed = !select->valid ();
  int k2;
  for (k2 = 0; k2 < _count; k2++) {
    assert (select->_rows[irows[k2]] != NULL);
    _rows[k2] = new NDoTraceRow (select->_rows[irows[k2]]);
    _disp_order[k2] = select->_disp_order[irows[k2]];
    if (!order_needed) {
      _data_order[k2] = select->_data_order[irows[k2]];
    }
  }
  if (irows) free (irows);

  if (order_needed) {
    setDataOrder (); // will be invalid if display order not correct
  }
  else if (select->_list_size > 0 && select->_list != NULL && valid()) {
    // if there were gaps the following will not be useful
    assert (select->_list_size == select->getNPlot());
    _list_size = select->_list_size;
    _list = new long[_list_size];
    for (k2 = 0; k2 < _list_size; k2++) {
      _list[k2] = select->_list[k2];
    }
  }
}

// non-trivial headers must have been defined and unique
// data order must have been defined and unique
// display order must have been defined and unique
Boolean NDoTraceSelection::valid ()
{
  Boolean retval;

  if (_count < 1) {
    return False;
  }

  int k2;
  for (k2 = 0, retval = True; retval && k2 < _count; k2++) {
    if (_rows[k2]->getScope()->getHeader() < 1) retval = False; // no header
    else if (_data_order[k2] != k2            ) retval = False; // no order
  }

  int k3, head_count, disp_count;
  for (k2 = 0; retval && k2 < _count; k2++) {
    for (k3 = 0, head_count = 0, disp_count = 0; retval && k3 < _count;
      k3++) {
      if (_rows[k2]->getScope()->getHeader() == 
          _rows[k3]->getScope()->getHeader()   ) head_count++;
      if (_disp_order[k3] == k2                ) disp_count++;
    }
    if (head_count != 1     ) retval = False; // must be one and only one
    else if (disp_count != 1) retval = False;
  }
  return retval;
}

// if next is True then
// only increment the slowest changing (in the displayed sense) header word
// else
// only decrement the slowest changing (in the displayed sense) header word
int NDoTraceSelection::scanFrame (Boolean next, Boolean exact, int frames)
{
  int count = getHeaderCount ();
  assert (count > 0);
  int index = _data_order[_disp_order[count-1]];
  assert (index > -1);

  int retval;

  if (frames < 1) frames = 1;

  int k2, num;
  for (k2 = 0, retval = NDoTraceRow::NO_ERROR, num = 0;
    retval != NDoTraceRow::ERROR && k2 < frames; k2++) {
    retval = _rows[index]->scanFrame (next, exact);
    if (retval != NDoTraceRow::ERROR) {
      clearList ();
      num++;
    }
  }

  if (retval == NDoTraceRow::ERROR && frames > 1) {
    // go back to the initial state
    for (k2 = 0; k2 < num; k2++) {
      _rows[index]->scanFrame (!next, exact);
    }
  }

  return retval;
}

// a specialized function
//   assume that select has a set of rows with which to update this object
//   assume that this object already has all the headers that match select's
//   as appropriate make this' rows identical to select's rows
//   if this has "other" rows not in select, they are unaffected
void NDoTraceSelection::setRows (NDoTraceSelection *select)
{
  Boolean found;
  int k2, k3, header;
  for (k2 = 0; k2 < _max_count; k2++) {
    header = select->getRow(k2)->getScope()->getHeader ();
    if (header > 0) {
      // it is appropriate to look for a match
      for (k3 = 0, found = False; !found && k3 < _max_count; k3++) {
	if (getRow(k3)->getScope()->getHeader() == header) {
	  // make the rows identical
	  getRow(k3)->set (select->getRow(k2));
	  found = True;
	}
      }
      assert (found); // insist that this contains select's header!
    }
  }
}

// determines the number of non-zero headers and ignore gaps
// optionally indicate if gaps were found in rows
// optionally return the rows in order from fastest to slowest
int NDoTraceSelection::getCount (Boolean *gap_found, int **irows,
  Boolean sort)
{
  if (_max_count < 1) {
    if (irows) *irows = NULL;
    if (gap_found) *gap_found = False;
    return 0;
  }

  int retval;

  Boolean hit_a_gap, loc_gap_found;
  int k2;
  for (k2 = 0, retval = 0, hit_a_gap = False, loc_gap_found = False;
    k2 < _max_count; k2++) {
    if (_rows[k2]                               &&
        _rows[k2]->getScope()->getHeader() > 0  &&
        (!sort || _rows[k2]->getOrder()    > 0)   ) {
      if (irows) {
        _irows[retval] = k2;
      }
      retval++;
      if (hit_a_gap) loc_gap_found = True;
    }
    else {
      hit_a_gap = True;
    }
  }

  if (irows) {
    if (retval > 0) {
      *irows = (int *)malloc (retval*sizeof(int*));
      for (k2 = 0; k2 < retval; k2++) (*irows)[k2] = _irows[k2];

      if (sort) {
	// sort irows ordered from fastest to slowest.
	int k3, tmp;
	for (k2 = 0; k2 < retval; k2++) {
	  for (k3 = k2; k3 < retval; k3++) {
	    if (_rows[(*irows)[k3]]->getScope()->getRun() <
		_rows[(*irows)[k2]]->getScope()->getRun()  ) {
	      tmp = (*irows)[k2];
	      (*irows)[k2] = (*irows)[k3];
	      (*irows)[k3] = tmp;
	    }
	  }
	}
      }
    }
    else {
      *irows = NULL;
    }
  }

  if (gap_found) {
    *gap_found = loc_gap_found;
  }
  return retval;
}

// determines the number of headers and optionally return header array
//   and data order does not matter
int NDoTraceSelection::getHeaderCount (int **headers, Boolean ignore_gap)
{
  Boolean gap_found;
  int *irows;
  _count = getCount (&gap_found, &irows);
  if (_count > 0) {
    if (!ignore_gap && gap_found) {
      if (headers) {
	*headers = NULL;
	_count = 0;
      }
    }
    else {
      // either no gap or ignore gap
      if (headers) {
	int k2;
	*headers = (int *)malloc (_count*sizeof(int));
	for (k2 = 0; k2 < _count; k2++) {
	  (*headers)[k2] = _rows[irows[k2]]->getScope()->getHeader ();
	}
      }
    }
    free (irows);
  }
  return _count;
}

// warning: this only returns the number of traces in the data set if enough
//   of the headers have been specified to span the entire data set.
long NDoTraceSelection::getNumTraces ()
{
  long retval;

  Boolean gap_found;
  int *irows;
  int num_rows = getCount (&gap_found, &irows);
  if (num_rows > 0) {
    int k2;
    for (k2 = 0, retval = 1L; k2 < num_rows; k2++) {
      retval *= (long)getRow(k2)->getScope()->getLength ();
    }
  }
  else {
    retval = 0L;
  }
  return retval;
}

void NDoTraceSelection::setScopes (int count, TraceHeaderScope **scopes)
{
  int k2, k3;
  Boolean found_it;
  for (k2 = 0; k2 < count; k2++) {
    for (k3 = 0, found_it = False; !found_it && k3 < _max_count; k3++) {
      if (scopes[k2]->getHeader() == getRow(k3)->getScope()->getHeader()) {
	getRow(k3)->getScope()->set (scopes[k2], True);
	found_it = True;
      }
    }
    assert (found_it); // assert that a matching header was found
  }
}

// verify no gaps are in the rows
// verify all headers are unique
// verify each _rows order has been defined where _order = 0
// is the fastest changing displayed dimension and _order = _count-1
// is the slowest changing displayed dimension used once and only once
// verify the data order is the order of the rows. ie.
// the 0th row corresponds to the fastest changing data dimension
// the _count-1 row corresponds to the slowest changing data dimension
// _count is defined here and is the number of rows whose scope's
// headers are non-trivial
Boolean NDoTraceSelection::setDataOrder ()
{
  // that some headers have been defined
  _count = getHeaderCount ();
  if (_count < 1) return False;

  // insist that each _row's _order has been defined in a valid range
  //   note: getOrder returns a "One-relative" result
  int k2;
  for (k2 = 0; k2 < _count; k2++) {
    if (_rows[k2] == NULL              ||
        _rows[k2]->getOrder() < 1      ||
	_rows[k2]->getOrder() > _count   ) {
      _count = 0;
      return False;
    }
  }

  // insist that the _rows are ordered where faster changing data
  //   is followed by slower changing data with no ties!
  for (k2 = 0; k2 < _count; k2++) {
    if (k2 < _count-1) {
      if (_rows[k2  ]->getScope()->getRun() >=
          _rows[k2+1]->getScope()->getRun()   ) {
	_count = 0;
	return False; // next run must get longer!
      }
    }
    _data_order[k2] = k2;
  }

  // define _disp_order
  for (k2 = 0; k2 < _count; k2++) {
    _disp_order[k2] = _rows[k2]->getOrder() - 1;
  }

  // review the validity of all definitions
  return valid();
}

NDoTraceRow *NDoTraceSelection::getRow (int index)
{
  assert (index > -1 && index < _max_count);
  if (_rows[index] == NULL) {
    _rows[index] = new NDoTraceRow ();
  }
  return _rows[index];
}

// Attempt to set the default to view an overview of the section of at most
//   def_nplot or the total number of traces, whichever is less
long NDoTraceSelection::setNPlotDefault (long def_nplot)
{
  if (_count < 1) return 0L;

  int k2;
  long min;
  // start off by skipping all but one trace in each dimension
  long *adj = new long[_count];
  for (k2 = 0; k2 < _count; k2++) {
    _rows[k2]->setFirst ((float)_rows[k2]->getScope()->getMinimum());
    _rows[k2]->setLast  ((float)_rows[k2]->getScope()->getMaximum());
    _rows[k2]->setNDo   (1L);
    _rows[k2]->setNSkip (_rows[k2]->getScope()->getLength()-1);
    adj[k2] = _rows[k2]->getScope()->getLength ();
    if (k2 == 0) {
      min = adj[0];
    }
    else {
      if (min > adj[k2]) min = adj[k2];
    }
  }

  // define the adjustments so as to maintain the aspect ratio of each header
  //   range to an integer multiple
  for (k2 = 0; k2 < _count; k2++) {
    adj[k2] = (adj[k2] + min - 1) / min;
  }

  // loop while each time skipping adj[k2] # of traces in each dimension
  //   until all the skips are 0 or until getNPlot is >= DEF_NPLOT
  long nskip, nplot, adj_count;
  int last_row;
  for (nplot = getNPlot(), adj_count = 1, last_row = -1;
    adj_count > 0 && nplot < def_nplot; nplot = getNPlot()) {
    for (k2 = 0, adj_count = 0; k2 < _count; k2++) {
      nskip = _rows[k2]->getNSkip();
      if (nskip > 0) {
	nskip -= adj[k2];
	if (nskip < 1) nskip = 0;
        _rows[k2]->setNSkip (nskip);
	adj_count++;
	last_row = k2;
      }
    }
  }
  if (nplot > def_nplot) {
    // went a bit too far; reduce by the last row
    assert (last_row != -1);
    _rows[last_row]->setNSkip (_rows[last_row]->getNSkip()+adj[last_row]);
    nplot = getNPlot ();
  }
  delete [] adj;
  return nplot;
}

long NDoTraceSelection::getNPlot ()
{
  long retval;

  int k2;
  long nplot;
  for (k2 = 0, retval = 1; k2 < _count; k2++) {
    // Insist that all rows are defined
    assert (_rows[k2] != NULL);
    nplot = _rows[k2]->getNPlot ();
    // note: if any row has less than one trace, result is zero!!!
    retval *= nplot;
  }
  return retval;
}

// return the number of valid frames (only full-sized).
//   first attemp to find the given number of frames.
//   should the last frame exist only partially, return the number of
//   traces in it, but return only the number of full-sized frames.
//   if the returned number of frames are all equal sized, return 0 for the
//   last frame size.
int NDoTraceSelection::verifyNFrames (int frames, int *last_frame_size)
{
  int retval;

  if (frames == 0) return 0;

  int count = getHeaderCount ();
  if (count < 1) return 0;
  int slowest = _data_order[_disp_order[count-1]];
  // remember the current frame
  long nplt       = _rows[slowest]->getNPlot ();
  float old_first = _rows[slowest]->getFirst ();
  float old_last  = _rows[slowest]->getLast  ();

  int k2, stat;
  Boolean next  = True;
  Boolean exact = True;

  // determine if the given number of frames are obtainable?
  // begin with 1 because you are sitting on a valid frame!
  for (k2 = 0, retval = 1; k2 < frames-1; k2++) {
    stat = _rows[slowest]->scanFrame (next, exact);
    if (stat != NDoTraceRow::ERROR) retval++;
  }

  if (last_frame_size) {
    if (retval == frames) {
      *last_frame_size = 0;
    }
    else {
      NDoTraceRow *row = _rows[slowest];
      TraceHeaderScope *scope = row->getScope ();
      if (scope->getTrace(row->getLast())      <
          scope->getTrace(scope->getMaximum())  ) {
	stat = row->scanFrame (next, !exact);
	*last_frame_size = getNPlot ();
	// another partial frame exists but is not counted
      }
      else {
	*last_frame_size = 0;
      }
    }
  }

  // make the original frame current
  _rows[slowest]->setFirst (old_first);
  _rows[slowest]->setLast  (old_last );

  return retval;
}

void NDoTraceSelection::clearList ()
{
  if (_list) {
    delete [] _list;
    _list = NULL;
    _list_size = 0;
  }
}

long NDoTraceSelection::getList (long **list)
{
  assert (list != NULL);

  long retval;
  if (_list == NULL) {
    retval = findList ();
  }
  else {
    retval = getNPlot ();
    if (retval > 0 && retval != _list_size) {
      // list is out of sync, redo it
      clearList ();
      retval = findList ();
    }
  }

  if (_list_size > 0 && list != NULL) {
    *list = (long *)malloc (_list_size*sizeof(long));
    *list = (long *)memcpy (*list, _list, _list_size*sizeof(long));
  }
  else if (list != NULL) {
    *list = NULL;
  }

  return retval;
}

void NDoTraceSelection::setAbortFunction (AbortFunction function,
  void *abort_data)
{
  _abort_function = function;
  _abort_data = abort_data;
}

AbortFunction NDoTraceSelection::getAbortFunction ()
{
  return _abort_function;
}

void *NDoTraceSelection::getAbortData ()
{
  return _abort_data;
}

void NDoTraceSelection::constructorHelper (int max_count)
{
  assert (max_count > 0);
  _max_count = max_count;
  _rows  = new NDoTraceRow*[_max_count];
  _irows = new int[_max_count];
  int k2;
  for (k2 = 0; k2 < _max_count; k2++) {
    _rows[k2] = NULL;
  }
  _disp_order = new int[_max_count];
  _data_order = new int[_max_count];
}

void NDoTraceSelection::initialize ()
{
  int k2;
  for (k2 = 0; k2 < _max_count; k2++) {
    if (_rows[k2]) _rows[k2]->initialize ();
    _disp_order[k2] = -1;
    _data_order[k2] = -1;
  }
  _count = 0;
  clearList ();
}

// must call setDataOrder before getList. It defines _count, etc.
long NDoTraceSelection::findList ()
{
  assert (_list == NULL);

  long retval;

  retval = getNPlot ();
  if (retval > 0) {
    if (valid()) {
      _list_size = retval;
      _list = new long[_list_size];
      if (_list == NULL) {
	return 0; // could not allocate the memory
      }
      assert (_nplots == NULL);
      _nplots = new long[_count];
      int k2;
      for (k2 = 0; k2 < _count; k2++) {
	_nplots[k2] = 0L;
      }
      assert (_tlists == NULL);
      _tlists = new long*[_count];
      int k3;
      for (k2 = 0; k2 < _count; k2++) {
	//k3 = _data_order[k2];
	//assert (_rows[k2] != NULL);
	if (_rows[k2] == NULL) {
	  cleanFindLists ();
          return 0L;
	}
	_nplots[k2] = _rows[k2]->getTraceList (&_tlists[k2]);
	//_nplots[k3] = _rows[k2]->getTraceList (&_tlists[k3]);
	//assert (_nplots[k2] > 0);
	if (_nplots[k2] < 1) {
	  cleanFindLists ();
          return 0;
	}
	//assert (_nplots[k3] > 0);
      }

      for (k2 = 0; k2 < _list_size; k2++) {
        _list[k2] = getTrace (k2, _count-1);
      }

      cleanFindLists ();
    }
    else {
      retval = 0L;
    }
  }
  return retval;
}

void NDoTraceSelection::cleanFindLists ()
{
  if (_tlists != NULL) {
    assert (_nplots != NULL);
    int k2;
    for (k2 = 0; k2 < _count; k2++) {
      if (_nplots[k2] > 0) delete [] _tlists[k2];
    }
    delete [] _tlists, _tlists = NULL;
  }
  if (_nplots != NULL) {
    delete [] _nplots, _nplots = NULL;
  }
}

// It is assumed that _rows[*], _tlists[*], and _nplots[*} correspond with
//   each other. If the recursive method required that the _rows, _nplots,
//   and _tlists arrays be ordered by how the data is stored, then the
//   following code could be written more simply by replacing _data_order[*]
//   with *. However, to accomodate a random order of _rows, _data_order is
//   used. Also note that the _disp_order array defines the requested
//   display order
long NDoTraceSelection::getTrace (long trace, int dim)
{
  long retval;

  int k2;
  long den, ltrace;
  for (k2 = 0, den = 1; k2 < _disp_order[dim]; k2++) {
    den *= _nplots[_disp_order[_data_order[k2]]];
  }
  ltrace = (trace / den) % _nplots[_data_order[dim]];

  if (dim == 0) {
    retval = _tlists[_data_order[dim]][ltrace];
  }
  else {
    long len;
    for (k2 = 0, len = 1; k2 < dim; k2++) {
      len *= (long)_rows[_data_order[k2]]->getScope()->getLength ();
    }
    retval = (_tlists[_data_order[dim]][ltrace] - 1) * len
      + getTrace (trace, dim-1);
  }
  return retval;
}




NDoTraceRow::NDoTraceRow () :
  _scope  (NULL)
{
  initialize ();
}

NDoTraceRow::~NDoTraceRow ()
{
  delete _scope;
}

NDoTraceRow::NDoTraceRow (NDoTraceRow *row) :
  _scope  (NULL)
{
  set (row);
  assert (_scope->valid());
}

void NDoTraceRow::set (NDoTraceRow *row)
{
  assert (row != NULL);

  _order = row->_order;
  _first = row->_first;
  _last  = row->_last;
  _ndo   = row->_ndo;
  _nskip = row->_nskip;
  _cur   = NONE;

  if (_scope == NULL) {
    _scope = new TraceHeaderScope (row->_scope);
  }
  else {
    assert (_scope->set(row->_scope,True));
  }
}

int NDoTraceRow::scanFrame (Boolean next, Boolean exact)
{
  int retval;

  int sign;
  if (next) sign = +1;
  else      sign = -1;

  float oldfirst = _first;
  float oldlast  = _last;

  long inc = _ndo + _nskip;

  if (inc > 0) {
    double diff = ((double)_last - (double)_first) / _scope->getIncrement();
    _first = (float)((double)_last + sign * _scope->getIncrement() * inc);
    _last = _first + (float)(diff * sign * _scope->getIncrement());
    retval = validPattern (exact);
  }
  else {
    retval = ERROR;
  }
  if (retval == ERROR) {
    // put it back the way it was
    _first = oldfirst;
    _last  = oldlast;
  }
  _cur = NONE;
  return retval;
}

void NDoTraceRow::initialize ()
{
  _order = -1;
  _first = 0; // -FLT_MAX;
  _last  = 0; // -FLT_MAX;
  _ndo   = 0L;
  _nskip = 0L;
  _cur   = NONE;

  if (_scope == NULL) {
    _scope = new TraceHeaderScope ();
  }
  else {
    _scope->initialize (); // causes invalidity
  }
}

int NDoTraceRow::valid ()
{
  int retval;

  Boolean ok =
    (_scope              != NULL) &&
    (_scope->getHeader()  >    0)   ;

  if (ok) {
    retval = validPattern ();
    if (retval != ERROR && _order < 0) {
      retval = ERROR;
    }
  }
  else {
    retval = ERROR;
  }
  return retval;
}

int NDoTraceRow::getOrder ()
{
  return _order + 1;
}

float NDoTraceRow::getFirst ()
{
  return _first;
}

float NDoTraceRow::getLast ()
{
  return _last;
}

long NDoTraceRow::getNDo ()
{
  return _ndo;
}

long NDoTraceRow::getNSkip ()
{
  return _nskip;
}

long NDoTraceRow::getNPlot ()
{
  long retval;

  if (valid() == ERROR) return 0;

  long patt_len = _ndo + _nskip;
  double value;
  long patt;
  for (value = (double)_first, retval = 0, patt = 0; hasNext(value);
    value += _scope->getIncrement(), patt++) {
    if (patt == patt_len) patt = 0;
    if (patt < _ndo) {
      retval++;
    }
  }
  return retval;
}

// as needed alter in order: _nskip, _last, & _first to achieve the given
//   number of traces. do not alter _ndo
Boolean NDoTraceRow::adjustUsingNPlot (long *nplot)
{
  Boolean retval;

  if (_ndo < 1) return False; // not enough info; number to do is not altered

  retval = findClosestNSkip(nplot) || findClosestLast(nplot) ||
           findClosestFirst(nplot);
  return retval;
}

long NDoTraceRow::getTraceList (long **tlist)
{
  long retval;

  long count = getNPlot ();

  if (count > 0) {
    *tlist = new long[count];
    long patt_len = _ndo + _nskip;
    double value;
    long trace, patt;
    for (value = (double)_first, retval = 0,
      trace = _scope->getTrace((double)_first), patt = 0;
      hasNext(value); value += _scope->getIncrement(), trace++,
      patt++) {
      if (patt == patt_len) patt = 0;
      if (patt < _ndo) {
	(*tlist)[retval++] = trace;
      }
    }
    assert (retval == count);
  }
  else {
    *tlist = NULL;
    retval = count;
  }
  return retval;
}

void NDoTraceRow::setOrder (int order)
{
  _cur = ORDER;
  _order = order - 1;
}

void NDoTraceRow::setFirst (float first)
{
  _cur = FIRST;
  _first = first;
}

void NDoTraceRow::setLast (float last)
{
  _cur = LAST;
  _last = last;
}

void NDoTraceRow::setNDo (long ndo)
{
  _cur = NDO;
  _ndo = ndo;
}

void NDoTraceRow::setNSkip (long nskip)
{
  _cur = NSKIP;
  _nskip = nskip;
}


// returns NO_ERROR, FIXED_ERROR, or ERROR
// if exact != True, then fix error if possible
int NDoTraceRow::validPattern (Boolean exact)
{
  int retval = NO_ERROR;

  if (!_scope || !_scope->valid()) {
    retval = ERROR;
  }
  else {

    float first, last;
    // _first must be within bounds
    first = (float)fixFirst ();

    if (first != _first) {
      if (!exact) {
	_first = first;
	retval = FIXED_ERROR;
      }
      else {
	retval = ERROR;
      }
    }

    // _last must be within bounds
    last = (float)fixLast ();

    if (last != _last) {
      if (!exact) {
	_last = last;
	retval = FIXED_ERROR;
      }
      else {
	retval = ERROR;
      }
    }

    if (retval != ERROR) {

      long ftrace, ltrace;
      // a trace with _first precisely in it must be chosen
      ftrace =        _scope->getTrace ((double)_first);
      first  = (float)_scope->getValue (ftrace);
      if (first != _first) {
	if (!exact) {
	  // force _first to be computed as first
	  _first = first;
	  retval = FIXED_ERROR;
	}
	else {
	  retval = ERROR;
	}
      }

      // a trace with _last precisely in it must be chosen
      ltrace  =        _scope->getTrace ((double)_last );
      last    = (float)_scope->getValue (ltrace);
      if (last != _last) {
	if (!exact) {
	  // force _last to be computed as last
	  _last = last;
	  retval = FIXED_ERROR;
	}
	else {
	  retval = ERROR;
	}
      }

      if (ftrace > ltrace) {
	if (!exact) {
	  // it is invalid for first trace and last trace to be ordered
	  //   left to right-reverse the trace display order can accomplish
	  //   that. Here if this is detected-assume the user is wanting
	  //   the last trace to be equal to the first trace!
	  if (_cur == LAST) {
	    _first = _last;
	    _cur = NONE;
	  }
	  else if (_cur == FIRST) {
	    _last = _first;
	    _cur = NONE;
	  }
	  else {
	    _last = _first;
	  }
	  retval = FIXED_ERROR;
	}
	else {
	  retval = ERROR;
	}
      }
    }

    if (retval != ERROR) {
      if (_ndo < 1) {
	// initialize to do 1
	_ndo = 1;
	retval = FIXED_ERROR;
      }
      if (_nskip < 0) {
	// initialize to skip 0
	_nskip = 0;
	retval = FIXED_ERROR;
      }
    }
  }
  return retval;
}

TraceHeaderScope *NDoTraceRow::getScope ()
{
  return _scope;
}

// find the closest _nskip to achieve the given number of traces
Boolean NDoTraceRow::findClosestNSkip (long *nplot)
{
  Boolean retval;

  long ftrace  = _scope->getTrace ((double)_first);
  long ltrace  = _scope->getTrace ((double)_last);
  long ntraces = ltrace - ftrace + 1;

  if (ntraces > *nplot) {
    // there are too many traces, so try to skip some
    int del = ntraces / *nplot;
    if (del > _ndo) {
      // required increment is bigger than number to do
      _nskip = del - _ndo;
      int inc = _ndo + _nskip;
      int npacks = ntraces / inc;
      int dtraces = ntraces - npacks * _ndo;
      if (dtraces > _ndo) dtraces + _ndo;
      retval = *nplot == npacks * _ndo + dtraces;
    }
    else {
      // required increment is smaller than number to do
      _nskip = 0;
      retval = False;
    }
  }
  else if (ntraces < *nplot) {
    // there are not enough traces
    _nskip = 0;
    retval = False;
  }
  else {
    // there are exactly the right amount
    _nskip = 0;
    retval = True;
  }
  return retval;
}

// find the closest _last to achieve the given number of traces
Boolean NDoTraceRow::findClosestLast (long *nplot)
{
  Boolean retval;

  long ftrace = _scope->getTrace ((double)_first);
  int npacks = (*nplot + _ndo - 1) / _ndo;
  int inc    = _ndo + _nskip;
  long ltrace = ftrace + npacks * inc - 1;
  long ntraces = npacks * _ndo;

  if (ntraces != *nplot) {
    int dtrace = ntraces - *nplot;
    ltrace = ltrace - inc + dtrace;
  }

  float last = (float)_scope->getValue (ltrace);

  if ((double)last < _scope->getMinimum()) {
    _last = (float)_scope->getMinimum ();
    retval = False;
  }
  else if ((double)last > _scope->getMaximum()) {
    _last = (float)_scope->getMaximum ();
    retval = False;
  }
  else {
    _last = last;
    retval = True;
  }

  return retval;
}

// find the closest _first to achieve the given number of traces
Boolean NDoTraceRow::findClosestFirst (long *nplot)
{
  Boolean retval;

  long ltrace = _scope->getTrace ((double)_last);
  int npacks = (*nplot + _ndo - 1) / _ndo;
  int inc    = _ndo + _nskip;
  long ftrace = ltrace + _nskip - npacks * inc + 1;
  long ntraces = npacks * _ndo;

  if (ntraces != *nplot) {
    int dtrace = ntraces - *nplot;
    ftrace = ftrace + dtrace;
  }

  float first = (float)_scope->getValue (ftrace);

  if ((double)first < _scope->getMinimum()) {
    _first = (float)_scope->getMinimum ();
    retval = False;
  }
  else if ((double)first > _scope->getMaximum()) {
    _first = (float)_scope->getMaximum ();
    retval = False;
  }
  else {
    _first = first;
    retval = True;
  }
  return retval;
}

Boolean NDoTraceRow::hasNext (double value)
{
  double incr = _scope->getIncrement ();

  if (incr > 0) {
    return value <= (double)_last;
  }
  else {
    return value >= (double)_last;
  }
}

double NDoTraceRow::fixFirst ()
{
  double retval;

  double incr = _scope->getIncrement ();

  // the minimum is the left-most value and the maximum is the
  //   right-most value
  if (incr > 0) {
    // the increment is positive! thus the minimum value is less than
    //   the maximum value and the increment is positive
    retval = (double)_first < _scope->getMinimum() ?
      (float)_scope->getMinimum() : (double)_first > _scope->getMaximum() ?
      (float)_scope->getMaximum() : _first;
  }
  else {
    // the increment is negative! thus the minimum value is greater than
    //   the maximum value and the increment is negative
    retval = (double)_first > _scope->getMinimum() ?
      (float)_scope->getMinimum() : (double)_first < _scope->getMaximum() ?
      (float)_scope->getMaximum() : _first;
  }
  return retval;
}

double NDoTraceRow::fixLast ()
{
  double retval;

  double incr = _scope->getIncrement ();

  // the minimum is the left-most value and the maximum is the
  //   right-most value
  if (incr > 0) {
    // the increment is positive! thus the minimum value is less than
    //   the maximum value and the increment is positive
    retval = (double)_last < _scope->getMinimum() ?
      (float)_scope->getMinimum() : (double)_last > _scope->getMaximum() ?
      (float)_scope->getMaximum() : _last;
  }
  else {
    // the increment is negative! thus the minimum value is greater than
    //   the maximum value and the increment is negative
    retval = (double)_last > _scope->getMinimum() ?
      (float)_scope->getMinimum() : (double)_last < _scope->getMaximum() ?
      (float)_scope->getMaximum() : _last;
  }
  return retval;
}


TraceHeaderScope::TraceHeaderScope (int header)
{
  initialize ();
  if (header > 0) _hdr = header;
}

TraceHeaderScope::~TraceHeaderScope ()
{
}

TraceHeaderScope::TraceHeaderScope (TraceHeaderScope *scope)
{
  assert (set(scope,True));
}

void TraceHeaderScope::initialize ()
{
  _min = -1; // left-most header value
  _max = -2; // right-most header value
  _inc =  0; // increment from one header value to the next
  _run =  0; // number of traces before a value increment occurs
  _hdr =  0;
}

// caution: run does not figure into the validity
Boolean TraceHeaderScope::valid ()
{
  Boolean retval;

  if (_hdr > 0 && abs(_inc) > 0) {
    long tcount = getTrace (_max);

    double test = getValue (tcount);
    if (tcount < 1 || test != _max) {
      retval = False;
    }
    else {
      retval = True;
    }
  }
  else {
    retval = False;
  }
  return retval;
}

Boolean TraceHeaderScope::set (TraceHeaderScope *scope, Boolean insist)
{
  if (scope == NULL) return False;

  Boolean retval;

  if (scope->_hdr < 1) {
    if (insist) {
      initialize ();
      return True;
    }
    else {
      return False;
    }
  }

  if (insist) setHeader (scope->_hdr);
 
  if (_hdr != scope->_hdr) {
    retval = False;
  }
  else {
    set (scope->_min, scope->_max, scope->_inc, scope->_run);
    retval = True;
  }
  return retval;
}

void TraceHeaderScope::set (double min, double max, double inc, long run)
{
  _min = min;
  _max = max;
  _inc = inc;
  _run = run;
}

void TraceHeaderScope::setHeader (int header)
{
  _hdr = header;
}

int TraceHeaderScope::getHeader ()
{
  return _hdr;
}

long TraceHeaderScope::getTrace (double value)
{
  long retval = (long)((value - _min) / _inc + 1.5);
  return retval;
}

double TraceHeaderScope::getValue (long trace)
{
  double retval = (double)(trace - 1) * _inc + _min;
  return retval;
}

long TraceHeaderScope::getLength ()
{
  return getTrace (_max);
}

double TraceHeaderScope::getMinimum ()
{
  return _min;
}

double TraceHeaderScope::getMaximum ()
{
  return _max;
}

double TraceHeaderScope::getIncrement ()
{
  return _inc;
}

long TraceHeaderScope::getRun ()
{
  return _run;
}



TraceOrderAnalyzer::TraceOrderAnalyzer () :
  _header_scopes   (NULL),
  _abort_function  (NULL),
  _abort_data      (NULL),
  _st_mtime        (-1),
  _ok              (False),
  _filename        (NULL),
  _ntraces         (0),
  _count           (0),
  _alloc           (0)
{
}

TraceOrderAnalyzer::~TraceOrderAnalyzer ()
{
  free (_filename), _filename = NULL;

  int k2;
  for (k2 = 0; k2 < _count; k2++) {
    delete _header_scopes[k2], _header_scopes[k2] = NULL;
  }
  delete [] _header_scopes;
}

void TraceOrderAnalyzer::initialize ()
{
  int k2;
  for (k2 = 0; k2 < _count; k2++) {
    delete _header_scopes[k2], _header_scopes[k2] = NULL;
  }
  _abort_function = NULL;
  _abort_data = NULL;
  _st_mtime = -1;
  _ok = False;
  _ntraces = 0;
  _count = 0;
}

void TraceOrderAnalyzer::set (char *filename, long ntraces)
{
  Boolean do_init;

  if (filename != NULL && _filename != NULL) {
    if (strcmp(filename,_filename) != 0) {
      // changing the file
      free (_filename);
      _filename = (char *)malloc ((strlen(filename)+1)*sizeof(char));
      strcpy (_filename, filename);
      initialize ();
      _ntraces = ntraces;
    }
  }
  else if (filename == NULL) {
    if (_filename) {
      // clearing out a previous file, ignore the given ntraces
      free (_filename);
      _filename = NULL;
      initialize ();
    }
    assert (_count == 0);
  }
  else if (_filename == NULL) {
    // initializing _filename
    assert (_count == 0);
    _filename = (char *)malloc ((strlen(filename)+1)*sizeof(char));
    strcpy (_filename, filename);
    _ntraces = ntraces;
  }
}

// Warning: This function is not robust. It quickly returns True and
// (re)defines _filename if the given number of traces match _ntraces and the
// selected header values in the last trace match the current file, otherwise
// False is returned
Boolean TraceOrderAnalyzer::setOnProbableMatch (char *filename, long ntraces,
  NDoTraceSelection *select)
{
  Boolean ok;

  int *rows;
  Boolean gap_found;
  int count = select->getCount (&gap_found, &rows);
  long num_traces = select->getNumTraces ();
  if (count == 0 || filename == NULL || ntraces != num_traces) {
    // insist that headers have been selected, a file existed and the number
    //   of traces match
    ok = False;
  }
  else {
    // insist that the headers in the last trace match the select data
    int k2, err, open, nhwd, ns, itrace, lun;
    double hdr_vlu, *thdrs;
    float trace[1];
    NDoTraceRow *row;
    TF_Global *globals = workstation_globals_get_globals (filename, &err);
    int isa_jseis_file;
    if (globals) {
      // insist that the globals were found
      open = 1;
      nhwd = workstation_globals_get_nhdwd (globals);
      thdrs = (double *)malloc ((size_t)nhwd*sizeof(double));
      ns = 1;
      itrace = ntraces;
      // read the last trace headers
      /*printf ("TraceOrderAnalyzer::setOnProbableMatch\n");*/
      lun = jsfiles_getlun (filename, "r");
      jsfiles_open (lun);
      isa_jseis_file = jsfiles_isa (lun);
      if (!isa_jseis_file) {
        trciof77wrapper_get_trace_ (filename, &open, thdrs, trace, &itrace,
	  &lun, &err, &nhwd, &ns);
      }
      else {
        jsfiles_settracenumber (lun, itrace);
	jsfiles_getheaders (lun, thdrs, nhwd);
	err = jsfiles_status (lun);
      }
      open = 0;
      if (!err) {
	// insist that no error occurred reading the trace headers
	for (k2 = 0, ok = True; ok && k2 < count; k2++) {
	  row = select->getRow (rows[k2]);
	  hdr_vlu = thdrs[row->getScope()->getHeader()-1];
	  if (hdr_vlu != row->getScope()->getMaximum()) {
	    // insist the header values match the select data
	    ok = False;
	  }
	}
      }
      else {
	// error reading the trace headers
	ok = False;
      }
      // clean up
      err = locClose (lun);
      free (rows);
      free (globals);
      free (thdrs);
    }
    else {
      // the global data was not found
      ok = False;
    }
  }
  if (ok) {
    // all matching requirements were met, store the filename
    if (_filename != NULL) free (_filename);
    _filename = (char *)malloc ((strlen(filename)+1)*sizeof(char));
    strcpy (_filename, filename);
    _ntraces = ntraces;
  }
  return ok;
}

// check the given headers to make sure they are unqiue and that the data is
//   regularized.
// 
// returned in the data_order array is merely indicies that correspond to the
//   order of the given headers array.
// a value of NULL in the data_order array means only check the given headers
//   for regularization 
Boolean TraceOrderAnalyzer::ok (int count, int *headers,
  NDoTraceSelection *select, int *user_aborted)
{
  if (_filename == NULL || _ntraces < 1) return False;

  struct stat *status = (struct stat *)malloc (sizeof(struct stat));
  int error = stat (_filename, status);
  if (error) {
    _ok = False;
    _st_mtime = -1;
    free (status);
    return False;
  }
  else {
    if (status->st_mtime != _st_mtime) {
      _ok = False;
      if (status->st_mtime != -1) _st_mtime = status->st_mtime;
    }
    else {
      _ok = True;
    }
  }
  free (status);

  // insist that the headers are unique
  int k2, k3;
  Boolean ok;
  for (k2 = 0, ok = True; ok && k2 < count; k2++) {
    for (k3 = 0; ok && k3 < count; k3++) {
      if (k2 != k3) {
	if (headers[k2] == headers[k3]) {
	  return False;
	}
      }
    }
  }
  // if ok is True then the headers are unique as required

  int *ldo = NULL;
  TraceHeaderScope **header_scopes = new TraceHeaderScope*[count];
  Boolean *doit   = (Boolean *)malloc (count*sizeof(Boolean));
  Boolean *finc   = (Boolean *)malloc (count*sizeof(Boolean));
  double *hdr_min = (double  *)malloc (count*sizeof(double ));
  double *hdr_max = (double  *)malloc (count*sizeof(double ));
  double *hdr_inc = (double  *)malloc (count*sizeof(double ));
  int    *hdr_run = (int     *)malloc (count*sizeof(double ));

  int doit_count;
  Boolean found;
  for (k2 = 0, doit_count = 0; k2 < count; k2++) {
    // _count is the number of existing _header_scopes
    for (k3 = 0, found = False; !found && k3 < _count; k3++) {
      if (headers[k2] == _header_scopes[k3]->getHeader()) {
	header_scopes[k2] = new TraceHeaderScope (_header_scopes[k3]);
	found = True;
	doit[k2] = !_ok;
	finc[k2] = _ok;
      }
    }
    if (!found) {
      header_scopes[k2] = new TraceHeaderScope (headers[k2]);
      doit[k2] = True;
      finc[k2] = False;
    }
    if (doit[k2]) doit_count++;
  }

  Boolean file_is_opened = False;
  double *thdrs = NULL;
  float trace[1];
  int err, open, nhwd, ns, lun, itrace;
  int isa_jseis_file;

  if (doit_count > 0) {
    _ok = False; // even if the file has been analyzed before, there are now
                 // more headers to analyze than before, so reread
    // as necessary gather data from seismic file
    TF_Global *globals = workstation_globals_get_globals (_filename, &err);
    if (globals) {
      file_is_opened = True;
      open = 1;
      nhwd = workstation_globals_get_nhdwd (globals);
      if (!strcmp(globals->ftyp,"JSEIS")) {
	isa_jseis_file = 1;
        /*printf ("TraceOrderAnalyzer::ok\n");*/
        lun = jsfiles_getlun (_filename, "r");
        jsfiles_open (lun);
	err = jsfiles_status (lun);
	ok = lun > 0 && err == 0;
      }
      else {
	isa_jseis_file = 0;
      }
      thdrs = (double *)malloc ((size_t)nhwd*sizeof(double));
      double hdr_vlu;
      int domore;
      ns = 1;
      for (k2 = 0, domore = doit_count; !(*user_aborted) &&
        ok && domore > 0 && k2 < _ntraces; k2++) {
	itrace = k2 + 1;
	// read the itraceth trace headers...;
	if (!isa_jseis_file) {
	  trciof77wrapper_get_trace_ (_filename, &open, thdrs, trace, &itrace,
	    &lun, &err, &nhwd, &ns);
	}
	else {
          jsfiles_settracenumber (lun, (long)itrace);
	  jsfiles_getheaders (lun, thdrs, nhwd);
	  err = jsfiles_status (lun);
	}
	open = 0;
	if (!err) {
	  for (k3 = 0; domore > 0 && k3 < count; k3++) {
	    if (doit[k3] && !finc[k3]) {
	      hdr_vlu = thdrs[headers[k3]-1];
	      if (k2 > 0) {
		if (hdr_vlu != hdr_min[k3]) {
		  hdr_inc[k3] = hdr_vlu - hdr_min[k3];
		  hdr_run[k3] = k2;
		  finc[k3] = True;
		  domore--;
		}
	      }
	      else {
		hdr_min[k3] = hdr_vlu;
	      }
	    }
	  }
          if (_abort_function && _abort_data) {
	    *user_aborted = _abort_function (_abort_data);
	    if ( *user_aborted ) {
	      err = locClose (lun);
	      ok = False;
	      file_is_opened = False;
	    }
	  }
	}
	else {
	  ok = False;
	  err = locClose (lun);
	  file_is_opened = False;
	}
      }
      if (ok && domore == 0) {
	if (!isa_jseis_file) {
	  trciof77wrapper_get_trace_ (_filename, &open, thdrs, trace,
            &_ntraces, &lun, &err, &nhwd, &ns);
	}
	else {
          jsfiles_settracenumber (lun, (long)_ntraces);
	  jsfiles_getheaders (lun, thdrs, nhwd);
	  err = jsfiles_status (lun);
	}
	for (k2 = 0; ok && k2 < count; k2++) {
	  if (doit[k2]) {
	    hdr_vlu = thdrs[headers[k2]-1];
	    header_scopes[k2]->set (hdr_min[k2], hdr_vlu, hdr_inc[k2],
	      hdr_run[k2]);
	    if (header_scopes[k2]->valid()) {
	      add (header_scopes[k2]);
	    }
	    else {
	      err = locClose (lun);
	      ok = False;
	      file_is_opened = False;
	    }
	  }
	}
      }
      else {
	// insist that no error occurs reading the sesimic traces and that
	//   all the necessary inc's were found
	ok = False;
      }
      free (globals);
    }
    else {
      // insist that the global data was found
      ok = False;
    }
  }

  int run_k2;
  if (ok) {
    // create the data order array
    ldo = (int *)malloc (count*sizeof(int));
    for (k2 = 0; k2 < count; k2++) {
      ldo[k2] = k2;
    }
    // sort the order array from smallest run to largest run
    int tmp, run_k3;
    for (k2 = 0; ok && k2 < count; k2++) {
      run_k2 = header_scopes[ldo[k2]]->getRun ();
      for (k3 = k2; ok && k3 < count; k3++) {
	if (k2 != k3) {
	  run_k3 = header_scopes[ldo[k3]]->getRun ();
	  if (run_k2 == run_k3) {
	    // two header runs MUST NOT be the same!
	    ok = False;
	  }
	  else if (run_k2 > run_k3) {
	    tmp     = ldo[k2];
	    ldo[k2] = ldo[k3];
	    ldo[k3] = tmp;
	  }
	}
      }
    }
  }

  if (ok && select) {
    if (file_is_opened) {
      // insist that the runs of the headers fit within a whole
      //   multiple of each other in order
      // also insist that the predictable maximum run values are precise
      TraceHeaderScope *hsc;
      int mul, inc, max, hdr, vlu;
      run_k2 = header_scopes[ldo[0]]->getRun ();
      if (run_k2 > 0) {
	int run_k2m1, mul;
	for (k2 = 1; ok && k2 < count; k2++) {
	  run_k2m1 = run_k2;
	  run_k2 = header_scopes[ldo[k2]]->getRun ();
	  if (run_k2 > 0) {
	    mul = run_k2 / run_k2m1;
	    ok = mul * run_k2m1 == run_k2;
	    if (ok) {
	      hsc = header_scopes[ldo[k2-1]];
	      inc = hsc->getIncrement ();
	      hdr = hsc->getHeader    ();
	      max = (int)hsc->getValue (mul);
	      if (!isa_jseis_file) {
		trciof77wrapper_get_trace_ (_filename, &open, thdrs, trace,
		  &run_k2, &lun, &err, &nhwd, &ns);
	      }
	      else {
                jsfiles_settracenumber (lun, (long)run_k2);
		jsfiles_getheaders (lun, thdrs, nhwd);
		err = jsfiles_status (lun);
	      }
	      vlu = (int)thdrs[hdr-1];
	      ok = vlu == max;
	    }
	  }
	  else {
	    ok = False;
	  }
	}
	if (ok) {
	  mul = _ntraces / run_k2;
	  ok = mul * run_k2 == _ntraces;
	  if (ok) {
	    hsc = header_scopes[ldo[k2-1]];
	    inc = hsc->getIncrement ();
	    hdr = hsc->getHeader    ();
	    max = (int)hsc->getValue (mul);
	    if (!isa_jseis_file) {
	      trciof77wrapper_get_trace_ (_filename, &open, thdrs, trace,
                &_ntraces, &lun, &err, &nhwd, &ns);
	    }
	    else {
              jsfiles_settracenumber (lun, (long)_ntraces);
	      jsfiles_getheaders (lun, thdrs, nhwd);
	      err = jsfiles_status (lun);
	    }
	    vlu = (int)thdrs[hdr-1];
	    ok = vlu == max;
	  }
	}
      }
      else {
	ok = False;
      }
    }
    select->setScopes (count, header_scopes);

    for (k2 = 0; k2 < count; k2++) {
      delete header_scopes[k2];
    }
    delete [] header_scopes;
  }

  if (thdrs) free (thdrs);
  if (file_is_opened) {
    err = locClose (lun);
    file_is_opened = False;
  }

  if (ldo) free (ldo);

  free (doit);
  free (finc);
  free (hdr_min);
  free (hdr_max);
  free (hdr_inc);
  free (hdr_run);

  _ok = ok;
  return _ok;
}

void TraceOrderAnalyzer::setAbortFunction (AbortFunction function,
  void *abort_data)
{
  _abort_function = function;
  _abort_data = abort_data;
}

#undef INCR
#define INCR 5
void TraceOrderAnalyzer::add (TraceHeaderScope *scope)
{
  if (_count == _alloc) {
    _alloc += INCR;
    TraceHeaderScope **tmp = new TraceHeaderScope*[_alloc];
    int k2;
    for (k2 = 0; k2 < _count; k2++) {
      tmp[k2] = _header_scopes[k2];
    }
    for (; k2 < _alloc; k2++) {
      tmp[k2] = NULL;
    }
    if (_count > 0) {
      delete [] _header_scopes;
    }
    _header_scopes = tmp;
  }
  _header_scopes[_count++] = new TraceHeaderScope (scope);
}

int TraceOrderAnalyzer::locClose (int lun)
{
  int retval;

  /*printf ("TraceOrderAnalyzer::locClose: calling jsfiles_close\n");*/
  if (jsfiles_close(lun) == 1) {
    retval = 0;
  }
  else {
    trciof77wrapper_close_file_ (&lun, &retval);
  }
  return retval;
}




NDoTraceFind::NDoTraceFind ()
{
  initialize ();
}

NDoTraceFind::~NDoTraceFind ()
{
}

NDoTraceFind::NDoTraceFind (NDoTraceFind *results)
{
  set (results);
}

void NDoTraceFind::set (NDoTraceFind *results)
{
  if (results) {
    _traces_found         = results->_traces_found;
    _frames_found         = results->_frames_found;
    _traces_in_last_frame = results->_traces_in_last_frame;
  }
  else {
    initialize ();
  }
}

void NDoTraceFind::initialize ()
{
  _traces_found         = 0;
  _frames_found         = 0;
  _traces_in_last_frame = 0;
}

void NDoTraceFind::setTraces (long traces_found)
{
  _traces_found = traces_found;
}

void NDoTraceFind::setFrames (long frames_found)
{
  _frames_found = frames_found;
}

void NDoTraceFind::setLast (long traces_in_last_frame)
{
  _traces_in_last_frame = traces_in_last_frame;
}

void NDoTraceFind::setFirst (long traces_in_first_frame)
{
  _traces_in_first_frame = traces_in_first_frame;
}

long NDoTraceFind::getTraces ()
{
  return _traces_found;
}

long NDoTraceFind::getFrames ()
{
  return _frames_found;
}

long NDoTraceFind::getLast ()
{
  return _traces_in_last_frame;
}

long NDoTraceFind::getFirst ()
{
  return _traces_in_first_frame;
}
