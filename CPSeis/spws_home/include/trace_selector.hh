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
//========== Michael L. Sherrill 02/26/01                         ===========
//===========================================================================

#ifndef _TRACE_SELECTOR
#define _TRACE_SELECTOR

#include "plot_image.hh"


typedef int (*AbortFunction)(void*);

class PatternReader {
  
  public:
    PatternReader (int pixmap_number);
    ~PatternReader ();
    Boolean assignTraceNumbers (long num_traces);
    Boolean incrementTraceNumbers (long count);
    long getNumTraces ();
    void resetNumTraces () {_num_traces = 0;}
    long *getTraceNumbers () {return _trace_numbers;}

    
  private:
    long
      *_trace_numbers,
      _num_alloc,
      _num_traces;
  
    int
      _pixmap_number;


};


class TraceSelector {
  
  public:
    TraceSelector();
    ~TraceSelector();
    long *getTraceNumbers(int which);
    long getNumTraces(int which);
    void resetNumTraces(int pixmamp);

    long findTraces
      (class NDoTraceSelection *select,
       long nplt = 1,
       long iskp = 0,
       long ndo = 1,
       long nskp = 0,
       int pixmap = 0);

    long findTraces(char *filename,              int primary_header = 0,
                    float primary_min = 0.0,     float primary_max = 0.0,
                    float primary_inc = 0.0,     int secondary_header = 0, 
                    float secondary_min = 0.0,   float secondary_max = 0.0,
                    float secondary_inc = 0.0,   int tertiary_header = 0,
                    float tertiary_min  = 0.0,   float tertiary_max = 0.0,
                    float tertiary_inc  = 0.0,   long nplt = 1, 
                    long iskp = 0,               long ndo = 1,   
                    long nskp = 0,               long still_need = 1, 
                    int pixmap = 0,              int open_file = 1,     
                    int nhdrs = 64,              int *user_aborted = 0,  
                    int *lun = 0);

    long findTracesByPattern(char *filename,        int xheader, 
                             int yheader,           long num_traces,
                             long                   nhdrs,
                             long *num_gathers,     int *user_aborted, 
                             float *xloc =NULL,     float *yloc = NULL,
                             float *vel_min = NULL, float *vel_max = NULL,
                             float *min_x = NULL,   float *max_x = NULL); 

    Boolean setTrace(int pixmap, long trace);
    int   primaryHeader(){return _primary_header;}    
    float primaryMin(){return _primary_min;}
    float primaryMax(){return _primary_max;}
    float primaryInc(){return _primary_inc;}
    int   secondaryHeader(){return _secondary_header;}    
    float secondaryMin(){return _secondary_min;}
    float secondaryMax(){return _secondary_max;}
    float secondaryInc(){return _secondary_inc;}
    int   tertiaryHeader(){return _tertiary_header;}    
    float tertiaryMin(){return _tertiary_min;}
    float tertiaryMax(){return _tertiary_max;}
    float tertiaryInc(){return _tertiary_inc;}
    void setTraceSelectorAbortFunction(AbortFunction func, void *data)
      {_abort_function = func; _abort_data = data;}

    enum{USER_ABORTED = 1, PAST_USER_LIMITS = 2};


  protected:
    AbortFunction _abort_function;
    void          *_abort_data;
    Boolean       isBin(float min, float max, float inc, float v);

  private:
    int locClose
      (int lun,
       char *msg,
       int len);

    PatternReader **_pattern_readers;
    long          _num_traces_found;
    int           _primary_header;
    float         _primary_min;
    float         _primary_max;
    float         _primary_inc;
    int           _secondary_header;
    float         _secondary_min;
    float         _secondary_max;
    float         _secondary_inc;
    int           _tertiary_header;
    float         _tertiary_min;
    float         _tertiary_max;
    float         _tertiary_inc;
    

};


class NDoTraceSelection
{
public:
  NDoTraceSelection
    (int max_count);

  ~NDoTraceSelection ();

  NDoTraceSelection
    (NDoTraceSelection *select);

  void set
    (NDoTraceSelection *select);

  Boolean valid ();

  int scanFrame
    (Boolean next = True,
     Boolean exact = False,
     int frames = 0);

  void setRows
    (NDoTraceSelection *select);

  int getCount
    (Boolean *gap_found = NULL,
     int **irows = NULL,
     Boolean sort = False);

  int getHeaderCount
    (int **headers = NULL,
     Boolean ignore_gap = False);

  long getNumTraces ();

  void setScopes
    (int count,
     class TraceHeaderScope **scopes);

  Boolean setDataOrder ();

  class NDoTraceRow *getRow
    (int index);

  long setNPlotDefault
    (long def_nplot);

  long getNPlot ();

  int verifyNFrames
    (int frames,
     int *last_frame_size);

  void clearList ();

  long getList
    (long **list);

  void setAbortFunction
    (AbortFunction function,
     void *abort_data);

  AbortFunction getAbortFunction ();

  void *getAbortData ();

private:
  void constructorHelper
    (int max_count);

  void initialize ();

  long findList ();

  long getTrace
    (long trace,
     int dim);

  void cleanFindLists ();

  NDoTraceRow
    **_rows;

  AbortFunction
    _abort_function;

  void
    *_abort_data;

  long
    **_tlists,
    *_nplots,
    *_list,
    _list_size;

  int
    *_disp_order,
    *_data_order,
    *_irows,
    _max_count,
    _count;
    
};


class NDoTraceRow
{
public:
  NDoTraceRow ();

  ~NDoTraceRow ();

  NDoTraceRow
    (NDoTraceRow *row);

  void set
    (NDoTraceRow *row);

  int scanFrame
    (Boolean next = True,
     Boolean exact= False);

  void initialize ();

  int valid ();

  int getOrder ();

  float getFirst ();

  float getLast ();

  long getNDo ();

  long getNSkip ();

  long getNPlot ();

  Boolean adjustUsingNPlot
    (long *nplot);

  long getTraceList
    (long **tlist);

  long getTrace
    (double value);

  double getValue
    (long trace);

  void setOrder
    (int order);

  void setFirst
    (float first);

  void setLast
    (float last);

  void setNDo
    (long ndo);

  void setNSkip
    (long nskip);
  
  int validPattern
    (Boolean exact = False);

  class TraceHeaderScope *getScope ();

  enum {
    NO_ERROR,
    FIXED_ERROR,
    ERROR
  };

private:
  Boolean findClosestNSkip
    (long *nplot);

  Boolean findClosestLast
    (long *nplot);

  Boolean findClosestFirst
    (long *nplot);

  Boolean hasNext
    (double value);

  double fixFirst ();

  double fixLast ();

  enum {
    NONE,
    ORDER,
    FIRST,
    LAST,
    NDO,
    NSKIP
  };

  TraceHeaderScope
    *_scope;

  float
    _first,
    _last;

  int
    _order,
    _cur;

  long
    _ndo,
    _nskip;

};

class TraceHeaderScope
{
public:
  TraceHeaderScope
    (int header = 0);

  ~TraceHeaderScope ();

  TraceHeaderScope
    (TraceHeaderScope *scope);

  void initialize ();

  Boolean valid ();

  Boolean set
    (TraceHeaderScope *scope,
     Boolean insist = False);

  void set
    (double min,
     double max,
     double inc,
     long run);

  void setHeader
    (int header);

  int getHeader ();

  long getTrace
    (double value);

  double getValue
    (long trace);

  long getLength ();

  double getMinimum ();

  double getMaximum ();

  double getIncrement ();

  long getRun ();

private:
  double
    _min,
    _max,
    _inc;

  int
    _hdr;

  long
    _run;
};

class TraceOrderAnalyzer
{
public:
  TraceOrderAnalyzer ();

  ~TraceOrderAnalyzer ();

  void initialize ();

  void set
   (char *filename = NULL,
    long ntraces = 0);

  Boolean setOnProbableMatch
    (char *filename,
     long ntraces,
     NDoTraceSelection *select);

  Boolean ok
    (int count,
     int *headers,
     NDoTraceSelection *select,
     int *user_aborted);

  void setAbortFunction
    (AbortFunction function,
     void *abort_data);

private:
  void add
    (TraceHeaderScope *scope);

  int locClose
    (int lun);

  TraceHeaderScope
    **_header_scopes;

  AbortFunction
    _abort_function;

  void
    *_abort_data;

  time_t
    _st_mtime;

  Boolean
    _ok;

  char *
    _filename;

  int
    _ntraces,
    _count,
    _alloc;
};




class NDoTraceFind {
public:
  NDoTraceFind ();

  ~NDoTraceFind ();

  NDoTraceFind
    (NDoTraceFind *results);

  void set
    (NDoTraceFind *results);

  void initialize ();

  void setTraces
    (long traces_found);

  void setFrames
    (long frames_found);

  void setLast
    (long traces_in_last_frame);

  void setFirst
    (long traces_in_first_frame);

  long getTraces ();

  long getFrames ();

  long getLast ();

  long getFirst ();

private:
  long
    _traces_found,
    _frames_found,
    _traces_in_first_frame,
    _traces_in_last_frame;
};

#endif
