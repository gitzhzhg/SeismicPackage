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
// Author Michael L. Sherrill 08/94
// Provides linear (e.g. offset) and non-linear data shifts for SeisPlot


#include "pick/seis_shift.hh"

/*
#if (ultrix || sun || __sgi)
#define statcc statcc_
#elif CRAY
#define statcc STATCC
#endif

extern "C" {
void statcc(float *shift,long *nsamp,
            float bytes_floated[],
            float float_shifted[]);
}
*/

extern "C" {
void statcc_c(float shift,long nsamp,
              float bytes_floated[],
              float float_shifted[]);
}




//***************************** Contructor ***************************/
SeisShift::SeisShift( SeisPlot *sp, float velocity, int header)
                    : SeisInform(sp), _sp(sp), _velocity(velocity),
                      _header(header), _shift_reploted(False)

{
  _bytes_floated  = NULL;
  _floats_shifted = NULL;
  _shifts         = NULL;
  _forward        = False;
  _reverse        = False;
  _data_shifted   = False;
  _divisor        = 1000.0; 
  _flatten_to_time= 0.20;
  addSP(_sp);
}

//***************************** Destructor ***************************/
SeisShift::~SeisShift()
{
  if(_bytes_floated  != NULL) free(_bytes_floated );
  if(_floats_shifted != NULL) free(_floats_shifted);
  if(_shifts         != NULL) free(_shifts);
}


//************************** SeisInform Reactions *********************/
void SeisShift::newPlot(SeisPlot *)
{

  if(_shift_reploted)
     {
     _shift_reploted = False;
     }
  else
     {
     noShift();
     }
}


void SeisShift::preScan(SeisPlot *, SeisPlot::ScanDir)
{
  noShift();
  _can_scan_shift = True;
}


void SeisShift::postScan(SeisPlot *, SeisPlot::ScanDir)
{
  //noShift();
  _can_scan_shift = False;
}


void SeisShift::postZoomSeparateWindow(SeisPlot *, SeisPlot * /*separate_sp*/)
{
   //addSP(separate_sp); not used since separate window is destroyed on new plot
}


void SeisShift::addSP(SeisPlot *newsp)
{
  if(_list.find(newsp) == NULL) _list.add(newsp);
}


void SeisShift::noPlotDisplayed(SeisPlot *)
{
  noShift();
}


void SeisShift::noShift()
{
  setForwardApplied(False);
  setReverseApplied(False);
  _data_shifted = False;
  if(_shifts != NULL) 
     for(int i = 0; i < _num_shifts; i++)
        _shifts[i]=0.0;
}
 

//********************** Linear (e.g. offset) Shifts **********************/
Boolean SeisShift::linearShift(Boolean forward, Boolean make_plot)
{
 long i, j;
 float shift_samples;
 const float *hd = _sp->headers();
 unsigned char *bytes = _sp->byteTraceDataForUpdate();
 float *floats = _sp->floatTraceDataForUpdate();
 long nsamp = _sp->samplesPerTrace();
 Boolean float_data = False;
 SeisPlot *lsp;
 float header_val;
   
  if( _sp->imageIsDisplayed() == False  ) return(False);

  //if this function is being called by image and the _can_scan_shift
  //flag has not been set in the preScan inform method just return
  //because this is probably being called by the setup/change plot menu
  //instead of the image scan .
  if( make_plot == False && _can_scan_shift == False ) return(False);


  if(bytes == NULL)
     {
     float_data = True;
     if(floats == NULL) return(False);
     }
  
  if(dataShifted())
       if(!removeShift(False))
            return(False);

  _num_shifts = _sp->memoryTraces() * _sp->plottedFrames();

  _bytes_floated = (float *)malloc((unsigned int)(nsamp*sizeof(float)));
  if(_bytes_floated == NULL)
     {
     return(False);
     } 

  _floats_shifted= (float *)malloc((unsigned int)(nsamp*sizeof(float)));
  if(_floats_shifted == NULL)
     {
     free(_bytes_floated);
     _bytes_floated = NULL;
     return(False);
     }

  if(_shifts == NULL)
     _shifts = (float *)malloc((unsigned int)(_num_shifts*sizeof(float)));
  else
     _shifts = (float *)realloc(_shifts,(unsigned int)(_num_shifts
                                                       *sizeof(float)));
  if(_shifts == NULL)
     {
     free(_bytes_floated);
     _bytes_floated = NULL;
     free(_floats_shifted);
     _floats_shifted = NULL;
     return(False);
     }
  

  for(i = 0; i < _num_shifts; i++)
     {
     header_val = hd[i*_sp->numHeaders()+_header-1];
     if(header_val < 0.0) header_val = (-header_val);//make neg offsets pos.
     _shifts[i]=(header_val - _sp->memTmin()) / _velocity
               - _flatten_to_time;
     shift_samples = _shifts[i] / _sp->srval();
     shift_samples = forward ? shift_samples : -shift_samples;
     if(!float_data)
       {
       convert_byte_to_float(nsamp, &bytes[i*nsamp], &_bytes_floated[0]);
       statcc_c(shift_samples,nsamp,&_bytes_floated[0],&_floats_shifted[0]);
       return_float_to_byte(nsamp,&_floats_shifted[0], &bytes[i*nsamp]);
       }
     else
       {
       statcc_c(shift_samples,nsamp,&floats[i*nsamp],&_floats_shifted[0]);
       for(j = 0; j < nsamp; j++)
          floats[i*nsamp+j] = _floats_shifted[j];
       }
       
     }

  free(_bytes_floated);
  free(_floats_shifted);
  _bytes_floated = NULL;
  _floats_shifted = NULL;
 
  _shift_type = LINEAR_SHIFT;
  setShiftInfo(forward);  


  if(make_plot)
    {
    for(lsp = _list.top(); lsp; lsp = _list.next() )
      {
      if( lsp->isPlotDisplayed() )
        {	
        if(lsp->isZoomed())
           lsp->replotZoom();
        else
           lsp->plot();
        }
      }
    }
 
  return(True);
}


//*********** Nonlinear (e.g. individual header values) Shift ******************
Boolean SeisShift::nonlinearShift(Boolean forward, float *shifts_in, 
                                  Boolean make_plot)
{
 long i,j;
 float shift_samples;
 const float *hd = _sp->headers();
 unsigned char *bytes = _sp->byteTraceDataForUpdate();
 float *floats = _sp->floatTraceDataForUpdate();
 long nsamp = _sp->samplesPerTrace();
 Boolean float_data = False;
 SeisPlot *lsp;
 

  

  if( _sp->imageIsDisplayed() == False )return(False);

  //if this function is being called by image and the _can_scan_shift
  //flag has not been set in the preScan inform method just return
  //because this is probably being called by the setup/change plot menu
  //instead of the image scan.
  if( make_plot == False && _can_scan_shift == False ) return(False);

  if(bytes == NULL)
     {
     float_data = True;
     if(floats == NULL) return(False);
     }

  if(dataShifted())
      if(!removeShift(False))
            return(False);

  _num_shifts = _sp->memoryTraces() * _sp->plottedFrames();

  _bytes_floated = (float *)malloc((unsigned int)(nsamp*sizeof(float)));
  if(_bytes_floated == NULL)
     {
     return(False);
     }

  _floats_shifted= (float *)malloc((unsigned int)(nsamp*sizeof(float)));
  if(_floats_shifted == NULL)
     {
     free(_bytes_floated);
     _bytes_floated = NULL;
     return(False);
     }

  if(_shifts == NULL)
     _shifts = (float *)malloc((unsigned int)(_num_shifts*sizeof(float)));
  else
     _shifts = (float *)realloc(_shifts,(unsigned int)(_num_shifts
                                                       *sizeof(float)));
  if(_shifts == NULL)
     {
     free(_bytes_floated);
     _bytes_floated = NULL;
     free(_floats_shifted);
     _floats_shifted = NULL;
     return(False);
     }


  for(i = 0; i < _num_shifts; i++)
     {
     if(shifts_in == NULL)
       _shifts[i] = hd[i*_sp->numHeaders()+_header-1] / _divisor 
                  - _flatten_to_time; 
     else
       _shifts[i] = shifts_in[i] - _flatten_to_time;
     shift_samples =_shifts[i] / _sp->srval();
     shift_samples = forward ? shift_samples : -shift_samples;
     if(!float_data)
       {
       convert_byte_to_float(nsamp, &bytes[i*nsamp], &_bytes_floated[0]);
       statcc_c(shift_samples,nsamp,&_bytes_floated[0],&_floats_shifted[0]);
       return_float_to_byte(nsamp,&_floats_shifted[0], &bytes[i*nsamp]);
       }
     else
       {
       statcc_c(shift_samples,nsamp,&floats[i*nsamp],&_floats_shifted[0]);
       for(j = 0; j < nsamp; j++)
          floats[i*nsamp+j] = _floats_shifted[j];
       }
     }

  free(_bytes_floated);
  _bytes_floated = NULL;
  free(_floats_shifted); 
  _floats_shifted = NULL;

  _shift_type = NONLINEAR_SHIFT;
  setShiftInfo(forward);

  if(make_plot)
    {
    for(lsp = _list.top(); lsp; lsp = _list.next() )
      {
      if( lsp->isPlotDisplayed() )
        {	
        if(lsp->isZoomed())
          lsp->replotZoom();
        else
          lsp->plot();
        }
      }
    }


  return(True);
}


//************************** Remove Shifts **********************************
Boolean SeisShift::removeShift(Boolean replot)
{
 long i,j;
 float shift_samples;
 Boolean forward;
 long nsamp = _sp->samplesPerTrace();
 unsigned char *bytes = _sp->byteTraceDataForUpdate();
 float *floats = _sp->floatTraceDataForUpdate();
 Boolean float_data = False;
 SeisPlot *lsp;
 
  if( _sp->imageIsDisplayed() == False ) return(False);

  if(bytes == NULL)
    {
     float_data = True;
     if(floats == NULL) return(False);
   }

  if(!dataShifted())return(False);

  forward = reverseApplied() ? True : False; 

  _bytes_floated = (float *)malloc((unsigned int)(nsamp*sizeof(float)));
  if(_bytes_floated == NULL)
    {
     return(False);
    }

  _floats_shifted= (float *)malloc((unsigned int)(nsamp*sizeof(float)));
  if(_floats_shifted == NULL)
    {
     free(_bytes_floated);
     _bytes_floated = NULL;
     return(False);
   }



  for(i = 0; i < _num_shifts; i++)
    {
     shift_samples = _shifts[i] / _sp->srval();
     shift_samples = forward ? shift_samples : -shift_samples;
     if(!float_data)
       {
       convert_byte_to_float(nsamp, &bytes[i*nsamp], &_bytes_floated[0]);
       statcc_c(shift_samples,nsamp,&_bytes_floated[0],&_floats_shifted[0]);
       return_float_to_byte(nsamp,&_floats_shifted[0], &bytes[i*nsamp]);
       }
     else
       {
       for(j = 0; j < nsamp; j++)
         _bytes_floated[j] = floats[i*nsamp+j];
       statcc_c(shift_samples,nsamp,&_bytes_floated[0],&_floats_shifted[0]);
       for(j = 0; j < nsamp; j++)
          floats[i*nsamp+j] = _floats_shifted[j];
       }
   }

  free(_bytes_floated);
  _bytes_floated = NULL;
  free(_floats_shifted);
  _floats_shifted = NULL;

  noShift();  

  if(replot) 
     {
     for(lsp = _list.top(); lsp; lsp = _list.next() )
        {
        if( lsp->isPlotDisplayed() )
           {	
           if(lsp->isZoomed())
              lsp->replotZoom();
           else
              lsp->plot();
           }
        }
     }


  return(True);
}


//********************* Return an applied shift to app **********************
float SeisShift::getShift(long memtrace)
{
  if( (dataShifted() == False) || (memtrace > _num_shifts) ) return(0.0);
  return(_shifts[memtrace-1]);
}


//********************* Define current shift status *************************
void SeisShift::setShiftInfo(Boolean forward)
{
  if(forward)
     {
     if(!dataShifted())
        {
        _data_shifted = True;
        setForwardApplied(True);
        setReverseApplied(False);
        }
     else
        {
        _data_shifted = False;
        setForwardApplied(False);
        setReverseApplied(False);
        }
     }
  else
     {
     if(!dataShifted())
        {
        _data_shifted = True;
        setForwardApplied(False);
        setReverseApplied(True);
        }
     else
        {
        _data_shifted = False;
        setForwardApplied(False);
        setReverseApplied(False);
        }
     }

  _shift_reploted = True;

}
