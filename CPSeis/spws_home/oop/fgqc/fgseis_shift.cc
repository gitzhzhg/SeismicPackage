// Author Michael L. Sherrill 08/94
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
// Provides linear (e.g. offset) and non-linear data shifts for SeisPlot


#include "fgqc/fgseis_shift.hh"
#include "geom/field_geometry.hh"


extern "C" {
void statcc_c(float shift,long nsamp,
              float bytes_floated[],
              float float_shifted[]);
}




//***************************** Contructor ***************************/
FgSeisShift::FgSeisShift( SeisPlot *sp, FgSeisOvjdPop *fop,
                          FieldGeometry  *fg, float /*velocity*/,
                          int /*header*/)
                    : SeisShift(sp)

{
  _fop = fop;
  _fg  = fg;
}


//********************** Linear (e.g. offset) Shifts **********************/
Boolean FgSeisShift::linearShift(Boolean forward, Boolean make_plot)
{
 long i,j;
 float shift_samples;
 const float *hd = _sp->headers();
 unsigned char *bytes = _sp->byteTraceDataForUpdate();
 float *floats = _sp->floatTraceDataForUpdate();
 long nsamp = _sp->samplesPerTrace();
 Boolean float_data = False;
 SeisPlot *lsp;
 float header_val;
 long primary_index, secondary_index;
 int error;
 long trace;

   
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
     header_val = 0.0;
     if(_fop->matchHeaderMode())//use seismic headers
       {
       primary_index=   i*_sp->numHeaders() + _fop->primaryHeader()  - 1;
       secondary_index= i*_sp->numHeaders() + _fop->secondaryHeader()- 1;
      if(!_fop->secondaryHeader())// using only one header to match
        {
        error = _fg->calculateHeaderWords((long)hd[primary_index],False);
        }
      else//using header pairs to match
        {
        trace = _fg->findTraceNumber((long)hd[primary_index],
                                (long)hd[secondary_index]);
        error = _fg->calculateHeaderWords(trace,False);
        }
       if(!error) header_val = _fg->getHeaderWordValue(_header);
       }
     else//use label skip
       {
       _fg->startHeadersFromScratch(); 
       _fg->calculateHeaderWords(i+1+_fop->skipHeaders(),False);  
       header_val = _fg->getHeaderWordValue(_header);
       }
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
       for(j=0;j<nsamp;j++)
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
Boolean FgSeisShift::nonlinearShift(Boolean forward, float *shifts_in, 
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
 float header_val;
 long primary_index, secondary_index; 
 int error;
 long trace;

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
     header_val = 0.0;
     if(shifts_in == NULL)
       {
       if(_fop->matchHeaderMode())//use seismic headers
         {
         primary_index=   i*_sp->numHeaders() + _fop->primaryHeader()  - 1;
         secondary_index= i*_sp->numHeaders() + _fop->secondaryHeader()- 1;
         if(!_fop->secondaryHeader())// using only one header to match
           {
           error = _fg->calculateHeaderWords((long)hd[primary_index],False);
           }
         else//using header pairs to match
           {
           trace = _fg->findTraceNumber((long)hd[primary_index],
                                   (long)hd[secondary_index]);
           error = _fg->calculateHeaderWords(trace,False);
           }
         if(!error)
           {
           header_val = _fg->getHeaderWordValue(_header);
           _shifts[i] = header_val / _divisor - _flatten_to_time;  
           }
         }
       else//use label skip
         {
         _fg->startHeadersFromScratch();
         _fg->calculateHeaderWords(i+1+_fop->skipHeaders(),False);  
         header_val = _fg->getHeaderWordValue(_header);
         _shifts[i] = header_val / _divisor - _flatten_to_time; 
         }
       }
     else
       {
       _shifts[i] = shifts_in[i] - _flatten_to_time;
       }
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
       for(j=0;j<nsamp;j++)
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


