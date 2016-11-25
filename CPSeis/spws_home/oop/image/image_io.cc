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
/*------------------------------------------------------------------
 *USER DOC
 *Name   : imageIO 
 *Purpose: Reads file data needed for image generation 
 *         
 *Author : Michael L. Sherrill
 *Date   : 10/96 (C++ version 4/97)
 *
 * Function Definition:
 * long imageIO ( long AryOffset, 
 *                long HdrOffset, long image_number, )
 * 
 * AryOffset in         Index into image data
 * HdrOffset in         Index into header data                 
 * image_number in      Image number to create
 *
 *NOTES:
 * 1. 
 * 2.  
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *END DOC
 *------------------------------------------------------------------*/


#include "plot_image.hh"
#include "tfdefs.h"
#include "net/net_env.hh"
#include "read_data.h"
#include "cube.h"
#include "tf_global.h"
#include "image_amplitude_recovery.hh"
#include "trace_selector.hh"

#define mem_error \
 "You do not have enough memory on your server for this image.\n\
You might try again with a smaller plot."

#define badread "Error in reading your data file. You may have \n\
attempted to scan past the end of the file."


//========================================================================
//================ Read data and scale as needed =========================
//========================================================================
long PlotImage::imageIO( long               AryOffset,
                         long               HdrOffset,
                         long               /*image_number*/)
{
int cl_stat = 0, is_3d;
long arysize;
int istat = 0, nhdrs;
long i, j, stat;
int normalize;
char msg[80],*trpntr=NULL;
long images;
float tstrt, srval, trmaxg;
double dtrmaxg;
long *request_array;
int old_iskp, old_ndo, old_ntot;
long data_request_offset = 0;
long header_request_offset = 0;
long scaler_request_offset = 0;
int data_size;
long panel_index;
long num_reads, out_index;
int open_file = 1, lun;
Boolean sequential = True;


  //save and restore at end of this method because tf_glbl_getn will
  //reset them.
  tstrt = _user->_G.tstrt;
  srval = _user->_G.srval;
  trmaxg =_user->_G.trmaxg;


  

  if(!_user->getMovieOption())
    images = 1;
  else
    images = _user->getFrames();

  if(isFloatData() == False)//byte data 
    {
    trpntr =  (char *) &_byte_array[0];
    data_size = sizeof(char);
    }
  else//float data
    {
    trpntr = (char *) &_float_array[0];
    data_size = sizeof(float);
    }

  //If using the selector we may have fewer headers read so we want
  //to pad out those that are not with zeroes so they will appear as
  //dead trace headers
  if(usingSelector())
    {
    for(i = 0; i < _ntot * _nhdrs; i++)
       _hd[i + HdrOffset] = 0.0F;
    }



  panel_index = AryOffset / (getTracesInMemory() * getSamplesInMemory());

  if(_Cl.axis == 1)//cube slice
    arysize = _user->_G.ntrfil;
  else
    arysize = _ntot;
  stat = _amp_recovery->allocateScaleFactorArray((panel_index + 1) * arysize);
  if(!stat)
    {
    strcpy(_errstr,mem_error);
    _displayed= False;
    return ( ResourceFail );
    }

//Read the data
  old_iskp = _Cl.iskp;
  old_ndo  = _Cl.ndo;
  old_ntot = _Cl.ntot;
  header_request_offset = HdrOffset;
  data_request_offset   = AryOffset * data_size;
  if(_Cl.axis == 1)//cube slice
    scaler_request_offset = panel_index * getTracesInMemory() * 
                            getSamplesInMemory();
  else
    scaler_request_offset = panel_index * getTracesInMemory();


  //If using the selector, see if the traces are sequential
  if(usingSelector())
    {
    request_array = _trace_selector->getTraceNumbers(getCurrentPanel());
    num_reads = _trace_selector->getNumTraces(getCurrentPanel());
    if (num_reads < 1) {
      strcpy (_errstr,badread);
      return (ReadFail);
    }
    for(i = 1; i < num_reads; i++)
      {
      if(request_array[i] - request_array[i - 1] > 1)
        {
        sequential = False;
        i = num_reads;//Traces are not sequential
        }
      }
    if(sequential) num_reads = 1;
    }
  else
    {
    num_reads = 1;
    }



  for(i = 0; i < num_reads; i++)
    {
    if(usingSelector())
      {
      request_array = _trace_selector->getTraceNumbers(getCurrentPanel());
      assert(request_array != NULL);
      _Cl.iskp = request_array[i];
      if(!sequential)
        _Cl.ndo = _Cl.ntot = 1;
      else
        _Cl.ndo = _Cl.ntot = _trace_selector->getNumTraces(getCurrentPanel());
      scaler_request_offset = i;
       
      }
    if(_user->_G.nbydp < 4 && strstr(_user->_G.ftyp,"BYTE")!=0) 
      {
      nhdrs = (int)_nhdrs;
      read_byt_(_user->getFilename(),&istat,&_Cl,&normalize,//normalize not used
                &nhdrs,(char *) &_hd[header_request_offset], 
                (unsigned char *) &trpntr[data_request_offset], 
                _amp_recovery->getScaleFactorArray(scaler_request_offset) );
      _nhdrs = nhdrs;
      if(_Cl.axis == 1) //cube timeslice, get the header trace scalers
        {
        read_data_hdslice_byname(_user->getFilename(),  
                 _amp_recovery->getScaleFactorArray(scaler_request_offset),
                 _Cl.trnsps );
        }
      } 
    else 
      {
      NetEnv::tfioReadData(_netenv, _user->getFilename(),&istat,&_Cl, 
                   (char *) &_hd[header_request_offset],
                   (char *) &trpntr[data_request_offset],
                   _amp_recovery->getScaleFactorArray(scaler_request_offset),
                   &lun, open_file );
      open_file = 0;

      if(_Cl.axis == 1) //cube timeslice, get the header trace scalers
        {
        NetEnv::tfioReadDataHdsliceByname(_netenv, _user->getFilename(), 
                   _amp_recovery->getScaleFactorArray(scaler_request_offset),
                   _Cl.trnsps);
        }
      }
    header_request_offset =  HdrOffset + ((i+1) * _nhdrs);
    data_request_offset   +=  (_Cl.ndo * _nsamp) * data_size;
    }//end reading data



  //If using the selector we may have fewer traces read so we want
  //to pad out those that are not with zeroes so they will appear as
  //dead traces
  if(usingSelector() && _trace_selector->getNumTraces(getCurrentPanel()) < _ntot)
    {
    assert(_float_array);
    j = getCurrentPanel();
    out_index = _trace_selector->getNumTraces(j) * _nsamp + AryOffset;
    for(i = 0; i < (_ntot - _trace_selector->getNumTraces(j)) * _nsamp; i++)
       _float_array[i + out_index] = 0.0F;
    out_index = _trace_selector->getNumTraces(j) * _nhdrs + HdrOffset;
    for(i = 0; i < (_ntot - _trace_selector->getNumTraces(j)) * _nhdrs; i++)
       _hd[i + out_index] = 0.0F;
    }


  _Cl.iskp = old_iskp;
  _Cl.ndo  = old_ndo;
  _Cl.ntot = old_ntot;


  _amp_recovery->setScaleFactorForReadArray(panel_index, _ntot);


  if (_netenv == NULL) {
      //If we have read a 3d file we need to save the Grid3DDesc info
      //because it will be freed and lost when the file is closed. CSV
      //needs this info for xy coordinates within the cube
      if(tf_global_is3d(&_user->_G)) {
         memcpy(&_user->_3dDesc, &_user->_G.h, sizeof(Grid3DDesc));
         is_3d = 1;
      }
      else {
         is_3d = 0;
      }

      if(!isFloatData())
        {
        tf_glbl_getn_( &istat,  _user->getFilename(), &_user->_G); 
        tf_close_(&_user->_G.lun, &cl_stat, msg);  
        }
      else
        {
        //Only close if no error. If error it should already be closed by
        //tfio library
        if(istat == 0)
          tf_close_(&lun, &cl_stat, msg);  
        }

      if(is_3d) {
         memcpy(&_user->_G.h, &_user->_3dDesc,  sizeof(Grid3DDesc));

         if (!strcmp(_user->_G.ftyp,"DSEGY") &&
             cube_trcio_getTrmaxgFromFile(_user->getFilename(),
             _crossline_header, _inline_header, &dtrmaxg)) {
// this is a kluge to recover the TRMAXG from the CubeTrcio auxilliary file
//   potential problems could be caused if the user has set TRMAXG elsewhere
//   for scaling purposes this was done to view nonConoco SEGY data using CSV
           trmaxg = (float)dtrmaxg;
         }
      }
  }
  else {
      strcpy(msg, "OK");
  }
  //now reset the parameters that tf_glbl_getn has changed
  _user->_G.tstrt  = tstrt;
  _user->_G.srval  = srval;
  _user->_G.trmaxg = trmaxg;


//see if we have an error
  if(istat != 0   ||   strncmp(msg,"OK",2) != 0)
    {
    strcpy(_errstr,badread);
    return(ReadFail);
    }

  return (PlotSuccess);
}




