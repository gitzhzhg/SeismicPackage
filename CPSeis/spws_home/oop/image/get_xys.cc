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


/*------------------------------------------------------------------ 
 *USER DOC
 *Name   : getXys 
 *Purpose: Open byte or float file to determine xy grid defaults.
 *                                      
 *Author : Michael L. Sherrill
 *Date   : 12/93 (C++ version 4/97)
 *
 * Function Definition:
 * long   get_xys ()
 *
 *
 *NOTES:
 * 1.
 *
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *END DOC
 *------------------------------------------------------------------*/
#include "tfio.h"
#include "tfdefs.h"
#include "plot_image.hh"
#include "trace_selector.hh"
#include "net/net_env.hh"
#include <stdlib.h>

long PlotImage::getXys()

{
 float *hd = NULL;
 unsigned char *bytes = NULL;
 float *float_array = NULL;       
 static float *tasf = NULL;
 long maxread;
 int normalize = 0;
 char msg[81];
 int istat, istat2, cl_stat = 0;
 long  i;
 int nhdrs;
 long headersize;
 struct Cntrl Cl;
 long *request_array;
 long num_reads;
 long header_request_offset = 0;
 int open_file = 1;
 int lun;

/*allocate arrays to read in data*/
 maxread = min(_user->getNumberToPlot(),_user->getGlobals().ntrfil);
 headersize = _user->getGlobals().nhdwd * (2 * sizeof(double));
 hd  = (float *) malloc( (int)(maxread*headersize) );
 if(hd == NULL) return(DEFS_FAIL);

 nhdrs = _user->getGlobals().nhdwd;

 //If using the selector we may have fewer headers read so we want
 //to pad out those that are not with zeroes so they will appear as
 //dead trace headers
 if(usingSelector())
    {
    for(i = 0; i < maxread * nhdrs; i++)
       hd[i] = 0.0F;
    } 


 //if(_user->getGlobals().nbydp < 4) /*byte data*/
 if(_user->getGlobals().nbydp < 4 && strstr(_user->_G.ftyp,"BYTE")!=0)
    {
     bytes = ( unsigned char *) malloc((int)(maxread*(2*sizeof(float))
                                       *sizeof(unsigned char) ));
     if(bytes == NULL)
        {
        free(hd);
        return(DEFS_FAIL);
        }
    }
 else // data from tfile
    {
    float_array = (float *) malloc( (int)((1+(2*sizeof(float))) * maxread 
                                      * sizeof(float) ));
    if(float_array == NULL)
       {
       free(hd);
       return(DEFS_FAIL);
       }
    }


 tasf = (float *) malloc((int)((maxread+1)*sizeof(float)));
 if(tasf == NULL)
    {
    free(hd);
    if(bytes != NULL) free(bytes);
    if(float_array != NULL) free(float_array);
    return(DEFS_FAIL);
    }

 if(usingSelector())
    num_reads = _trace_selector->getNumTraces(getCurrentPanel());
  else
    num_reads = 1;

 
 Cl.ntot   = (int)maxread;
 Cl.nsamp  = Cl.samp1 = Cl.sdec = 1;
 Cl.ndo    = (int)(max(1,_user->getNumberToDo()));
 Cl.iskp   = (int)_user->getInitialSkip();
 Cl.nskp   = (int)_user->getNumberToSkip();
 Cl.trnsps = Cl.cnvrt = 0;
 Cl.axis   = 0;
 Cl.index = 0;
 

 for(i = 0; i < num_reads; i++)
    {
    if(usingSelector())
      {
      request_array = _trace_selector->getTraceNumbers(getCurrentPanel());
      assert(request_array != NULL);
      Cl.iskp = request_array[i];
      Cl.ndo  = Cl.ntot = 1;
      }

  /*read in traces*/
   if(_user->_G.nbydp < 4 && strstr(_user->_G.ftyp,"BYTE")!=0)/*bytes*/
     {
     normalize = True;
     read_byt_(_user->getFilename(),&istat,&Cl,&normalize,
               &nhdrs,(char *) &hd[header_request_offset], 
               (unsigned char *)bytes, tasf );
     if(istat != 0)
       {
       tf_close_(&_user->_G.lun, &cl_stat, msg);
       free(hd);
       free(tasf);
       free(bytes);
       return (DEFS_FAIL);
       }
     }
   else  /*floating point data*/
     {
     NetEnv::tfioReadData(_netenv, _user->getFilename(),&istat,&Cl, 
                           (char *) &hd[header_request_offset],
                           (char *) float_array, tasf, &lun, open_file);
     if(istat != 0)
       {
       if (_netenv == NULL) tf_close_(&lun, &cl_stat, msg);
       free(hd);
       free(tasf);
       free(float_array);
       return (DEFS_FAIL);
       }
     }
   header_request_offset = (i+1) * nhdrs; 
  }

  if(bytes       != NULL) free(bytes);
  if(float_array != NULL) free(float_array);


  if(_user->_G.nbydp < 4 && strstr(_user->_G.ftyp,"BYTE")!=0)//old byte type
    {
    tf_glbl_getn_( &istat2,  _user->getFilename(), &_user->_G);
    tf_close_(&_user->_G.lun, &cl_stat, msg);
    }
  else
    {
    tf_close_(&lun, &cl_stat, msg);
    }

  _nhdrs = nhdrs;





 if(usingSelector())
   maxread = num_reads;

 /*transfer headers to defaults*/
 _user->setX1(hd[(_user->getMatchXheader()-1)]);
 _user->setY1(hd[(_user->getMatchYheader()-1)]);
 _user->setX2(hd[(maxread-1)*_nhdrs + (_user->getMatchXheader()-1)]);
 _user->setY2(hd[(maxread-1)*_nhdrs+(_user->getMatchYheader()-1)]);
 
 if(_xloc != NULL) free(_xloc);
 _xloc  = (float *) malloc( (int)(sizeof(float) * _user->getGlobals().ntrfil));
 if(_xloc == NULL) return (DEFS_FAIL);

 if(_yloc != NULL) free(_yloc);
 _yloc  = (float *) malloc( (int)(sizeof(float) * _user->getGlobals().ntrfil));
 if(_yloc == NULL){free(_xloc); return (DEFS_FAIL);}


 for(i = 0; i < maxread; i++) 
    {
    _xloc[i] = hd[i*_nhdrs+(_user->getMatchXheader()-1)];
    _yloc[i] = hd[i*_nhdrs+(_user->getMatchYheader()-1)];
    }

 if(_user->getX1() == _user->getX2())
    _user->setX2(_xloc[i-1]);

 free(hd);
 free(tasf); 

 return(DEFS_OK);


}
