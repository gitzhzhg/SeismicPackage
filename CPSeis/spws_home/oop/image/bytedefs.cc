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
 *Name   : bytedefs 
 *Purpose: Read file to determine defaults.
 *
 * NOTE  : Should change the name of this sometime since it reads most all
 *         types of files now (TFILES, SEGY, BYTE). Also if we have a file
 *         that has a varying number of traces in each group use 
 *         TraceSelector::findTracesByPattern instead.            
 *                                      
 *Author : Michael L. Sherrill
 *Date   : 10/92 (C++ version 4/97)
 *
 * Function Definition:
 * long   bytedefs (
 *                  float            *xloc,
 *                  float            *yloc,
 *                  long             xloc_hdr,
 *                  long             yloc_hdr,
 *                  long             *num_gathers)
 *
 * xloc         out   Array of x locations 
 * yloc         out   Array of y locations
 * xloc_hdr      in   X control header to find locations.
 * yloc_hdr      in   Y control header to find locations.
 * num_gathers  out   Number of velocity gathers.
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

#define NUMTOREAD 200


long PlotImage::bytedefs ( float            *xloc,
                           float            *yloc,
                           long             xloc_hdr,
                           long             yloc_hdr,
                           long             *num_gathers)
{

 float *hd;
 unsigned char *bytes;       
 long maxread;
 int normalize = 0;
 char msg[81];
 int istat, cl_stat = 0;
 long  i, count;
 float current_group, group1 = 0;
 static float *tasf;
 int tr_read = NUMTOREAD;
 int nhdrs;
 Boolean last_try = False;  
 long headersize;
 long MAX_V;
 int open_file = 1;


  if(strlen(_user->getFilename()) == 0) return (DEFS_FAIL);

  /*try to open file for read*/
  get_global_data_(_user->getFilename(), &_user->_G, &istat);
  if(istat > 0) return (DEFS_FAIL);


 _user->setVelocityMinimum(0.0);
 _user->setVelocityMaximum(0.0);

/*hope to get smarter than this when we have more info stored in the file,
  this currently allows a maximum of 256000 traces*/
 MAX_V = _user->getVelocityArraySize() + 251000;

/*really need a menu item to control the maxread parameter*/
 maxread = (_user->getGlobals().ntrfil < MAX_V) ? _user->getGlobals().ntrfil 
                                                : MAX_V;


/*allocate arrays to read in data*/
 headersize = _user->getGlobals().nhdwd * (2*sizeof(double));
 hd  = (float *) malloc( (int)(maxread*headersize) );
 if(hd == NULL) return(DEFS_FAIL);
 bytes = (unsigned char *) malloc((int)(maxread*(2*sizeof(float))
                                   *sizeof(unsigned char)) );
 if(bytes == NULL) return(DEFS_FAIL);
 tasf = (float *) malloc((int)((maxread+1)*sizeof(float)));
 if(tasf == NULL) return(DEFS_FAIL);
 

 _nhdrs = _user->getGlobals().nhdwd;
 _ntot = _Cl.ntot = _Cl.ndo = min(NUMTOREAD,(int)maxread);
 _Cl.nsamp = _Cl.samp1 = _Cl.sdec   = 1;
 _Cl.iskp  = _Cl.nskp  = _Cl.trnsps = _Cl.cnvrt = 0;
 _Cl.axis = 0;
 _Cl.index = 0;

 if(_user->getGlobals().lun < 0) 
    _user->_G.lun = -_user->_G.lun;



/*read in 10 traces at a time*/
 count = 0;
 while(count == 0)
    {
    if(_user->_G.nbydp < 4 && strstr(_user->_G.ftyp,"BYTE")!=0) 
      {
      nhdrs = (int)_nhdrs;
      read_byt_(_user->getFilename(),&istat,&_Cl,&normalize,//normalize not used
                &nhdrs,(char *) &hd[_Cl.iskp*_nhdrs], 
                (unsigned char *) &bytes[0], tasf);
      _nhdrs = nhdrs;
      } 
    else 
      {
      read_data_(_user->getFilename(),&istat,&_Cl, 
                 (char *) &hd[_Cl.iskp*_nhdrs],
                 (char *) &bytes[0], tasf, &_user->_G.lun, open_file);
      open_file = 0;
      }
    if(istat != 0)
       {
       tf_close_(&_user->_G.lun, &cl_stat, msg);
       free(hd);    hd    = NULL;
       free(bytes); bytes = NULL;
       free(tasf);  tasf  = NULL;
       return(DEFS_FAIL);
       }
    /*search the 10 new traces*/
    for(i=_Cl.iskp; i<_Cl.iskp+tr_read; i++)
       {
       current_group = hd[i*_nhdrs+GRPHDR];
       if(!i) group1 = current_group;
       if(current_group != group1) /*stop have default*/
          {
          count = i;
          i = _Cl.iskp + tr_read;
          }
       }
    /*If count is one assume we are reading a stacked section to be
     displayed in va, and set the count to the total traces in the file*/
    if(count == 1) count = _user->_G.ntrfil;

    /*may have only one panel if we havent found defaults on last read*/
    if(!count && last_try)  count = maxread;

    /*increment skip to read next 10 traces*/
    if(!count)
      {
      _Cl.iskp += tr_read;
      if(_Cl.iskp+tr_read >= maxread) /*end of file */
         {
	 _Cl.ndo = _Cl.ntot = maxread - _Cl.iskp;
         tr_read = _Cl.ndo;
         last_try = True;
         }
      }

   } /*end while*/


   if(count > _user->getVelocityArraySize())
    {
    printf("More locations found than the array can hold in bytedefs");
    return (DEFS_FAIL);
    }


/*transfer headers to defaults*/
  _user->setVelocityMinimum(hd[VELHDR]);
  _vel_min = hd[VELHDR];
  _user->setVelocityMaximum(hd[(count-1)*_nhdrs+VELHDR]);
  _vel_max = hd[(count-1)*_nhdrs+VELHDR];
  _user->setNumberToDo(count);
  _user->setNumberToPlot(count);
  _traces_per_group = count;
  for(i=0;i<count;i++) _vary[i] = hd[i*_nhdrs+VELHDR];
  *num_gathers = _user->getGlobals().ntrfil / count;
  _grid_x1 =  hd[(xloc_hdr-1)];
  _user->setX1(hd[(xloc_hdr-1)]);
  _grid_x2 = hd[(count-1)*_nhdrs+(xloc_hdr-1)];
  _user->setX2(hd[(count-1)*_nhdrs+(xloc_hdr-1)]);

/*now read one header from each gather to get all locations*/
  _ntot = _Cl.ntot = (int)*num_gathers;
  _Cl.ndo = 1;
  _Cl.nskp = (int)count - 1;
  _Cl.iskp = 0;

  nhdrs = (int)_nhdrs;
  if(_user->_G.nbydp < 4 && strstr(_user->_G.ftyp,"BYTE")!=0) 
      {
      nhdrs = (int)_nhdrs;
      read_byt_(_user->getFilename(),&istat,&_Cl,&normalize,//normalize not used
                &nhdrs,(char *) &hd[0], 
                (unsigned char *) &bytes[0], tasf);
      _nhdrs = nhdrs;
      } 
    else 
      {
      read_data_(_user->getFilename(),&istat,&_Cl, 
                 (char *) &hd[0],
                 (char *) &bytes[0], tasf, &_user->_G.lun, open_file);
      }
  if(istat != 0)
    {
    tf_close_(&_user->_G.lun, &cl_stat, msg);
    free(hd);    hd    = NULL;
    free(bytes); bytes = NULL;
    free(tasf);  tasf  = NULL;
    return (DEFS_FAIL);
    }

  tf_close_(&_user->_G.lun, &cl_stat, msg);

 

  for(i = 0; i < *num_gathers; i++) 
    {
    xloc[i] = hd[i*_nhdrs+(xloc_hdr-1)];
    yloc[i] = hd[i*_nhdrs+(yloc_hdr-1)];
    }

  if(_user->getX2() == _user->getX1())
    {
    _grid_x2 = xloc[i-1]; 
    _user->setX2(xloc[i-1]);
    }

  _Cl.nskp = 0;
  free(hd);    hd    = NULL;
  free(bytes); bytes = NULL;
  free(tasf);  tasf  = NULL; 
  return(DEFS_OK);


}



