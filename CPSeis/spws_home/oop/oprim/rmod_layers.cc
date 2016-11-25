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

//--------------------------- rmod_layers.cc ---------------------------//
//--------------------------- rmod_layers.cc ---------------------------//
//--------------------------- rmod_layers.cc ---------------------------//

//           implementation file for the RmodLayers class
//                    not derived from any class
//                        subdirectory oprim



#include "oprim/rmod_layers.hh"
#include "cprim.h"
#include "named_constants.h"
#include "str.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "tfio.h"
#include "wrdcnvrt.h"

#include "ebcdic.h"
#include "tfdefs.h"
#include "tf_global.h"
#include "rmodc.h"
#include "model_io.h"
#include "dbutil.h"


#define NBUF       222
#define NHOR_STEP   10
#define NVAL_STEP 1000
#define DEBUG     FALSE
/*
#define NHOR_STEP   2
#define NVAL_STEP   5
#define DEBUG     TRUE
*/


//-------------------------- constructor -------------------------------//
//-------------------------- constructor -------------------------------//
//-------------------------- constructor -------------------------------//


RmodLayers::RmodLayers()
                 :
           _nhor       (0),
           _nval       (0),
           _nhor_alloc (0),
           _nval_alloc (0),

           _xtype      (newstr("none")),
           _ytype      (newstr("none")),
           _ztype      (newstr("none")),

           _name       (NULL),
           _color      (NULL),
           _hid        (NULL),

           _xval       (NULL),
           _yval       (NULL),
           _zval       (NULL),
           _ident      (NULL),
           _segment    (NULL),
           _mult       (1.0)
{
}



//------------------------------ destructor -----------------------------//
//------------------------------ destructor -----------------------------//
//------------------------------ destructor -----------------------------//


RmodLayers::~RmodLayers()
{
  clearData();
  free(_xtype);
  free(_ytype);
  free(_ztype);
}



//--------------------------- clear data --------------------------------//
//--------------------------- clear data --------------------------------//
//--------------------------- clear data --------------------------------//

      // public.

void RmodLayers::clearData()
{
  free(_xtype);
  free(_ytype);
  free(_ztype);
  _xtype = newstr("none");
  _ytype = newstr("none");
  _ztype = newstr("none");
  for(int ihor = 0; ihor < _nhor; ihor++)
      {
      free(_name [ihor]);
      free(_color[ihor]);
      }
  if(_name   ) delete _name;
  if(_color  ) delete _color;
  if(_hid    ) delete _hid;
  if(_xval   ) delete _xval;
  if(_yval   ) delete _yval;
  if(_zval   ) delete _zval;
  if(_ident  ) delete _ident;
  if(_segment) delete _segment;
  _nhor       = 0;
  _nval       = 0;
  _nhor_alloc = 0;
  _nval_alloc = 0;
}



//------------------------- get values ---------------------------------//
//------------------------- get values ---------------------------------//
//------------------------- get values ---------------------------------//

      // public.

const char *RmodLayers::horizonName(int ihor)  const
{
  assert(ihor >= 0 && ihor < _nhor);
  return _name[ihor];
}


const char *RmodLayers::horizonColor(int ihor)  const
{
  assert(ihor >= 0 && ihor < _nhor);
  return _color[ihor];
}


int RmodLayers::horizonIdent(int ihor)  const
{
  assert(ihor >= 0 && ihor < _nhor);
  return _hid[ihor];
}


float RmodLayers::getXval(int ival)  const
{
  assert(ival >= 0 && ival < _nval);
  return _xval[ival];
}


float RmodLayers::getYval(int ival)  const
{
  assert(ival >= 0 && ival < _nval);
  return _yval[ival];
}


float RmodLayers::getZval(int ival)  const
{
  assert(ival >= 0 && ival < _nval);
  return _zval[ival];
}


int RmodLayers::getHorizonIdent(int ival)  const
{
  assert(ival >= 0 && ival < _nval);
  return _ident[ival];
}


int RmodLayers::getSegmentIdent(int ival)  const
{
  assert(ival >= 0 && ival < _nval);
  return _segment[ival];
}


int RmodLayers::getHorizonIndex(int ival)  const
{
  assert(ival >= 0 && ival < _nval);
  int ident = _ident[ival];
  for(int index = 0; index < _nhor; index++)
      {
      if(_hid[index] == ident) return index;
      }
  return 0;   // should not get to here.
}



//------------------------ static allocate -----------------------------//
//------------------------ static allocate -----------------------------//
//------------------------ static allocate -----------------------------//


/*****
static float *float_allocate(float *xval, int nval, int nval_alloc)
{
  float *xval_new = new float [nval_alloc];
  if(nval > 0)
      {
      memcpy(xval_new, xval, nval * sizeof(float));
      delete [] xval;
      }
  return xval_new;
}
*****/


static int *int_allocate(int *xval, int nval, int nval_alloc)
{
  int *xval_new = new int [nval_alloc];
  if(nval > 0)
      {
      memcpy(xval_new, xval, nval * sizeof(int));
      delete [] xval;
      }
  return xval_new;
}


static char **char_allocate(char **xval, int nval, int nval_alloc)
{
  char **xval_new = new char * [nval_alloc];
  if(nval > 0)
      {
      memcpy(xval_new, xval, nval * sizeof(char*));
      delete [] xval;
      }
  return xval_new;
}



//----------------------- small private functions -----------------------//
//----------------------- small private functions -----------------------//
//----------------------- small private functions -----------------------//

     // private.

void RmodLayers::privateSetCoordTypes(const char *xtype,
                                      const char *ytype,
                                      const char *ztype)
{
  assert(xtype && ytype && ztype);
  free(_xtype);
  free(_ytype);
  free(_ztype);
  _xtype = newstr(xtype);
  _ytype = newstr(ytype);
  _ztype = newstr(ztype);
/*
  convert_case_to_upper(_xtype, _xtype);
  convert_case_to_upper(_ytype, _ytype);
  convert_case_to_upper(_ztype, _ztype);
*/
  str_to_upper(_xtype, _xtype);
  str_to_upper(_ytype, _ytype);
  str_to_upper(_ztype, _ztype);
  if(DEBUG) printf("privateSetCoordTypes: %s %s %s\n", _xtype, _ytype, _ztype);
  if     (strncmp(_ztype, "TI", 2) == 0) _mult = 1000.0;
  else if(strncmp(_ztype, "SE", 2) == 0) _mult = 1000.0;
  else                                   _mult = 1.0;
}



void RmodLayers::privateAddHorizon
                      (const char *name, const char *color, int hid)
{
/////////////// check whether horizon is already added:

  for(int ihor = 0; ihor < _nhor; ihor++)
      {
      if(hid == _hid[ihor]) return;
      }

/////////////// allocate space if necessary:

  if(_nhor == _nhor_alloc)
      {
      _nhor_alloc += NVAL_STEP;
      _name  =  char_allocate(_name , _nhor, _nhor_alloc);
      _color =  char_allocate(_color, _nhor, _nhor_alloc);
      _hid   =   int_allocate(_hid  , _nhor, _nhor_alloc);
      }

/////////////// set the name:

  if(name && name[0] != ' ' && name[0] != '\0')
      {
      _name[_nhor] = newstr(name);
      }
  else
      {
      char buffer[NBUF];
      sprintf(buffer, "HOR%03d", hid);
      _name[_nhor] = newstr(buffer);
      }

/////////////// set the color:

  if(color && color[0] != ' ' && color[0] != '\0')
      {
      _color[_nhor] = newstr(color);
      }
  else
      {
      _color[_nhor] = newstr("green");
      }

/////////////// finish up and return:

  _hid  [_nhor] = hid;
  _nhor++;
  if(DEBUG) printf("privateAddHorizon: %d name=%s color=%s hid=%d\n",
                     _nhor, _name [_nhor-1],
                            _color[_nhor-1],
                            _hid  [_nhor-1]);
}



void RmodLayers::privateAddPick(float xval, float yval, float zval,
                                int ident, int segment)
{
  if(_nval == _nval_alloc)
      {
      _nval_alloc += NVAL_STEP;
      float *   xval_new = new float [_nval_alloc];
      float *   yval_new = new float [_nval_alloc];
      float *   zval_new = new float [_nval_alloc];
      int   *  ident_new = new int   [_nval_alloc];
      int   *segment_new = new int   [_nval_alloc];
      if(_nval > 0)
          {
          memcpy(   xval_new,    _xval, _nval * sizeof(float));
          memcpy(   yval_new,    _yval, _nval * sizeof(float));
          memcpy(   zval_new,    _zval, _nval * sizeof(float));
          memcpy(  ident_new,   _ident, _nval * sizeof(int));
          memcpy(segment_new, _segment, _nval * sizeof(int));
          delete [] _xval;
          delete [] _yval;
          delete [] _zval;
          delete [] _ident;
          delete [] _segment;
          }
      _xval    =    xval_new;
      _yval    =    yval_new;
      _zval    =    zval_new;
      _ident   =   ident_new;
      _segment = segment_new;
/*****
      _xval    = float_allocate(_xval   , _nval, _nval_alloc);
      _yval    = float_allocate(_yval   , _nval, _nval_alloc);
      _zval    = float_allocate(_zval   , _nval, _nval_alloc);
      _ident   =   int_allocate(_ident  , _nval, _nval_alloc);
      _segment =   int_allocate(_segment, _nval, _nval_alloc);
*****/
      }
  _xval   [_nval] = xval;
  _yval   [_nval] = yval;
  _zval   [_nval] = zval * _mult;
  _ident  [_nval] = ident;
  _segment[_nval] = segment;
  _nval++;
  if(DEBUG) printf("privateAddPick: %d x=%f y=%f z=%f id=%d seg=%d\n",
                     _nval, _xval   [_nval-1],
                            _yval   [_nval-1],
                            _zval   [_nval-1],
                            _ident  [_nval-1],
                            _segment[_nval-1]);
}



//-------------------------- prototypes -------------------------------//
//-------------------------- prototypes -------------------------------//
//-------------------------- prototypes -------------------------------//

extern "C"
{

void rmodrdhd_w_(const char *hfil,char *gfil,char *tfil,char *ftyp,
             char *cxi,char *cyi,char *czi,char *cxo,char *cyo,char *czo,
             int *mcoord, int *ncoord,float *xcoord,char *coord,
             float *mxmin,float *mxmax,
             float *mymin, float *mymax,
             float *mzmin,float *mzmax, float *zdatum,
             int *nx, float *ox, float *dx,
             int *ny, float *oy, float *dy,
             int *nz, float *oz, float *dz,
             int *wd_type,int *istat);

void rmodfstr_w_(int *IFILE,int *IREWIND,char *ISTR,int *ICARD,int *ISTAT);

void rmodrval_w_(int *IFILE,int *NDOF,int *MX,int *NX,float *X,int *ISTAT);

void rmodclos_w_(int *lun);

void rmodfcrd_w_(int *lun,int *rewind,char *str,char *cx,char * cz,
                 int *ndof,int *istat);

void rmodopen_w_(int *lun, char *dfile, int *iform, int *istatus,
                 int *iaccess, int *nrecl, int *wdtyp, int *istat);

void rmodhdpr_w_(int *m1);

}



//---------------------- private read header ---------------------------//
//---------------------- private read header ---------------------------//
//---------------------- private read header ---------------------------//

       // private.
       // sets coordinate types.
       // opens file and returns file unit number lun.
       // closes file and returns -1 if error occurs.

int RmodLayers::privateReadHeader (const char *hfile, char *msg)
{
  char  dfile[120];
  float xmin,xmax,ymin,ymax,zmin,zmax;
  float o1,o2,o3;
  float d1,d2,d3;
  int   n1,n2,n3, istat;
  int   word_type,m1= -1;
  char  coord[512];
  char  tfile[96],type[16];
  float xcoord[32];
  char  cxi[16], cyi[16], czi[16];
  char  cxo[16], cyo[16], czo[16];
  float zdatum;
  int ncoord  = 0;
  int mcord   = 32;

  strcpy(cxi, "none");
  strcpy(cyi, "none");
  strcpy(czi, "none");
  cxo[0]  = '\0';  /* no conversion of coordinate type */
  cyo[0]  = '\0';
  czo[0]  = '\0';
  dfile[0]= '\0';
  tfile[0]= '\0';
  type[0] = '\0';

  rmodhdpr_w_(&m1);
  rmodrdhd_w_(hfile,dfile,tfile, type,
              cxi,cyi,czi,cxo,cyo,czo,
              &mcord, &ncoord,xcoord,coord,
              &xmin,&xmax, &ymin,&ymax, &zmin,&zmax,&zdatum,
              &n2,&o2,&d2, &n3,&o3,&d3, &n1,&o1,&d1,
              &word_type, &istat);

  privateSetCoordTypes(cxi, cyi, czi);

  int   wdtyp=3, nrecl=0;
  int   istat2=0, lun;
  int   iform=0;   /*formatted*/
  int   istatus=1; /*old      */
  int   iaccess=0; /*sequential*/

  /* reopen the file - if open it returns the lun  */
  rmodopen_w_(&lun, dfile,&iform,&istatus,&iaccess,&nrecl,
              &wdtyp,&istat2);

  if(istat != 0)
      {
      strcpy(msg, "error in rmodrdhd_w_ while reading RMOD file header");
      rmodclos_w_(&lun);
      return -1;
      }
  if(istat2 != 0)
      {
      strcpy(msg, "error in rmodopen_w_ while reading RMOD file header");
      rmodclos_w_(&lun);
      return -1;
      }
  return lun;
}



//------------------------- private read picks ----------------------------//
//------------------------- private read picks ----------------------------//
//------------------------- private read picks ----------------------------//

     // private.
     // reads horizons and picks.
     // file is already open with specified unit number lun.
     // closes file when finished.
     // sets msg and returns error TRUE or FALSE.

int RmodLayers::privateReadPicks(int lun, char *msg)
{
  int  i,ndof,rewind,one,istat,ncards,nhor;
  int  hnumber, snumber, old_hnumber;
  int  Phdr,Shdr,Thdr;
  int  hids[199];
  char *hnames[199],*colors[199];
  char keyword[16], cx[33],cy[33],cz[33],str[33];
  float xvals[10];

///////////////// get the horizon names etcetera:

  nhor = 0;
  strcpy(keyword,"*HORIZON");
  pcardrdhz(&lun, keyword, &nhor,
             &Phdr, &Shdr, &Thdr,
             hids, hnames, colors);
  if(nhor > 199) nhor = 199;

  for(int ihor = 0; ihor < nhor; ihor++)
      {
      privateAddHorizon(hnames[ihor], colors[ihor], hids[ihor]);
      }

  for(i=0;i<nhor;i++) {free(hnames[i]),free(colors[i]);}

///////////////// rewind and position the file:

  ndof = 5;
  strcpy(cx,"XANNOTATION");
  strcpy(cy,"YANNOTATION");
  strcpy(cz,"KILOMETER");
  strcpy(str,"*PICKS");
  rewind=1; /* force a rewind of the file */
  /* should hop over the initial 2 cards in an old ICP file */
  rmodfcrd_w_(&lun,&rewind,str,cx,cz,&ndof,&istat);
  if(istat != 0)
      {
      strcpy(msg, "error in rmodfcrd_w_ while reading RMOD file");
      rmodclos_w_(&lun);
      return TRUE;
      }

///////////////// read the first pick:

  xvals[0]    = UNDEFINED_VAL; /* time */
  xvals[1]    = UNDEFINED_VAL; /* x */
  xvals[2]    = UNDEFINED_VAL; /* horizon number */
  xvals[3]    = UNDEFINED_VAL; /* segment flag */
  xvals[4]    = UNDEFINED_VAL;
  xvals[5]    = UNDEFINED_VAL; /* y */
  xvals[6]    = UNDEFINED_VAL;
  one         = 1;
  istat       = 0;
  rmodrval_w_(&lun, &ndof, &one, &ncards, xvals, &istat);
  if(istat != 0 || ncards == 0)
      {
      strcpy(msg, "error in rmodrval_w_ while reading RMOD file");
      rmodclos_w_(&lun);
      return TRUE;
      }

  hnumber = (int)NearestInteger(xvals[2]);
  snumber = (int)NearestInteger(xvals[3]);
  privateAddPick(xvals[0], xvals[5], xvals[1], hnumber, snumber);
  privateAddHorizon(NULL, NULL, hnumber);
  old_hnumber = hnumber;

///////////////// read subsequent picks:

  while(TRUE) {
     rmodrval_w_(&lun, &ndof, &one, &ncards, xvals, &istat);
     if(ncards == 0 || istat !=0) break;

     hnumber = (int)NearestInteger(xvals[2]);
     snumber = (int)NearestInteger(xvals[3]);
     privateAddPick(xvals[0], xvals[5], xvals[1], hnumber, snumber);
  
     if(hnumber != old_hnumber) {
        privateAddHorizon(NULL, NULL, hnumber);
        old_hnumber = hnumber;
     }
  }

  strcpy(msg, "RMOD file successfully read");
  rmodclos_w_(&lun);
  return FALSE;
}



//--------------------------- validate file ------------------------------//
//--------------------------- validate file ------------------------------//
//--------------------------- validate file ------------------------------//

      // public.
      // sets info and returns error = TRUE or FALSE.

int RmodLayers::validateFile(const char *filename, char* info)
{
  assert(filename && info);
  if(filename[0] == ' ' || filename[0]=='\0') return TRUE;
  if(strcmp(filename,"NONE")==0)              return TRUE;
  if(strcmp(filename,"none")==0)              return TRUE;

  int type = getgl_ftype((char*)filename);
  if(type != LAYER_TYPE) return TRUE;
  int lun = privateReadHeader(filename, info);
  if(lun == -1)
      {
      return TRUE;
      }
  rmodclos_w_(&lun);
  sprintf(info, "RMOD layer-type file using %s %s %s",
                                    _xtype, _ytype, _ztype);
  return FALSE;
}



//--------------------------- read file ------------------------------//
//--------------------------- read file ------------------------------//
//--------------------------- read file ------------------------------//

      // public.
      // sets msg and returns error = TRUE or FALSE.

int RmodLayers::readFile(const char *filename, char *msg)
{
  assert(filename && msg);
  int type = getgl_ftype((char*)filename);
  if(type != LAYER_TYPE)
      {
      strcpy(msg, "RMOD file is not a layer-type file");
      return TRUE;
      }
  int lun = privateReadHeader(filename, msg);
  if(lun == -1)
      {
      return TRUE;
      }
  int error = privateReadPicks(lun, msg);
  return error;
}



//--------------------------------- end ----------------------------------//
//--------------------------------- end ----------------------------------//
//--------------------------------- end ----------------------------------//

