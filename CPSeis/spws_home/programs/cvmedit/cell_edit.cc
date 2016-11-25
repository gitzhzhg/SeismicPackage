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
//------------------- coord_edit.cc ----------------------//
//         implementation file for the CoordEdit class
//                 derived from the SLDatabox class
#include <assert.h>
#include "wproc.h"
#include "cprim.h"
#include "vect/ll_seis_vect.hh"
#include "vl_data.hh"
#include "vec_list_util.hh"
#include "oprim/ll_base_data.hh"
#include "oprim/modbase_data.hh"
#include "sl/slp_push.hh"
#include "sl_cvm_app.hh"
#include "draw_mod.h"
#include "wbox.h"
#include "transform.h"
#include "mlimits.h"
#include "cell_edit.hh"
#include "named_constants.h"
#include "c2f_interface.h"

class SeisPlot;

//#ifdef CRAY
#ifdef NEED_CAPITALS
#define cellcalc   CELL
#define cellpnt_w  CELLPNT_W
#define cellwher_w CELLWHER_W
#define cellpoly_w CELLPOLY_W
#endif
//#if (VMS || _AIX || hpux)
#ifdef NEED_UNDERSCORE
#define cellcalc   cell_
#define cellpnt_w  cellpnt_w_
#define cellwher_w cellwher_w_
#define cellpoly_w cellpoly_w_
#endif

//#if ( ultrix || sun || __sgi)
//#define cellcalc_ cell_
//#endif

#ifdef __cplusplus
extern "C" {                 // for C++
#endif
extern void cellcalc(float *,float *,float *,float *,int *,int *,
       int *,float *,float *, int *, int *, int *, int *,int *,
       float *,float *,int  *, float *, int *);
extern void cellwher_w(float *, float *, int  *,int  *,float *,
       float *, float *,int  *, int  *,int  *,float *, float *,int  *);
extern void cellpnt_w(float *, float *, int  *, float *, float *,
       int  *,int  *);
extern void cellpoly_w(float *, float *, int  *, float *, float *,
       int  *,int  *);
#ifdef __cplusplus
}                   // for C++
#endif


CellEdit::CellEdit (SLDelay *slparent, char *name,void *cvm,
           HelpCtx Hctx, Boolean   doframe,
           Boolean   make_if_can, Boolean   manage_now, long ident)
         : SLSmartForm(slparent,name,Hctx,doframe,False,manage_now),
           DataUser(),
     _n     (0),
     _nmax  (99),
     _nvar  (16),
     _isw   (1),
     _ident(ident),
     _iradio(0)
{ _cvmapp = (CvmApp *) cvm;
  assert(_cvmapp);
  if(slparent->made() && make_if_can) make();
  setup();
}

CellEdit::CellEdit (Widget parent, char *name,void *cvm,
           HelpCtx Hctx, Boolean   doframe,
           Boolean   make_now, Boolean   manage_now, long ident)
         : SLSmartForm(parent,name,Hctx,doframe,False,manage_now),
           DataUser(),
     _n     (0),
     _nmax  (99),
     _nvar  (16),
     _isw   (1),
     _ident(ident),
     _iradio(0)
{ _cvmapp = (CvmApp *) cvm;
  assert(_cvmapp);
  if(make_now) make();
  setup();
}

void CellEdit::setup()
{// Initialize & retain starting information.

  int i;
  assert(_cvmapp);
  addData(_cvmapp->cvm_get_vldata(CvmAppBase::Cpointers));
  _vlist[0]=0;
  _hot_label=0;
  for (i = 0; i < _nmax; i++)
   {
     _mid[i] =0;
     _xc[i]  =0.0;
     _yc[i]  =0.0;
     _sc[i]  =0.0;
     _in[i]  =0;
     _has[i] =0;
   }
  _n = CE_Set();
}


CellEdit::~CellEdit(void)
{
 if(_databox) delete _databox;
 _databox = NULL;
}

Widget CellEdit::make(Widget/* parent*/)
{
 if(!made())
  {
   Widget wid = SLSmartForm::make();
   /************************************************
    * Create a table(SLDataBox here               */
    _form = this;
    _databox = new SLDatabox(_form,"cellbox",this);
    _databox->setMakeTrap (makeTrap, (void *) this);
    _databox->make();
    attach(_databox, this, NULL, this , this, 2,2,2,2 );

    _databox2 = new SLDatabox(_form,"cellbox2",this);
    _databox2->setMakeTrap (makeTrap2, (void *) this);
    _databox2->make();
    attach(_databox2, _databox, this, this , this, 8,2,2,2 );
  }
 makeChildren();
 return topWidget();
}

void CellEdit::makeTrap(void *data)
{CellEdit *cdtable = (CellEdit *) data;
 if(data) cdtable->makeHelper();
}
void CellEdit::makeTrap2(void *data)
{CellEdit *cdtable = (CellEdit *) data;
 if(data) cdtable->makeHelper2();
}


void CellEdit::makeHelper()
{static long zero=0,one=1;
//-------------------------create a set of linked arrays:
//        N   NMAX ROW MAXDISP
  wbox_rega(&_n, &_nmax, 0,   20);

//   TRAP  ID  LABEL    SWITCH  VARIABLE  SWITCH  COL NCHAR NDEC
  wbox_irega(htrap, 1, "MID",         &zero,  _mid ,  &one  , 0,   3, 0);
  wbox_frega(htrap, 2, "Pointer-X"  , &zero,  _xc ,  &one   , 0,  12, 3);
  wbox_frega(htrap, 3, "Pointer-Z"  , &zero,  _yc ,  &one   , 0,  12, 3);
  wbox_frega(htrap, 4, "Pointer-Y"  , &zero,  _sc ,  &one   , 0,  12, 3);
  wbox_irega(htrap, 5, "IN"         , &zero,  _in ,  &zero  , 0,   3, 0);
}

void CellEdit::makeHelper2()
{static long zero=0,four = 4;
//        N   NMAX ROW MAXDISP
  wbox_rega(&_numcell, &_nmax, 0,   20);
  wbox_rrega(radio_trap, 6, "CELL", &zero, &_iradio, &four  , 0, 2   , 0);
  wbox_irega(NULL, 7, "Has"      , &zero,  _has ,  &zero  , 0,  3, 0);
}

void CellEdit::modDone(BaseData *bptr, long /* ident */)
{// Changes have been made to the data object.
 // Update the table display.
 VectorListData *vld = (VectorListData *) bptr;
 if( _cvmapp->cvm_get_vldata(CvmAppBase::Cpointers) != vld)
   { printf("CellEdit::modDone: BaseData object is inconsistent\n");
     return;
   }
 CE_Set();
}

void CellEdit::dataDeleted(VectorListData *bdata)
{
  if(bdata != _vldata)
    {printf("CoordEdit:: dataDestroyed- wrong data?\n");
     return;
    }
  _vldata = NULL;
  delete this;
}

int CellEdit::CE_Set()
{// Load the existing data into the table.
 VectorLinkedList *vll;
 if(!_cvmapp) return 0;
 vll  = (VectorLinkedList *) _cvmapp->cvm_get_vll(CvmAppBase::Cpointers);
 return data_to_table(vll);
}

int CellEdit::CE_Get()
{// Load the table data into the data object.
 return (int) table_to_data();
}

int CellEdit::data_to_table(VectorLinkedList *vll)
{// Load the data object(VectorLinkedList) into the table.
 VectorLinkedList   *vll_cells;
 Vector             *vector;
 ModBaseData        *dobj;
 int                 i;
 long                count;

 vll_cells  = (VectorLinkedList *) _cvmapp->cvm_get_vll(CvmAppBase::Boundarys);
 _numcell = VectorListCount(vll_cells);

 if(!vll) return 0;
 _n = VectorListCount(vll);
 count =  (_n < _nmax) ? _n : _nmax;
 for(i=0;i< count;i++)
  {vector = cell_n_vector(vll,i+1);
   if(vector) vector->makeVisible();
   dobj   = NULL;
   if(vector) dobj   = (ModBaseData *) vector->getData();
   _mid[i] = 0; _xc[i] = 0.; _yc[i] = 0.; _sc[i]=0; _in[i]=0;
   if(dobj)
    {
     dobj->getpt(_xc+i, _yc+i, _sc+i);
     _mid[i] = dobj->getid();
    }
  }
 set_in();
 return (int) _n;
}

int CellEdit::table_to_data()
{ // Transfer table data to the data objects
 VectorLinkedList   *vll_cpntr,*vll_cells;
 Vector             *vector;
 ModBaseData        *dobj;
 int                 nil;
 int                 i;
 char                str[16],font[72];

 if(_cvmapp == NULL) return 0;
 vll_cells  = (VectorLinkedList *) _cvmapp->cvm_get_vll(CvmAppBase::Boundarys);
 if(vll_cells) VectorListSetVis(vll_cells,0); // make cells invisible
 _vlist[0] = 0;
 vll_cpntr  = (VectorLinkedList *) _cvmapp->cvm_get_vll(CvmAppBase::Cpointers);
 if(!vll_cpntr) return 0;

 nil = INIL;

 for (i = 0; i < _n; i++)
  { 
    if(_mid[i] == nil) _mid[i]=0; 
    vector = cell_n_vector(vll_cpntr, i+1);
    if(vector)
     {dobj = (ModBaseData *) vector->getData();
      if(dobj)
       {dobj->setpt(_xc[i], _yc[i], _sc[i]);
        dobj->setid((int) _mid[i]);
        dobj->replace(0,1,_xc+i,_yc+i,_sc+i,NULL,NULL);
        sprintf(str,"%d",_mid[i]);
        strcpy(font,"-adobe-*-bold-r-*--20-*-*-*-m-*-iso8859-1");
        vector->setLabel(str,font);
       }
     }

    // attach pointer to the correct cell
    vector = NULL;
    vector = which_cell(vll_cells, _xc+i, _sc+i, _yc+i);
    if(vector)
     {
      dobj = (ModBaseData *) vector->getData();
      if(dobj)
       {dobj->setpt(_xc[i], _yc[i], _sc[i]);
        dobj->setid((int) _mid[i]);
       }
     }

  }
// _cddata->update();
 return (int) _n;
}

Vector *CellEdit::which_cell(VectorLinkedList *vll,float *x, float *y, float *z)
{Vector *vector;
 void   *p;
 if(!vll) return NULL;
 vector = vll->top(&p);
 while(vector != NULL )
  {
   if(cell_inside(vector, x, y, z)) return vector;
   vector = vll->next(&p);
  }
 return NULL;
}

int CellEdit::cell_inside(Vector *vector, float *x, float *, float *z)
{ModBaseData        *dobj;
 int    ins, npts, mem=0;
 int    j,istat;
 float xx[800],zz[800],*xxpntr,*zzpntr;
 if(!vector) return 0;
 dobj = (ModBaseData *) vector->getData();
 if(!dobj) return 0;

 npts  = (int) dobj->getNumPts();
 xxpntr= xx;
 zzpntr= zz;
 if(npts>800)
  {xxpntr = (float *) malloc((unsigned int) npts*sizeof(float));
   zzpntr = (float *) malloc((unsigned int) npts*sizeof(float));
   mem=1;
  }
 j = (long) dobj->getNx(0,(int) npts,xxpntr);
 j = (long) dobj->getNy(0,(int) npts,zzpntr);
 cellpoly_w(x, z, &npts, xxpntr,zzpntr,&ins,&istat);
 if(mem) { free(xxpntr); free(zzpntr); }
 if(ins == 1 && istat==0)
  return 1;
 else
  return 0;
}

Vector *CellEdit::cell_n_vector(VectorLinkedList *list, int n)
{// return n'th vector where n starts counting at 1.
 Vector *v=NULL;
 void *p;
 int m=1;
 if(list==NULL) return v;
 v = list->top(&p);
 while(v != NULL )
  { if(m == n) return v;
    m++;
    v = list->next(&p);
  }
 return NULL;
}

int CellEdit::set_in()
{
 VectorLinkedList   *vll_cpntr,*vll_cells;
 Vector             *vector,*v;
 ModBaseData        *dobj;
 void               *p;
 float               x,y,z;
 int                 i=0,j,iscomplete=1,ins;
 if(_cvmapp == NULL) return 0;
 vll_cpntr  =  _cvmapp->cvm_get_vll(CvmAppBase::Cpointers);
 vll_cells  =  _cvmapp->cvm_get_vll(CvmAppBase::Boundarys);
 if(!vll_cpntr) return 0;
 _numcell = VectorListCount(vll_cells);
 for(j=0;j<_numcell; j++) _has[j]=0;
 vector = vll_cpntr->top(&p);
 while(vector != NULL )
  {
    dobj = (ModBaseData *) vector->getData();
    _in[i]=0;
    if(dobj)
      {
       dobj->getpt(&x,&z,&y);
       for(j=0;j<_numcell;j++)
         {
          v = cell_n_vector(vll_cells,j+1);
          ins = cell_inside(v, &x, &y, &z);
          if(ins >0)
           { _has[j] += 1;
             _in[i] = 1;
           }
         }
      }
    vector = vll_cpntr->next(&p);
    i++;
  }
 for(i=0;i<_n;i++) if(_in[i] <= 0) iscomplete=0;
 for(i=0;i<_numcell;i++) if(_has[i] < 1) iscomplete=0;
 return iscomplete;
}

int CellEdit::has(Vector *v)
{// v should be a vector for a cell boundary.
 // returns the number of pointers inside v.
 VectorLinkedList   *vll_cpntr;
 Vector             *vector;
 ModBaseData        *dobj;
 void               *p;
 float               x,y,z;
 int                 j,ins;
 if(!v) return 0;
 vll_cpntr  =  _cvmapp->cvm_get_vll(CvmAppBase::Cpointers);
 j=0;
 vector = vll_cpntr->top(&p);
 while(vector != NULL )
  {
    dobj = (ModBaseData *) vector->getData();
    if(dobj)
      {
       dobj->getpt(&x,&z,&y);
       ins = cell_inside(v, &x, &y, &z);
       if(ins >0) j += 1;
      }
    vector = vll_cpntr->next(&p);
  }
 return j;
}

void CellEdit::set_hot_label(Vector *v)
{
   if(_hot_label)
    { _hot_label->setColor("green");
      _hot_label = 0;
    }
   _hot_label = v;
   if(_hot_label) _hot_label->setColor("red");
   if(_hot_label) _hot_label->makeVisible();
}

//------------------------ traps -------------------------//

#define ARGUMENTS   void *box, long *ident, long *index,   \
                    char *text, long *nread, char *endkey

void CellEdit::radio_trap(void *box, long *, long *index,
                    char *, long *nread, char *endkey)
{CvmApp *cvmapp;
 VectorLinkedList *vll_cells,*vll_cpntr;
 Vector *vector;

 if(!strcmp("REDRAW", endkey)) return;   // box not valid.
 if(!box) return;
 CellEdit *data = (CellEdit *) SLDatabox::getUserData(box);
 if(strcmp(endkey,"REMOVE")==0)
  { // disallow deletes
    strcpy(endkey," "); return;
  }
 if(strcmp(endkey,"INSERT")==0 && *index <= data->_numcell)
  { //disallow insertions
    strcpy(endkey," "); return;
  }

 cvmapp = data->_cvmapp;
 if(!cvmapp) return;
 vll_cpntr  =  cvmapp->cvm_get_vll(CvmAppBase::Cpointers);
 vll_cells  =  cvmapp->cvm_get_vll(CvmAppBase::Boundarys);
 if( strcmp(endkey,"ARRIVED")==0)
  { 
  }
 if(*index > data->_numcell && *nread > 0)
   { //data->_iradio = data->_old_radio;
    *nread = 0;
    return;
  }
 if(strcmp(endkey,"RETURN")==0)
  { // done automatically data->_iradio = *index;
    data->_iradio = (*index > data->_n) ? data->_numcell : *index;
    if(data->_iradio < 1) data->_iradio=1;
    vector = data->cell_n_vector(vll_cells,(int) *index);
    // unhighlight old cells
    int n=0;
    while(data->_vlist[n] != NULL)
     {if(vector != data->_vlist[n])
       { data->_vlist[n]->makeInvisible();
         data->_vlist[n]->polyFillOff();
       }
      n++;
     }
    data->_vlist[0] = NULL;

    if(vector != NULL) /* highlight the new cell */
     {vector->makeVisible();
      vector->polyFillOn(vector->getColor());
      data->_vlist[0] = vector; data->_vlist[1] = NULL;
     }

    return;
  }
}

void CellEdit::htrap(void *box, long *, long *index, 
                    char *, long *nread, char *endkey)
{// Keep VectorLinkedList for labels in sync with the table
 CvmApp           *cvmapp;
 VectorLinkedList *vll_cpntr,*vll_cells;
 Vector           *vector;
 int   i,icell;

 if(!strcmp("REDRAW", endkey)) return;   // box not valid.
 if(!box) return;

 CellEdit *data = (CellEdit *) SLDatabox::getUserData(box);
 cvmapp = data->_cvmapp;
 if(!cvmapp) return;
 vll_cpntr  =  cvmapp->cvm_get_vll(CvmAppBase::Cpointers);
 vll_cells  =  cvmapp->cvm_get_vll(CvmAppBase::Boundarys);
 if(!vll_cpntr) return;

 if(*index > data->_nmax)
  { *index= data->_nmax;  data->_n = data->_nmax;  return; }

// if(*index > data->_n && *nread > 0)
// { strcpy(msg,"cell_mid: CELL NO. > THAN NUMBER OF CELLS");
//    wbox_messageline(box,"Addition of extra cells not allowed");
//    *nread = 0;
//    *index  = data->_n;
//    return;
//  }


 //if(*index > 1) i=0; 
 //if(*ident==5 && *nread > 0) i=1;

 if(strcmp(endkey,"REMOVE")==0)
  { 
    data->DelEntry(*index);
    strcpy(endkey," "); return;
  }
 if(strcmp(endkey,"INSERT")==0 && *index <= data->_n)
  { //disallow insertions
    strcpy(endkey," "); return;
  }

// replace blanks (i.e. nil) with 0
 int  nil;
 nil = INIL;
 icell = (int) (*index -1);
 if(data->_mid[icell] == nil) data->_mid[icell]=0;


 Vector *vc;
 vector = data->cell_n_vector(vll_cpntr,(int) *index);
 if(strcmp(endkey,"ARRIVED")==0)
  { // unhighlight old cells
   data->set_hot_label(vector);
   return;
  }
 i = icell;
 if(*nread > 0 && *index > data->_n )
   {// Add a new row at the end of the table
    vector = data->update_list(vll_cpntr,data->_mid[i],
      data->_xc[i],data->_sc[i], data->_yc[i],*index,1);
    data->set_hot_label(vector);
    if(vector) vector->makeVisible();
    vc = data->which_cell(vll_cells,data->_xc+i,data->_sc+i,data->_yc+i);
    data->set_in();
    return;
   }
  else if(*nread > 0 )
   {// Replace data for an existing entry?
    data->update_list(vll_cpntr,data->_mid[i],data->_xc[i],data->_sc[i],
     data->_yc[i],*index,1);
    data->set_hot_label(vector);
    data->update_list(vll_cells,data->_mid[i],data->_xc[i],data->_sc[i],
     data->_yc[i],*index,0);
    vc = data->which_cell(vll_cells,data->_xc+i,data->_sc+i,data->_yc+i);
    data->set_in();
    return;
   }

}

Vector *CellEdit::update_list(VectorLinkedList *vll, long id,
 float xo, float yo, float zo, long n, int label)
{// place the passed data into the n'th list entry
 ModLimits        *mlimits;
 ErsTransform     *tx,*ty,*tz;
 Vector           *vector=NULL;
 ModBaseData      *dobj=NULL;
 char  str[24],font[72];
 char  hname[32],cname[32];
 float x=xo,y=yo,z=zo;
 int   lcnt;
 int   mstyle=Vector::XMarker;
 if(!vll) return NULL;


 lcnt = VectorListCount(vll);
 if(n <= lcnt) vector = cell_n_vector(vll,(int) n);

 if(vector) dobj   = (ModBaseData *) vector->getData();

 sprintf(str,"%d",id);
 strcpy(font,"-adobe-*-bold-r-*--20-*-*-*-m-*-iso8859-1");
 if(label <= 0) mstyle = Vector::NoMarker;
 if(dobj)
  {
    dobj->setpt(x,z,y);
    dobj->setid(id);
    if(label>0)
     { vector->setLabel(str,font);
       if(dobj->getNumPts() < 1)
         dobj->insert(0,1,&x,&z,&y,NULL,NULL);
       else
         dobj->replace(0,1,&x,&z,&y,NULL,NULL);
     }
  }
 else
  {// Default Coordinates - see transform.h for definitions
   int phd=XBASEHDR_,shd=YBASEHDR_,thd=NILKEY,zeit=TIMEHDR_;
   int lstyle=Vector::NoLine;
    unsigned int mwidth=1;
    unsigned int lwidth=1,msize;
    msize  = 2*lwidth+5;

    mlimits = _cvmapp->cvmGetMLimits();
    mlimits_get_trans(mlimits,&tx,&ty,&tz);
    phd = (int) transform_gethdr(tx);
    shd = (int) transform_gethdr(ty);
    zeit= (int) transform_gethdr(tz);
    thd = NILKEY;
    dobj = new ModBaseData(1,phd,&x,shd,&y,thd,NULL,zeit,&z,NULL);
    if(dobj == NULL) return NULL;
    dobj->setid((int) id);
    dobj->setpt(x,z,y);
    sprintf(hname,"cell%d",id);
    strcpy(cname,"green");
    if(label > 0)
     {vector = vll->add(dobj,cname,0,False,
       (Vector::VectorStyle) lstyle,
       (Vector::VectorMarker) mstyle,msize,mwidth,str,font);
     }
    else
     {vector = vll->add(dobj,cname,lwidth,False,
       (Vector::VectorStyle) lstyle,
       (Vector::VectorMarker) mstyle,msize,mwidth);
     }
  }
 
 return vector;
}

void CellEdit::compTrap(void *data,long )
{ CellEdit         *cedit = (CellEdit *) data;
  CvmApp           *cvmapp=NULL;
  ModLimits        *mlimits=NULL;
  VectorLinkedList *vll=NULL;
  SeisVectLinkedList *svll;
  ErsTransform     *tx=NULL,*ty=NULL,*tz=NULL;
  long              npts;
  int               vcnt, *ipntr=NULL,*icntr=NULL;
  float            *xval=NULL,*yval=NULL,*zval=NULL;
  float             xmin,xmax,ymin,ymax,zmin,zmax;
  float             YDATUM;
  char              msg[120];

  int               mwrk=150000,maxbpts=15000,maxcell=200;
  int               istat=0,newnum,*cpntr=NULL,*ccntr=NULL;
  float             *xbndry=NULL,*ybndry=NULL,*zbndry=NULL;
  float             *wrk=NULL;
  int               phd,shd,thd,zeit;
  int               i,n;
  if(!data) return;

// Get the structural data.
  cvmapp = (CvmApp *) cedit->_cvmapp;
  if(!cvmapp) return;
  vll = cvmapp->cvm_get_vll(CvmAppBase::Structure);
  vcnt = VectorListCount(vll);
  npts = VectorListPointCount(vll);
  if(npts<=0 ) return;

// Retrieve the model limits and identify coord sys
  mlimits = cvmapp->cvmGetMLimits();
  if(!mlimits) return;
  mlimits_get(mlimits,&xmin,&xmax,&zmin,&zmax,&ymin,&ymax,
             &tx, &ty, &tz );
  phd = (int) transform_gethdr(tx);
  shd = (int) transform_gethdr(ty);
  zeit= (int) transform_gethdr(tz);
  thd = NILKEY;

// Extract geometry of the boundaries.
// (you need to free memory allocated by the call)
  vcnt = VectorListSaccess(vll, &vcnt,&ipntr,
     &icntr,&xval,&yval,&zval);
// Compensate for 2D nature of CELL call.
// Set y-values of boundary to a mean value.
 YDATUM = 0.0;
 for(i=0;i<npts;i++) YDATUM += yval[i];
 YDATUM /= npts;

// Allocate arrays to receive cell boundary data.
 cpntr = (int *) malloc((unsigned int) maxcell*sizeof(int));
 ccntr = (int *) malloc((unsigned int) maxcell*sizeof(int));
 xbndry = (float *) malloc((unsigned int) maxbpts*sizeof(float));
 ybndry = (float *) malloc((unsigned int) maxbpts*sizeof(float));
 zbndry = (float *) malloc((unsigned int) maxbpts*sizeof(float));
 wrk    = (float *) malloc((unsigned int) mwrk*sizeof(float));

// Compute the new cell boundaries.
 cvmapp->setMessage("Wait: computing cells");
 cellcalc(&xmin,&xmax,&zmin,&zmax,
           &vcnt,ipntr,icntr,xval,zval,
           &maxcell,&maxbpts,&newnum,cpntr,ccntr,xbndry,zbndry,
           &mwrk,wrk,&istat);
 cvmapp->setMessage("");
 if(istat != 0)
  {
    sprintf(msg,"compTrap: call to cell failed, istat=%d\n",istat);
    printf("%s",msg);
    goto jump;
  }

 svll = new SeisVectLinkedList();
 Vector *vect,*vectors[150];
// Find a point inside to initialize each new cell.
 int   m;
 float xo,yo,zo,xso,yso,zso,rso;
 int   iso,ins,mid;
 int   new_cno[100],cellno;
 char  hname[32],cname[32];
 strcpy(cname,"green");
 for(n=0;n<newnum;n++)
  {new_cno[n] = -1;
   xo = 0; yo = yval[0]; zo=0; mid = -1;
   yso=yval[0];
   i = (int) cpntr[n];
   cellpnt_w(&xo,&zo,&ccntr[n],&xbndry[i],&zbndry[i],&ins,&istat);
   yo = yval[0];
   if(istat == 0)
    {// identify the cell containing xo,zo
       for(m=0;m<ccntr[n];m++) {ybndry[i+m] = YDATUM;}
       cellwher_w(&xo,&zo,&iso,&new_cno[n],&xso,&zso,&rso,
        &newnum,cpntr,ccntr,xbndry,zbndry,&istat);
       sprintf(hname,"cell%d",n);
       vectors[n] = VectorListAdd(svll, hname,cname,phd, shd, thd, zeit,
        mid, xo, yo, zo, (int) ccntr[n], xbndry+i,ybndry+i,zbndry+i);
    }
   else
    {sprintf(msg,"compTrap: call to cellpnt failed, istat=%d\n",istat);
     printf("%s\n",msg);
     goto jump;
    }
  }

// Identify the old cells.
//   1. get the geometric info
//   2. from the cell pointer, get the cell number
  VectorLinkedList *vll_cpntr;
  int               oldnum;
  vll_cpntr  =  cvmapp->cvm_get_vll(CvmAppBase::Cpointers);
  oldnum = VectorListCount(vll_cpntr);
  if(oldnum>0 ) ;
  {
// Find the cell number for point (xo,yo,zo)
// for the old cells.
    Vector *vector;
    ModBaseData *dobjo,*dobjn;
    void        *p;
    vect = vll_cpntr->top(&p);
    while(vect != NULL)
     { cellno=-1;
       dobjo = (ModBaseData *) vect->getData();
       if(dobjo)
        { mid = (long) dobjo->getid();
          dobjo->getpt(&xo,&zo,&yo);

          vector = cedit->which_cell(svll, &xo, &yo, &zo);
          if(vector)
           {dobjn = (ModBaseData *) vector->getData();
            dobjn->setid((int) mid);
            dobjn->setpt(xo,zo,yo);
           }

        }
      vect = vll_cpntr->next(&p);
     }
    }

/* Kill old vector lists for cells */
 void *sp;
 VectorListData  *vldata;
 sp = cvmapp->cvm_get_seisplot(cvmapp);
 KillVectorList((void *) cvmapp->cvm_get_vll(CvmAppBase::Boundarys), sp);
 if(svll != NULL) svll->addPlot((SeisPlot *) sp);
 vldata = new VectorListData((VectorLinkedList *) svll,NULL);
 cvmapp->cvm_set_vldata(CvmAppBase::Boundarys,vldata);
// VectorListSetVis((void *) svll, 1);
 cedit->_numcell = VectorListCount( (VectorLinkedList *) svll);
 cedit->set_in();
 cedit->_vlist[0]=0;

jump:
 if(ipntr) free(ipntr);
 if(icntr) free(icntr);
 if(xval) free(xval);
 if(yval) free(yval);
 if(zval) free(zval);
 if(xbndry) free(xbndry);
 if(ybndry) free(ybndry);
 if(zbndry) free(zbndry);
 if(cpntr) free(cpntr);
 if(ccntr) free(ccntr);
 if(wrk) free(wrk);
}

int CellEdit::DelEntry(long row)
{// Remove a cell pointer
 VectorLinkedList *vll_cpntr;
 Vector *vector;
 ModBaseData *dobj;
 int  count;
 
 if(row < 1 || row > _n) return 0;
 if(_n == 1) return 0; // dont delete last entry
 vll_cpntr  =  _cvmapp->cvm_get_vll(CvmAppBase::Cpointers);
 vector = cell_n_vector(vll_cpntr,(int) row);
 if(vector==_hot_label) set_hot_label(0);
 if(vector)
  { 
    dobj = (ModBaseData *) vector->getData();
    vll_cpntr->remove(vector);
    delete dobj;
  }
// Reset the table display
 count = CE_Set();
 return 1;
}

//--------------------- end --------------------//


//------------------- cell_edit_pop.cc -------------------//
//       implementation file for the CellEditPop class
//              derived from the SLDialog class
//        Creates a CellEdit class inside a dialogue 

//------------- constructors and destructor --------------//

CellEditPop::CellEditPop (SLDelay *slparent, char *name, HelpCtx Hctx,
         void *cvm)
        : SLDialog(slparent, name, Hctx,True)
{ SLSmartForm *work = workArea();
  SLpPush     *cc;
  SLpPush     *rem = addBottomRemove(REMOVE);
                    addBottomKeyhelp();
                    addBottomHelp();
//  remove->unmanageShellWhenPressed(this);
  setTitle("Edit Cell Pointers");
  cc = new SLpPush( work, "cellpb",0,"Compute Cells");
  work->attach(cc, work, work, NULL, work,4,4,0,4);
  _cedit = new CellEdit(work,"celldb", cvm, Hctx, False, True, True );
  work->attach(_cedit, work, work, work, cc,4,4,0,4);
  cc->setAtrap(_cedit->compTrap, _cedit); //static member function
  _slform = work;
}

CellEditPop::CellEditPop (Widget parent, char *name, HelpCtx Hctx,
         void *cvm)
        : SLDialog(parent, name, Hctx,True)
{ SLSmartForm *work = workArea();
  SLpPush     *cc;
  SLpPush     *rem = addBottomRemove(REMOVE);
                    addBottomKeyhelp();
                    addBottomHelp();
  setTitle("Edit Cell Pointers");
  cc = new SLpPush( work, "cellpb",0,"Compute Cells");
  work->attach(cc, work, work, NULL, work,4,4,0,4);
  _cedit = new CellEdit(work,"celldb", cvm, Hctx, False, True, True);
  cc->setAtrap(_cedit->compTrap, _cedit); 
  work->attach(_cedit, work, work, work, cc,4,4,0,4);
  _slform = work;
}


CellEditPop::~CellEditPop(void)
{// CellEdit is deleted automatically by its parent
 //if(_cedit != NULL) delete _cedit;
}

Boolean CellEditPop::removeNotify()
{
 if(_cedit)
  {
   _cedit->table_to_data(); return True;
  }
  else return False;
}

Widget  CellEditPop::Form()
{if(_slform) return _slform->W();
 return NULL;
}
//--------------- end ----------------------//
