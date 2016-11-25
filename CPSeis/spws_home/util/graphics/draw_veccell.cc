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
#include "draw_mod.h"
#include "vect/ll_seis_vect.hh"


/********
C\USER DOC
C-----------------------------------------------------------------------
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                             U T I L I T Y 
C       written in c -- designed to be called from c
C
C     Utility Name:  draw_vcell,draw_vcells
C          Written:  93/01/01  by:  R.S.Day
C     Last revised:            by:  
C
C  Purpose:     Plot the boundary data in ErsCell and fill the interior.
C
C-----------------------------------------------------------------------
C                          LOCATION OF CODE
C
C  machine            source code directory       library
C  -------            ---------------------       -------
C  ultrix             ~spws/util/graphics         graphlib.a
C
C  c files          c++ files       fortran files      other files
C  -------          ---------       -------------      -----------
C  none             draw_veccell.c  
C----------------------------------------------------------------------
C          LIBRARIES AND HEADER FILES REQUIRED BY THIS UTILITY
C
C  Libraries:
C  Header files: draw_mod.h(vector.hh  modbase_data.hh(base_data.hh))
C                ll_seis_vect.hh model.h
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  1. 94/04/1333R.S.Day    Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                CALLING SEQUENCE        (I/O: IN, OUT, BOTH)
C
C  int draw_vcells(void *vl,ErsCells *cells,int pheader,long visible,
C      char *msg);
C  int draw_vcell(void *vl,ErsCell *cell, int pheader,long visible,
C      char *msg);
C  void CellSetVisible(ErsCell *cell,int visible);
C
C  Type    Name    I/O     Valid     Description  
C  ----    ----    ---     -----     -----------
C  void *  vl      IN                SeisVectLinkedList object
C  int     pheader IN      0-64      Flags header matching the x-coordinate
C  long    visible IN      0,1       0 = Undraw cell and label
C                                    1 = Draw the cell and label
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. draw_vcells returns the number of cells drawn.
C  2. draw_vcell returns 0 if it failed to draw, and 1 if it did.
C-----------------------------------------------------------------------
C\END DOC
****/
int draw_vcells(void *VL,ErsCells *cells,int phd,int shd,int thd,int zeit,
    long idraw, char *msg )
{ ErsCell *cell;
  int i,nplotted;
  
  nplotted = 0;
  if(cells==NULL) return 0;
  for(i=0;i<cells_count(cells);i++)
   {cell = cells_get_nth(cells,i);
    nplotted += draw_vcell(VL,cell, phd,shd,thd,zeit, idraw, msg );
   }
 if(nplotted != cells_count(cells))
  sprintf(msg,"draw_vcells: fewer cells plotted than exist\n");
 return nplotted;
}

int draw_vcell(void *VL,ErsCell *cell,int phd,int shd,int thd,int zeit,
    long idraw, char *msg )
{ 

  Vector   *vect;
  SeisVectLinkedList *vl;




 int   lstyle,mstyle;
 char  cname[32];
 unsigned int   lwidth,msize,mwidth;

 vl = (SeisVectLinkedList *) VL;
 msg[0]='\0';
 if(cell == NULL )
  {sprintf(msg,"draw_vcell: no cell to plot\n");
   return 0;
  }

 vect  = (Vector *) cellGetVect(cell);

 if(vect == NULL)
  {if(vl == NULL )
    {sprintf(msg,"draw_vcell: no drawable for plot\n");
     return 0;
    }
    vect = CellToVector(vl, cell,phd,shd,thd,zeit);
    if(vect == NULL) return 0;
    CellSetVisible(cell,(int) idraw);
  }
 else
  {lstyle = (int)  vect->getStyle();
   lwidth = vect->getWidth();
   strcpy(cname,vect->getColor());
   vect->getMarker((Vector::VectorMarker *) &mstyle,&msize,&mwidth);
   CellSetAttributes(cell,lstyle, lwidth,
     mstyle, msize, mwidth,cname, (int) idraw);
  }

 return 1;
}

void CellSetVisible(ErsCell *cell,int visible)
{Vector *vect;
 vect  = (Vector *) cellGetVect(cell);
 if(vect == NULL) return;
 if(visible != 0) vect->makeVisible();
    else vect->makeInvisible();
}

void CellSetAttributes(ErsCell *cell,int lstyle, unsigned int lwidth,
     int mstyle, unsigned int msize, unsigned int mwidth,char *color,
     int visible)
{Vector *vect;
 vect  = (Vector *) cellGetVect(cell);
 if(vect == NULL) return;
 vect->makeInvisible();
 vect->setColor(color);
 vect->setWidth(lwidth);
 if(lstyle >= 0) vect->setStyle((Vector::VectorStyle) lstyle);
 if(mstyle >= 0) vect->setMarker((Vector::VectorMarker) mstyle,msize,mwidth);
 if(visible != 0) vect->makeVisible();
 else vect->makeInvisible();

 strcpy(cell->color,color);
}
 

int draw_vcell_labels(void *vls,ErsCells *cells,int phd,int shd, int zeit,
                       int vis, char *msg )
{int i,j;
 ErsCell *cell;
 if(cells == NULL) return 0;

 j = 0;
 for(i=0;i<cells_count(cells);i++)
   { cell  = cells_get_nth(cells, i);
    j += draw_vcell_label(vls,cell,phd,shd,zeit,vis, msg);
   }

 return j;
}

int draw_vcell_label(void *vls,ErsCell *cell,int phd, int shd, int zeit,
    int vis, char *msg )
{ModBaseData *dobj,*dcell;
 Vector   *vect,*vect_lab;
 SeisVectLinkedList *vl;
 char  str[16],font[72];
 float xi,zi,yi;
 int  mid;

 int   lstyle,mstyle;


 lstyle = Vector::NoLine;
 mstyle = Vector::XMarker;
 strcpy(font,"-adobe-*-bold-r-*--20-*-*-*-m-*-iso8859-1");

  msg[0]='\0';
  if(cell == NULL )
   { sprintf(msg,"draw_vcell: no cell to plot\n");
     return 0; }
  cell_get3(cell,&xi,&zi,&yi,&mid);
  if(vls == NULL )
   { sprintf(msg,"draw_vcell: SeisVectLinkedList is NULL\n");
     return 0; }
  vl = (SeisVectLinkedList *) vls;
/* Check to see if label was created already */
  vect_lab= (Vector *) cell->vectlab;
  if(vect_lab  == NULL)
   {
    sprintf(str,"%d",mid);
    dobj = new ModBaseData(1,phd,&xi,shd,&yi,NILKEY,NULL,zeit,&zi,NULL);
    if(dobj == NULL) return 0;
    dobj->setid((int) mid);
    dobj->setpt(xi,zi,yi);
    vect_lab = vl->addLabel(dobj,str,cell_getcolor(cell),font);
/* vect_lab->seetMarker((Vector::VectorMarker) mstyle,msize,mwidth);*/
    if(vis != 0) vect_lab->makeVisible();
    else vect_lab->makeInvisible();
    cell->vectlab = (void *) vect_lab;
   }
  else
   {
    dobj = (ModBaseData *) vect_lab->getData();
    if(dobj==NULL) return 0;
    dobj->replace(0,1,&xi,&zi,&yi,NULL,NULL);
    dobj->setid((int) mid);
    dobj->setpt(xi,zi,yi);
    sprintf(str,"%d",mid);
    vect_lab->setLabel(str,font);
    if(vis != 0) vect_lab->makeVisible();
    else vect_lab->makeInvisible();
   }
  vect = (Vector *) cellGetVect(cell);
  if(vect != NULL)
   {dcell= (ModBaseData *) vect->getData();
    dcell->setid((int) mid);
    dcell->setpt(xi,zi,yi);
   }

 return 1;
}

ErsCells *VectorListToCells(void *vls)
{// Convert a vector list to an ErsCells structure.
 SeisVectLinkedList *vlist;
 Vector *vect;
 ModBaseData *dobj;
 ErsCells *cells;
 ErsCell  *cell;
 int i;
 char  name[96],color[64];
 if(vls == NULL) return NULL;
 vlist = (SeisVectLinkedList *) vls;

 vect = vlist->top();
 if(vect==NULL) return NULL;
 cells = new_cells();
 while(vect != NULL)
  {
   dobj = (ModBaseData *) vect->getData();
   strcpy(name,"dummy");
   if(vect->getName() != NULL) strcpy(name,vect->getName());
   strcpy(color,"red");
   if(vect->getColor() != NULL) strcpy(color,vect->getColor());

   cell = BaseDataToCell(dobj,NULL); // create a new cell
   cell_setname(cell,name);
   cell_setcolor(cell,color);
   cellSetVect(cell,vect);
   i = cells_add(cells,cell);
   vect = vlist->next();

 }

 return cells;
}

SeisVectLinkedList *CellsToVectorList(ErsCells *cells,
      int phd,int shd, int thd, int zeit)
{//Convert the boundary info in an ErsCells structure to a vector list.
 SeisVectLinkedList *vl;
 Vector *vect;
 ErsCell *cell;
 int i;

  if(cells==NULL) return 0;
  if(cells_count(cells) < 1) return 0;
  vl = new SeisVectLinkedList();
  if(vl == 0) return 0;
  for(i=0;i<cells_count(cells);i++)
   {cell = cells_get_nth(cells,i);
    vect = CellToVector(vl, cell, phd, shd,thd,zeit);
   }
 return vl;
}

SeisVectLinkedList *CellsToVectorList2(ErsCells *cells,
      int phd,int shd, int thd, int zeit)
{//Convert the cell pointer info in an ErsCells structure to a vector list.
 SeisVectLinkedList *vl;
 Vector *vect;
 ErsCell *cell;
 int i;

  if(cells==NULL) return 0;
  if(cells_count(cells) < 1) return 0;
  vl = new SeisVectLinkedList();
  if(vl == 0) return 0;
  for(i=0;i<cells_count(cells);i++)
   {cell = cells_get_nth(cells,i);
    vect = CellToVector2(vl, cell, phd,shd,thd,zeit);
   }
 return vl;
}

Vector *CellToVector(SeisVectLinkedList *vl, ErsCell *cell,
      int phd,int shd, int thd, int zeit)
{// Convert cell boundary to a vector and add to a list.
 ModBaseData *dobj;
 Vector *vect;
 int   lstyle,mstyle;
 char  hname[32],cname[32];
 unsigned int   lwidth=3,msize,mwidth=1;

 if(cell == NULL || vl ==NULL ) { return NULL; }

 strcpy(hname,cell_getname(cell));
 strcpy(cname,cell_getcolor(cell));
 msize  = 2*lwidth+5;
 lstyle = Vector::SolidLine;
 mstyle = Vector::NoMarker;
 dobj   = CellToBaseData(cell,phd,shd,thd,zeit);
 if(dobj == NULL) return 0;
 vect = vl->add(hname,dobj,cname,lwidth,False,
           (Vector::VectorStyle) lstyle,
           (Vector::VectorMarker) mstyle,msize,mwidth);
 if(vect == NULL) return NULL;
 cellSetVect(cell,(void *) vect);
 vect->makeInvisible();
 return vect;
}

Vector *CellToVector2(SeisVectLinkedList *vl, ErsCell *cell,
      int phd,int shd, int thd, int zeit)
{// Convert cell pointer to a vector and add to a list.
 ModBaseData *dobj;
 Vector *vect;
 float xi,zi,yi;
 int  mid;
 int   lstyle,mstyle;
 unsigned int mwidth=1;
 char  hname[32],cname[32],str[16],font[72];
 unsigned int   lwidth=3,msize;
 strcpy(font,"-adobe-*-bold-r-*--20-*-*-*-m-*-iso8859-1");
 thd = NILKEY;
 if(cell == NULL || vl ==NULL ) return 0;

 strcpy(hname,cell_getname(cell));
 strcpy(cname,cell_getcolor(cell));
 lstyle = Vector::NoLine;
 mstyle = Vector::XMarker;
 cell_get3(cell,&xi,&zi,&yi,&mid);
 msize  = 2*lwidth+5;
 sprintf(str,"%d",mid);
 dobj = new ModBaseData(1,phd,&xi,shd,&yi,thd,NULL,zeit,&zi,NULL);
 if(dobj == NULL) return 0;
 dobj->setid((int) mid);
 dobj->setpt(xi,zi,yi);
 vect = vl->add(dobj,cname,0,False,
           (Vector::VectorStyle) lstyle,
           (Vector::VectorMarker) mstyle,msize,mwidth,str,font);
 vect->makeInvisible();
 cell->vectlab = (void *) vect;
 return vect;
}

ModBaseData *CellToBaseData(ErsCell *cell,
   int phd,int shd, int thd, int zeit)
{// Convert cell boundary to a new ModBaseData object.
 ModBaseData *dobj;
 float *xb,*zb,*yb,xi,zi,yi;

 int  npts,mid;
 // shd=NILKEY;   override argument and treat as dummy
 thd=NILKEY;

 if(cell == NULL) return NULL;
 cell_get_bnd3ptr(cell,&npts,&xb,&zb,&yb);
 if(npts <=0) return NULL;
 cell_get3(cell,&xi,&zi,&yi,&mid);
 dobj = new ModBaseData((int) npts,phd,xb,shd,yb,thd,NULL,zeit,zb,NULL);
 dobj->setid((int) mid);
 dobj->setpt(xi,zi,yi);
 return dobj;
}

ErsCell *BaseDataToCell(ModBaseData *dobj,ErsCell *cellin)
{// copy cell boundary information from dobj to cell
 ErsCell *cell;
 float *xb,*zb,*yb,xi,zi,yi;
 int   i;
 unsigned int num;
 int  npts,mid;

 if(dobj == NULL) return NULL;
 npts = dobj->getNumPts();
 if(npts <=0) return NULL;
 if(cellin == NULL)
  cell = (ErsCell *) new_cell();
 else
  cell = cellin;
 num = npts*sizeof(float);
 xb = (float *) malloc(num);
 zb = (float *) malloc(num);
 yb = (float *) malloc(num);
 i = dobj->getNx(0,(int) npts,xb); 
 i = dobj->getNy(0,(int) npts,zb); 
 i = dobj->getNs(0,(int) npts,yb); 
 cell_set_bnd3(cell,npts,xb,zb,yb);
 if(xb) free(xb);
 if(zb) free(zb);
 if(yb) free(yb);
 mid = (int) dobj->getid();
 dobj->getpt(&xi,&zi,&yi);
 cell_set3(cell,&xi,&zi,&yi,&mid);
 cell_setdim(cell,3);
 return cell;
}
