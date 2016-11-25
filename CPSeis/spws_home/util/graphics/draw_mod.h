#ifndef _draw_mod
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
#define _draw_mod

#include "model.h"
#include "pick.h"
#include "vect/vector.hh"
#include "data/modbase_data.hh"

class SeisVectLinkedList;

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

//int draw_vpdat(void *vl,PR_ *pr,int *lorm,  int *labels,
//             long *iwk, float *ul, float *ur, float *ut, float *ub);

/* following are in draw_vecpdat.cc */
int draw_vhdat(void *vl, PR_ *pr, ErsHorizon *horizon,int *lorm, int zeit);
Vector *draw_vsdat(void *VL, ErsHorizon *horizon ,
       int phd,int shd, int thd, int zeit);

void SegmentSetVisible(ErsHorizon *hz,int visible);
void SegmentSetAttributes(ErsHorizon *hz,int lstyle, unsigned int lwidth,
     int mstyle, unsigned int msize, unsigned int mwidth,char *color,
     int visible);
ModBaseData *SegmentToModBaseData(ErsHorizon *horizon,
             int phd,int shd,int thd, int zeit);
int ModBaseDataToHorizon(ModBaseData *dobj,ErsHorizon *h);
SeisVectLinkedList *PR_ToVectorList(PR_ *pr,
             int phd,int shd, int thd, int zeit);
PR_ *VectorListToPR_(void *vector_list);


/* following are in draw_vecmdat.cc */
int draw_vmdats(void *VL,ErsMaterials *mats,
    int phd,int shd,int thd,int zeit,int idraw, char *msg );
int draw_vmdat(void *VL,ErsMaterial *mat,
    int phd,int shd,int thd,int zeit,int idraw,char *msg);
void MaterialSetVisible(ErsMaterial *mat,int visible);
void MatSetAttributes(ErsMaterial *mat,int lstyle, unsigned int lwidth,
     int mstyle, unsigned int msize, unsigned int mwidth,char *color,
     int visible);
ErsMaterials *VectorListToMdata(void *vector_list);
SeisVectLinkedList *MdataToVectorList(ErsMaterials *mats,
    int phd,int shd,int thd,int zeit);
int MaterialToBaseData(ErsMaterial *mat, ModBaseData ***objarr,
    int phd,int shd,int thd,int zeit);

/* following are in draw_veccell.cc */
int draw_vcells(void *VL,ErsCells *cells,int phd,int shd,int thd,int zeit,
     long idraw, char *msg );
int draw_vcell(void *VL,ErsCell *cell,int phd,int shd,int thd,int zeit,
     long idraw, char *msg );
void CellSetVisible(ErsCell *cell,int visible);
void CellSetAttributes(ErsCell *cell,int lstyle, unsigned int lwidth,
     int mstyle, unsigned int msize, unsigned int mwidth,char *color,
     int visible);
int  draw_vcell_labels(void *vls,ErsCells *cells,int phd,int shd,int zeit,
     int vis, char *msg );
int draw_vcell_label(void *vls,ErsCell *cell,int phd,int shd, int zeit,
     int vis, char *msg );
ErsCells *VectorListToCells(void *vls);
SeisVectLinkedList *CellsToVectorList(ErsCells *cells,
     int phd,int shd, int thd, int zeit);
SeisVectLinkedList *CellsToVectorList2(ErsCells *cells,
     int phd,int shd, int thd, int zeit);
Vector *CellToVector(SeisVectLinkedList *vl, ErsCell *cell,
     int phd,int shd, int thd, int zeit);
Vector *CellToVector2(SeisVectLinkedList *vl, ErsCell *cell,
     int phd,int shd, int thd, int zeit);
ModBaseData *CellToBaseData(ErsCell *cell,
     int phd,int shd, int thd, int zeit);
ErsCell *BaseDataToCell(ModBaseData *dobj, ErsCell *cellin);

/* following are in vlist.cc */
int UniqueVectNameList(void *ll,int *num ,int lsiz, char **names);
void UniqueVectNameFree(char **names);

#ifdef __cplusplus
}                   // for C++
#endif

#endif






