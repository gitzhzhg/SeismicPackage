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
#ifndef VA_CONST_H
#define VA_CONST_H


#define MAX_DRAW    5
#define ISOVEL      0
#define GVS         1
#define SEM         2
#define CMP         3
#define VGRID       4

#define ISOFLG  (1<<ISOVEL)
#define GVSFLG  (1<<GVS)
#define SEMFLG  (1<<SEM)
#define CMPFLG  (1<<CMP)
#define GRIDFLG (1<<VGRID)
#define ALLFLG  ~0


#define VEL         100

/*picking actions constants*/
#define MODIFYP     1
#define DELETEP     2
#define INSERTP     3
#define NOACTION    4

/*velocity posting constants*/
#define  XCURRENTF     1
#define  XPREVIOUSF    2
#define  XNEXTF        3
#define  XREFERENCEF   4
#define  YPREVIOUSF    5
#define  YNEXTF        6
#define  REFF          7

/*iso velocity x,y,z types*/
#define XTYPE   0
#define YTYPE   1
#define ZTYPE   2


/*gvs and cvst header to get gvs modifier and cvst velocities from*/
#define VELHEADER 62

#endif






