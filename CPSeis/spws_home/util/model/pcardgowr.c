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
#include "c2f_interface.h"
#include "pcardgowr.h"

#if NEED_CAPITALS
#define bswap_   BSWAP
#endif

#if( VMS || _AIX || __hpux || LINUX)
#define bswap_       bswap
#endif

/*
                             pcardgowr.c

************************* COPYRIGHT NOTICE ****************************
*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
************************* COPYRIGHT NOTICE ****************************
C\USER DOC
-----------------------------------------------------------------------
                    SEISMIC PROCESSING WORKSTATION
                          C-LANGUAGE UTILITY
             designed to be called from C or C++

  Utility name:  pcardgowr    (Write out LAYER model as a gocad++ file)

  Subdirectory:  util/model
  Library:       libmodel.a
  Header file:   pcardgowr.h
  Source file:   pcardgowr.c

  Written:       95/03/28  by:  R.S.Day
  Last revised:  96/08/16  by:  R.S.Day

  Purpose:       Given a model, save it in a  gOcad++ file 

  Related Documentation:
-----------------------------------------------------------------------
                       GENERAL INFORMATION

 The functions in this file take care of translating a Conoco
 model into a gocad++ file. The functions in this file will deal
 with Conoco LAYER, G3DL, and GRID models.

-----------------------------------------------------------------------
                   INPUT AND OUTPUT ARGUMENTS

      i = value required upon INPUT to the function.
      o = value set by the function upon OUTPUT.
      b = value BOTH required upon input and changed upon output.

                           i            i
 int pcardgowr(ErsModel *model, char *fileo)


 char *fileo     --> name of the output file.
 ErsModel *model --> A Conoco layer model(see model.h)

-----------------------------------------------------------------------
                        REVISION HISTORY

     Date      Author     Description
     ----      ------     -----------
  3. 96/08/16  R.S. Day   Corrected write of multi segment horizons
  2. 95/08/09  R.S. Day   Added more output properties for mats.
  1. 95/03/28  R.S.Day    Initial version.
-----------------------------------------------------------------------
C\END DOC
*/


int pcardgowr(ErsModel *model, char *fileo)
{FILE *gofil;
 char *mtype;
 char *dfile;

 if(model == NULL)  return 0;
 mtype = model_gettype(model);
 dfile = model_getdfile(model);
 gofil = fopen(fileo,"w");
 if(!gofil)  return 0;
 fclose(gofil);
 
 if(strcmp(mtype,"LAYER")==0)
   {
    pcardgowr_xslim(model, fileo);
    pcardgowr_pik(model, fileo);
   }
 pcardgowr_mat(model, fileo);
 pcardgowr_cpntr(model, fileo);
/* pcardgowr_bnd(model, fileo); */

 if(strcmp(mtype,"G3DL")==0)
  pcardgowr_g3dl(model, fileo);
 if(strcmp(mtype,"GRID")==0)
  pcardgowr_grid(model_getglim(model), model_getdfile(model),
                 model_getwtype(model),fileo, "GridMod");
 return 1;
}

void pcardgowr_xslim(ErsModel *model, char *fileo)
{ FILE         *gofil;
  ErsCells     *cells;
  ErsCell      *cell;
  ModLimits    *mlim=NULL;
  float        y=0,xmin,xmax,ymin,ymax,zmin,zmax;
  float        xi,yi=0,zi;
  ErsTransform *tx,*ty,*tz;
  int          cnt=0;
  int          cell_id;

  if(!model)  return;
  mlim = model_getmlim(model);
  if(!mlim) return;
  mlimits_get(mlim,&xmin,&xmax,&zmin,&zmax,&ymin,&ymax,&tx,&ty,&tz);
  y = ymin;
  cells  = model_getcdata(model);
  cell = cells_get_nth(cells,0);
  if(cell)
   {
    cell_get3(cell,&xi,&zi,&yi,&cell_id);
    y = yi;
   }
  gofil = fopen(fileo,"a+");
  if(!gofil)  return;

   fprintf(gofil,"GOCAD PLINE 1.0\n");
   fprintf(gofil,"HEADER {\n");
   fprintf(gofil,"*name: xsection_limits\n");
   fprintf(gofil,"*color: yellow\n");
   fprintf(gofil,"}\n");

   fprintf(gofil," VRTX %4d %10.4f %10.4f %10.4f\n", cnt,xmin,y,zmin);
   cnt++;
   fprintf(gofil," VRTX %4d %10.4f %10.4f %10.4f\n", cnt,xmin,y,zmax);
   cnt++;
   fprintf(gofil," VRTX %4d %10.4f %10.4f %10.4f\n", cnt,xmax,y,zmax);
   cnt++;
   fprintf(gofil," VRTX %4d %10.4f %10.4f %10.4f\n", cnt,xmax,y,zmin);
   cnt++;

   fprintf(gofil," SEG %5d  %5d\n",0,1);
   fprintf(gofil," SEG %5d  %5d\n",1,2);
   fprintf(gofil," SEG %5d  %5d\n",2,3);
   fprintf(gofil," SEG %5d  %5d\n",3,0);

   fprintf(gofil,"END\n\n");
  fclose(gofil);
}

void pcardgowr_pik(ErsModel *model, char *fileo)
{ FILE         *gofil;
  PR_          *pikrec;
  ErsHorizon   *thorizon,*uhlist[99],*slist[99];
  ErsPoint     *point;
  int i,j,hnumber,snumber,count;
  int n,m, cnt=0;
  int  nclass;
  char *name,*cname,tname[108];
  float xvals[8];

  if(model == NULL)  return;
  pikrec = (PR_ *) model_getpdata(model);

  gofil = fopen(fileo,"a+");
  if(!gofil)  return;
/*
 * Write out the structural picks first.
 */
 ErsHorizonGetHList(pikrec, &nclass, uhlist);
 for(i=0;i<nclass;i++)
  {
   cnt  = 0;
   ErsHorizonMemberList(pikrec, uhlist[i], &count, &snumber, slist);
   if(snumber == -1) snumber = 1;
   name     = uhlist[i]->horizon_name;
   cname    = uhlist[i]->color_name;
   fprintf(gofil,"GOCAD PLINE 1.0\n");
   fprintf(gofil,"HEADER {\n");
   fprintf(gofil,"*name:%s\n",name);
   fprintf(gofil,"*color:%s\n",cname);
   sprintf(tname,"TS_%s",name);
   fprintf(gofil,"*ts_name:%s\n",tname);
   fprintf(gofil,"}\n");
   for(j=0;j<count;j++)
    {thorizon = slist[j];
     point    = thorizon->first_point;
     hnumber  = thorizon->hid;
     while (point != NULL)
      {for (n=0; n<=point->npicks; n++)
        {
         if (n == point->npicks)
          {
           xvals[0] = point->pkey;
           xvals[1] = point->time;
           xvals[4] = point->user_data;
           xvals[5] = point->skey;
           xvals[6] = point->tkey;
          }
         else
          {
           xvals[0] = point->pick_list[n].pkey;
           xvals[1] = point->pick_list[n].time;
           xvals[4] = point->pick_list[n].user_data;
           xvals[5] = point->pick_list[n].skey;
           xvals[6] = point->pick_list[n].tkey;
          }
         xvals[2] = hnumber;
         xvals[3] = snumber;
         cnt++;
         fprintf(gofil," VRTX %4d %10.4f %10.4f %10.4f\n",
                 cnt,xvals[0],xvals[5],xvals[1]);
        }
       point = point->next;
      }
    }
   cnt = 0;
   for(j=0;j<count;j++)
    {
     m = ErsHorizonGetNumPicks(slist[j]);
     for(n=cnt;n<cnt+m-1;n++) fprintf(gofil," SEG %5d  %5d\n",n+1,n+2);
     cnt += m;
    }
    fprintf(gofil,"END\n\n");

  }

 fclose(gofil);
}

void pcardgowr_cpntr(ErsModel *model, char *fileo)
{FILE         *gofil;
 ErsCells     *cells;
 ErsCell      *cell;
 int           i;
 if(model==NULL || fileo==NULL) return;
 cells  = model_getcdata(model);

  gofil = fopen(fileo,"a+");
  if(!gofil)  return;

/*
 * Output the cell pointer data
 */
 if(cells)
  {int ncell;
   float xi,yi,zi;
   int   cell_id;
   ncell =cells_count(cells);
   if(ncell<1) goto jmpout;
   fprintf(gofil,"GOCAD VSET 1.0\n");
   fprintf(gofil,"HEADER {\n");
   fprintf(gofil,"*name:%s\n","cellpntrs");
   fprintf(gofil,"*color:%s\n","red");
   fprintf(gofil,"}\n");

   fprintf(gofil,"PROPERTIES material_id\n");
   for(i=0;i<ncell;i++)
    { cell = cells_get_nth(cells,i);
      if(cell == NULL) break;
      cell_get3(cell,&xi,&zi,&yi,&cell_id);
      fprintf(gofil,"PVRTX %4d %10.4f %10.4f %10.4f %4d\n",
       i,xi,yi,zi,cell_id);
    }
   fprintf(gofil,"END\n\n");
  }

jmpout:
 fclose(gofil);
}

void pcardgowr_bnd(ErsModel *model, char *fileo)
{FILE         *gofil;
 ErsCells     *cells;
 ErsCell      *cell;
 char *name,defname[32];
 int           i,j;
 if(model==NULL || fileo==NULL) return;
 cells  = model_getcdata(model);

  gofil = fopen(fileo,"a+");
  if(!gofil)  return;

/*
 * Output the cell boundary data
 */
 if(cells)
  {int ncell;
   float *xb,*yb,*zb;
   int   cell_id,npts;
   ncell =cells_count(cells);
   strcpy(defname,"cell_bnd");
   if(ncell<1) goto jmpout;

   for(i=0;i<ncell;i++)
    { cell = cells_get_nth(cells,i);
      if(cell == NULL) break;
      cell_id =  cell_get_id(cell);
      sprintf(defname,"cell_bnd%-3d",i+1);
      name = cell_getname(cell);
      if(strlen(name)==0) name = defname;
      cell_get_bnd3ptr(cell,&npts, &xb,&zb,&yb);
      if(npts>3)
       {fprintf(gofil,"GOCAD PLINE 1.0\n");
        fprintf(gofil,"HEADER {\n");
        fprintf(gofil,"*name:%s\n",name);
        fprintf(gofil,"*color:%s\n",cell_getcolor(cell));
        fprintf(gofil,"}\n");
        for(j=0;j<npts-1;j++) /* last point = 1st point */
         fprintf(gofil," VRTX %4d %10.4f %10.4f %10.4f\n",
          j,xb[j],yb[j],zb[j]);
        for(j=0;j<npts-2;j++)
         fprintf(gofil," SEG %4d %4d\n",j,j+1);
        fprintf(gofil," SEG  %4d %4d\n",npts-2,0);
        fprintf(gofil,"END\n\n");
       }
      else
        fprintf(gofil,"#bad boundary-%s\n",name);
    }

  }

jmpout:
 fclose(gofil);
}

void pcardgowr_mat(ErsModel *model, char *fileo)
{FILE         *gofil;
 ErsMaterials *mats;
 ErsMaterial  *mat;
 char         *name,*cname;
 int           i,j,n;
 if(model==NULL || fileo==NULL) return;

 mats   = model_getmdata(model);
 if(!mats) return;
 if(materials_count(mats) < 1) return;

 gofil = fopen(fileo,"a+");
 if(!gofil)  return;

 if(mats)
  {int ndof,mid,N,*stype,*segid;
   float *x,*z,*y,*pv;
   for(i=0;i<materials_count(mats);i++)
    { mat = mats_get_nth(mats,i);
      n   = material_getnpts(mat);
      if(mat == NULL) break;
      material_get3(mat, &ndof, &mid, &N,
                 &stype,&segid, &x, &y, &z, &pv);
      cname = material_getcolor(mat);
      name  = material_getname(mat);
      if(N<=1) 
       fprintf(gofil,"GOCAD VSET 1.0\n");
      else
       fprintf(gofil,"GOCAD PLINE 1.0\n");
      fprintf(gofil,"HEADER {\n");
      fprintf(gofil,"*name:%s\n",name); 
      fprintf(gofil,"*color:%s\n",cname);
      fprintf(gofil,"}\n");

      fprintf(gofil,"PROPERTIES velocity material_id secondary tertiary\n");
      for(j=0;j<N;j++)
       fprintf(gofil,
        "PVRTX %4d %10.4f %10.4f %10.4f %10.4f %4d %4d %4d\n",
        j,x[j],y[j],z[j],pv[j],mid,stype[j],segid[j]);

      if(N> 1)
       {int cnt=1;
        for(j=1;j<N;j++)
         {if(stype[j] == stype[j-1] && segid[j]==segid[j-1])
           {fprintf(gofil," SEG %4d %4d\n",j-1,j);
            cnt++;
           }
          else
           {if(cnt==1) fprintf(gofil," SEG %4d %4d\n",j-1,j-1);
            if(j==N-1) fprintf(gofil," SEG %4d %4d\n",j,j);
            cnt=1;
           }
         }
       }

      fprintf(gofil,"END\n\n");
    }

  }

fclose(gofil);
}

void pcardgowr_g3dl(ErsModel *model,char *fileo)
{FILE *gofil=NULL, *glfile=NULL;
 GridLimits *glimits;
 char *mtype;
 char *dfile;
 int   wtype;
 int   i,j,ix,iy,four=4,nc;
 ErsTransform *t1,*t2,*t3;
 int   n1,n2,n3;
 float o1,o2,o3,d1,d2,d3;
 float *zdata=NULL,x,y,z;

 if(model == NULL)  return;
 mtype = model_gettype(model);
 wtype = model_getwtype(model);
/* Check that we are dealing with a G3DL file */
 if(strcmp(mtype,"G3DL") != 0) return;
 dfile = model_getdfile(model);
/* Deal only with true G3DL files */
 if(strcmp(dfile,"SAME") == 0 || strcmp(dfile,"NONE") == 0) return;

 glimits = model_getglim(model);
 glimits_get(glimits, &n1,&o1,&d1, &n2,&o2,&d2,
             &n3,&o3,&d3,&t1,&t2,&t3);
 if(n1==0 || n2==0 || n3==0) return;
/*
 * Storage order for G3DL is x,y,z 
 * Assumed order was n1=z,n2=x,n3=y
 */
 gofil = fopen(fileo,"a+");
 if(!gofil)  goto error;
 glfile = fopen(dfile,"r");
 if(!glfile)  goto error;
 zdata = (float *) calloc(1,n2*n3*sizeof(float));
 if(!zdata) goto error;

 for(i=0;i<n1;i++)
  {int num;
   num = (int) fread(zdata,four,n2*n3,glfile);
   if(wtype == WIEEE) nc = bswap_(&num,(unsigned char *) zdata);
   if(num==0) break;
   fprintf(gofil,"GOCAD VSET 1.0\n");
   fprintf(gofil,"HEADER {\n");
   fprintf(gofil,"*name:%s%-d\n","surf",i+1);
   fprintf(gofil,"*color:%s\n","red");
   fprintf(gofil,"}\n");
   for(j=0;j<num;j++)
    { iy = (int) (j/n2);
      ix = j - iy*n2;
      x = o2 + ix*d2;
      y = o3 + iy*d3;
      z = zdata[j];
      fprintf(gofil," VRTX %4d %10.4f %10.4f %10.4f\n",j,x,y,z);
    }

   fprintf(gofil,"END\n\n");
  }

 error:
 if(zdata) free(zdata);
 if(gofil) fclose(gofil);
 if(glfile) fclose(glfile);
}

void pcardgowr_grid(GridLimits *glimits, char *dfile, int  wtype,char *fileo,
     char *obj_name)
{FILE *gofil=NULL;
 int   dsize;
 ErsTransform *t1,*t2,*t3;
 int   n1=1,n2=1,n3=1;
 float o1=0.0,o2=0.0,o3=0.0;
 float d1=1.0,d2=1.0,d3=1.0;
 float b1,b2,b3;
 float *zdata=NULL;

 if(!glimits || !fileo)  return;
 dsize = 4;
 if(wtype == WCRAY) dsize = 8;
/* Deal only with true GRID files */
 if(strcmp(dfile,"SAME") == 0 || strcmp(dfile,"NONE") == 0) return;

 glimits_get(glimits, &n1,&o1,&d1, &n2,&o2,&d2,
             &n3,&o3,&d3,&t1,&t2,&t3);
 if(n1==0 || n2==0 || n3==0) return;
/*
 * Storage order for GRID is z,x,y 
 * Assumed order was n1=z,n2=x,n3=y
 */
 gofil = fopen(fileo,"a+");
 if(!gofil)  goto error;

 b1 = o1 + (n1-1)*d1;
 b2 = o2 + (n2-1)*d2;
 b3 = o3 + (n3-1)*d3;
 fprintf(gofil,"GOCAD Voxet 0.01\n");
 fprintf(gofil,"HEADER {\n");
 fprintf(gofil,"name:%s\n",obj_name);
 fprintf(gofil,"*axis:on\n");
 fprintf(gofil,"*grid:on\n");
 fprintf(gofil,"}\n");
 fprintf(gofil,"AXIS_O %f %f %f\n",o1,o2,o3);
 fprintf(gofil,"AXIS_U %f %f %f\n",0.0,0.0,d1);
 fprintf(gofil,"AXIS_V %f %f %f\n",d2,0.0,0.0);
 fprintf(gofil,"AXIS_W %f %f %f\n",0.0,d3,0.0);
 fprintf(gofil,"AXIS_N %d %d %d\n",n1,n2,n3);
 fprintf(gofil,"AXIS_D %f %f %f\n",d1,d2,d3);
 fprintf(gofil,"AXIS_MIN %f %f %f\n",o1,o2,o3);
 fprintf(gofil,"AXIS_MAX %f %f %f\n",b1,b2,b3);
 fprintf(gofil,"AXIS_NAME \"%s\" \"%s\" \"%s\"\n",
    transform_getname(t1),transform_getname(t2),transform_getname(t3));
 fprintf(gofil,"AXIS_TYPE even even even\n");
 fprintf(gofil,"\n");
 fprintf(gofil,"PROPERTY 1 \"griddata\"\n");
 fprintf(gofil,"PROP_FILE 1 %s\n",dfile);
 fprintf(gofil,"PROP_ESIZE 1 %d\n",dsize);
 if(wtype==WIEEE) fprintf(gofil,"PROP_ETYPE 1 IEEE\n");
 if(wtype==WVMS) fprintf(gofil,"PROP_ETYPE 1 VMS\n");
 if(wtype==WIBM) fprintf(gofil,"PROP_ETYPE 1 IBM\n");
 if(wtype==WCRAY) fprintf(gofil,"PROP_ETYPE 1 CRAY\n");
 fprintf(gofil,"END");
 fflush(gofil);


 error:
 if(gofil) fclose(gofil);
}
