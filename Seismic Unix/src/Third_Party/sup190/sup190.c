/* sup190 < in.su > out.su */
/* Populates su headers by matching shot and chan to database */
/* entries with UTM coordinates in shot, reciever and cdp fields */
/* stored in sup190.gdbm  */

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include <limits.h>
#include <float.h>
#include <gdbm.h>

#include "su.h"
#include "segy.h"

/************************** self documentation ********************************/
char *sdoc[] = {
"                                                                            ",
" SUP190m - Put coordinates into trace headers sx, sy, gx, gy, swdep, gelev  ",
"                                                                            ",
" sup190m < stdin > stdout [optional parameters]                             ",
"                                                                            ",
" Optional parameters:                                                       ",
"    sourcekey=fldr  name of SU trace header containing source ID            ",
"    reckey=tracf    name of SU trace header containing receiver group number",
"                                                                            ",
" Notes:                                                                     ",
"    Coordinates are retrieved from the disk database sup190.gdbm created    ",
"    with the readp190m utility.                                             ",
"                                                                            ",
"    Use suazimuth to calculate shot-receiver azimuths and offsets.          ",

NULL};
typedef
struct {
  double longitude;
  double latitude;
} coords;

static segy tr;

int main(int argc, char **argv)
{
  size_t ntrace = 0;
  int16_t coord_scale;
  int16_t coord_units;
  int32_t sx, sy, gx, gy, swdep, gelev, selev, sdepth, gdel, sdel, gwdep;
  double sxd, syd, gxd, gyd, swdepd, gelevd, selevd, sdepthd, gdeld, sdeld, gwdepd;
  double dtmp;
  GDBM_FILE coordDB;
  datum xyKey, xyVal;
#define SHOTLONG 0
#define SHOTLAT 1
#define RECLONG 2
#define RECLAT 3
#define SWDEP 4
#define GELEV 5
#define SPARE 6
  double datumvals[7]; /* shot long, lat; receiver long, lat; swdep, gelev */
  int shotpt;
  int recgrp;
  int datumpair[2]; /* shotpt, group no. */ /* may need streamer ID later */
  int verbose = 1;
  cwp_String sourcekey = "fldr";
  cwp_String sourcetype = "i";
  int sourceindex;
  Value sourceval;
  cwp_String reckey = "tracf";
  cwp_String rectype = "i";
  int recindex;
  Value recval;

  initargs(argc, argv);
  requestdoc(1);

  xyKey.dptr = (char *) (&(datumpair[0]));
  xyKey.dsize = (int) sizeof(datumpair);
  xyVal.dptr = (char *) (&(datumvals[0]));
  xyVal.dsize = (int) sizeof(datumvals);

/* Decode arguments */
  getparstring("sourcekey",&sourcekey);
  getparstring("reckey",&reckey);
  sourcetype = hdtype(sourcekey);
  rectype = hdtype(reckey);
  sourceindex = getindex(sourcekey);
  recindex = getindex(reckey);

  coordDB = gdbm_open("./sup190.gdbm",1024,GDBM_READER, 0664, NULL);
  if(coordDB == NULL) {
     fprintf(stderr,"%s: %s\n",argv[0],gdbm_strerror(gdbm_errno));
     return(EXIT_FAILURE);
  }

  /* done initializing */

  /* now process headers, copy out trace data */
  while(gettr(&tr)) {

     gethval(&tr, sourceindex, &sourceval);
     gethval(&tr, recindex, &recval);
     shotpt = vtoi(sourcetype, sourceval);
     recgrp = vtoi(rectype, recval);

     datumpair[0] = shotpt; datumpair[1] = recgrp;
     xyVal = gdbm_fetch(coordDB,xyKey);
     if(xyVal.dptr != NULL)
     if(xyVal.dsize <= sizeof(datumvals)) {
	 memcpy(datumvals,xyVal.dptr,xyVal.dsize);
	 sxd = datumvals[SHOTLONG];
	 syd = datumvals[SHOTLAT];
	 gxd = datumvals[RECLONG];
	 gyd = datumvals[RECLAT];
         swdepd = datumvals[SWDEP];
         gelevd = datumvals[GELEV];
	 coord_scale = elco_scalar(4,datumvals);
         dtmp = to_segy_elco_multiplier(coord_scale);
	 sx = (int32_t) rint(sxd*dtmp);
	 sy = (int32_t) rint(syd*dtmp);
	 gx = (int32_t) rint(gxd*dtmp);
	 gy = (int32_t) rint(gyd*dtmp);
	 coord_units = 1; /* meters or feet */
         tr.scalco = coord_scale;
         tr.sx = sx;
         tr.sy = sy;
         tr.gx = gx;
         tr.gy = gy;
         tr.counit = coord_units;

         coord_scale = tr.scalel;
         dtmp = from_segy_elco_multiplier(coord_scale);
         selevd = tr.selev*dtmp;
         sdepthd = tr.sdepth*dtmp;
         gdeld = tr.gdel*dtmp;
         sdeld = tr.sdel*dtmp;
         gwdepd = tr.gwdep*dtmp;
         datumvals[0] = swdepd; datumvals[1] = gelevd; datumvals[2] = selevd;
         datumvals[3] = sdepthd; datumvals[4] = gdeld; datumvals[5] = sdeld;
         datumvals[6] = gwdepd;
         coord_scale = elco_scalar(7,datumvals);
         dtmp = to_segy_elco_multiplier(coord_scale);
         swdep = (int32_t) rint(swdepd*dtmp);
         gelev = (int32_t) rint(gelevd*dtmp);
         selev = (int32_t) rint(selevd*dtmp);
         sdepth = (int32_t) rint(sdepthd*dtmp);
         gdel = (int32_t) rint(gdeld*dtmp);
         sdel = (int32_t) rint(sdeld*dtmp);
         gwdep = (int32_t) rint(gwdepd*dtmp);
         tr.swdep = swdep;
         tr.gelev = gelev;
         tr.selev = selev;
         tr.sdepth = sdepth;
         tr.gdel = gdel;
         tr.sdel = sdel;
         tr.gwdep = gwdep;
         tr.scalel = coord_scale;
     }
     /* write out modified trace header */
     puttr(&tr);
   
     ++ntrace;

   }

   gdbm_close(coordDB);

   return (CWP_Exit());
}
