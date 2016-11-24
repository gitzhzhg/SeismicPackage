/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUSTATICRRS: $Revision: 1.23 $ ; $Date: 2011/11/16 23:16:23 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"


/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUSTATICRRS - Elevation STATIC corrections, apply corrections from	",
"	      headers or from a source and receiver statics file,	",
"	      includes application of Residual Refraction Statics	",
"									",
"     sustaticrrs <stdin >stdout  [optional parameters]	 		",
"									",
" Required parameters:							",
"	none								",
" Optional Parameters:							",
"	v0=v1 or user-defined	or from header, weathering velocity	",
"	v1=user-defined		or from header, subweathering velocity	",
"	hdrs=0			=1 to read statics from headers		",
" 				=2 to read statics from files		",
"	sign=1			=-1 to subtract statics from traces(up shift)",
" Options when hdrs=2:							",
"	sou_file=		input file for source statics (ms) 	",
"	rec_file=		input file for receiver statics (ms) 	",
"	ns=240 			number of sources 			",
"	nr=335 			number of receivers 			",
"	no=96 			number of offsets			",
"                                                                       ",
" Options when hdrs=3:                                                  ",
"       blvl_file=              base of the near-surface model file (sampled",
"                                  at CMP locations)                    ",
"       refr_file=              horizontal reference datum file (sampled at",
"                                  CMP locations)                       ",
"       nsamp=                  number of midpoints on line             ",
"       fx=                     first x location in velocity model      ",
"       dx=                     midpoint interval                       ",
"       V_r=                    replacement velocity                    ",
"       mx=                     number of velocity model samples in     ",
"                                  lateral direction                    ",
"       mz=                     number of velocity model samples in     ",
"                                  vertical direction                   ",
"       dzv=                    velocity model depth interval           ",
"       vfile=                  near-surface velocity model             ",
"                                                                       ",
" Options when hdrs=4:                                                  ",
"       nsamp=                  number of midpoints on line             ",
"       fx=                     first x location in velocity model      ", 
"       dx=                     midpoint interval                       ", 
"                                                                       ",
" Options when hdrs=5:                                                  ",
"       none                                                            ",
"									",
" Notes:								",
" For hdrs=1, statics calculation is not performed, statics correction  ",
" is applied to the data by reading statics (in ms) from the header.	",
"									",
" For hdrs=0, field statics are calculated, and				",
" 	input field sut is assumed measured in ms.			",
" 	output field sstat = 10^scalel*(sdel - selev + sdepth)/swevel	",
" 	output field gstat = sstat - sut/1000.				",
" 	output field tstat = sstat + gstat + 10^scalel*(selev - gelev)/wevel",
"									",
" For hdrs=2, statics are surface consistently obtained from the 	",
" statics files. The geometry should be regular.			",
" The source- and receiver-statics files should be unformated C binary 	",
" floats and contain the statics (in ms) as a function of surface location.",
"									",
" For hdrs=3, residual refraction statics and average refraction statics",
" are computed.  For hdrs=4, residual refraction statics are applied,   ",
" and for hdrs=5, average refraction statics are applied (Cox, 1999).   ",
" These three options are coupled in many data processing sequences:    ",
" before stack residual and average refraction statics are computed but ",
" only residual refractions statics are applied, and after stack average",
" refraction statics are applied.  Refraction statics are often split   ",
" like this to avoid biasing stacking velocities.  The files blvl_file  ",
" and refr_file are the base of the velocity model defined in vfile and ",
" the final reference datum, as described by Cox (1999), respectively.  ",
" Residual refraction statics are stored in the header field gstat, and ",
" the average statics are stored in the header field tstat.  V_r is the ",
" replacement velocity as described by Cox (1999).  The velocity file,  ",
" vfile, is designed to work with a horizontal upper surface defined in ",
" refr_file.  If the survey has irregular topography, the horizontal    ",
" upper surface should be above the highest topographic point on the    ",
" line, and the velocity between this horizontal surface and topography ",
" should be some very large value, such as 999999999, so that the       ",
" traveltimes through that region are inconsequential.                  ",
NULL};

/* Credits:
 *	CWP: Jamie Burns
 *
 *	CWP: Modified by Mohammed Alfaraj, 11/10/1992, for reading
 *	     statics from headers and including sign (+-) option
 *
 *      CWP: Modified by Timo Tjan, 29 June 1995, to include input of
 *           source and receiver statics from files. 
 *
 *      CWP: Modified by Chris Robinson, 11/2000, to include the splitting
 *           of refraction statics into residuals and averages
 *
 * Trace header fields accessed:  ns, dt, delrt, gelev, selev,
 *	sdepth, gdel, sdel, swevel, sut, scalel
 * Trace header fields modified:  sstat, gstat, tstat
 *
 * References:
 *
 * Cox, M., 1999, Static corrections for seismic reflection surveys:
 *    Soc. Expl. Geophys.
 */

/************************ end self doc ***********************************/


segy intrace, outtrace;

int
main(int argc, char **argv)
{
	int nt;		/* number of samples on output trace	*/
	float dt;	/* sample rate on outpu trace		*/
	int itime;	/* counter          			*/
	float tmin;	/* first time sample on output trace	*/
	float tsd=0.0;	/* time to move source to datum         */
	float trd=0.0;	/* time to move 0 offset receiver       */
	float v0;	/* weathering velocity			*/
	float v1;	/* subweathering velocity		*/
	int hdrs; 	/* flag to read statics from headers	*/ 
	float *t;	/* array of output times		*/
	float tstat=0.0; /* total (source and receiver) statics	*/
	int sign;	/* to add (+) or subtract (-) statics	*/
	int no;		/* number of offsets per shot 		*/
	int io;		/* offset counter 			*/
	int is;		/* source counter 			*/
	int ir;		/* receiver counter 			*/
	int ns;		/* number of sources = number of source statics */
	int nr;		/* number of receiver = number of rec. statics	*/
        int nsamp;      /* number of samples for each surface           */
        int i,j,res=0,bulk=0,mx,mz;
        float fx;      /* first sample position of surfaces            */
        float dx;
        int ic_c,is_c,ir_c;
        float z_blvl_s,z_refr_s;
        float z_blvl_r,z_refr_r;
	float *sou_statics=NULL;	/* array of source statics	*/
	float *rec_statics=NULL;	/* array of receiver statics	*/
        float *blvl_arr=NULL;           /* array of base LVL values     */
        float *refr_arr=NULL;           /* array of reference datum values */
        float *tcdp_stat=NULL, *tcdp_arr=NULL;
        float *num=NULL, *num_arr=NULL, **vel=NULL;
        float dzv;
        float V_r;
	FILE *fps, *fpr;       	/* file pointers for statics input 	*/
        FILE *fpb=NULL, *fpf=NULL;   /* file pointers for input surfaces     */
        FILE *g1=NULL, *g2=NULL, *g3=NULL, *g4=NULL;
        FILE *vfp;
	cwp_String sou_file, rec_file; /* statics filenames 		*/
        cwp_String blvl_file; /* surface filenames                      */
        cwp_String refr_file;            /*surface filename             */
        cwp_String vfile;

	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* Get information from first trace */
	if (!gettr(&intrace)) err("can't get first trace");
	nt   = intrace.ns;
	tmin = intrace.delrt/1000.0;
	dt   = ((double) intrace.dt)/1000000.0;
	
	/* Get parameters */
	if (!getparfloat("v1", &v1))          v1 = (float) intrace.swevel;
	if (!getparfloat("v0", &v0))
                v0 = (float) ((intrace.wevel) ? intrace.wevel : v1);
	if (!getparint("hdrs", &hdrs))        hdrs = 0;
	if (!getparint("sign", &sign))        sign = 1;

	/* Allocate vector of output times */
	t = ealloc1float(nt);

	/* reading source and receiver statics from files */
	if (hdrs == 2){

		/* getpar statics file related parameters */
		if (!getparint("ns", &ns))        ns = 240;
		if (!getparint("nr", &nr))        nr = 335;
		if (!getparint("no", &no))        no = 96;

		/* getpar statics file names */
        	getparstring("sou_file",&sou_file);
        	getparstring("rec_file",&rec_file);

		/* allocate space */
		rec_statics = alloc1float(nr);
        	sou_statics = alloc1float(ns);

		/* open and read from receiver statics file */
        	if((fpr=efopen(rec_file,"rb"))==NULL)
                	err("cannot open stat_file=%s\n",rec_file);
        	efread(rec_statics, sizeof(float),nr,fpr);
        	efclose(fpr);

		/* open and read from source statics file */
        	if((fps=efopen(sou_file,"rb"))==NULL)
                	err("cannot open stat_file=%s\n",sou_file);
        	efread(sou_statics, sizeof(float),ns,fps);
        	efclose(fps);
	}

        if (hdrs==3){
                getparstring("blvl_file",&blvl_file);
                getparstring("refr_file",&refr_file);
                getparint("nsamp",&nsamp);
                getparfloat("fx",&fx);
                getparfloat("dx",&dx);
                getparfloat("V_r",&V_r);

                blvl_arr = alloc1float(nsamp);
                if((fpb=efopen(blvl_file,"rb"))==NULL)
                        err("cannot open blvl_file=%s\n",blvl_file);
                efread(blvl_arr, sizeof(float),nsamp,fpb);

                refr_arr = alloc1float(nsamp);
                if((fpf=efopen(refr_file,"rb"))==NULL)
                        err("cannot open refr_file=%s\n",refr_file); 
                efread(refr_arr, sizeof(float),nsamp,fpf);

                g1=fopen("tcdp.b","wb");
                g2=fopen("num.b","wb");

                if (!getparstring("vfile",&vfile)) err("must specify vfile");
                if (!getparint("mx",&mx)) err("must specify mx");
                if (!getparint("mz",&mz)) err("must specify mz");
                if (!getparfloat("dzv",&dzv)) err("must specify dzv");
                vfp = fopen(vfile,"r");
                vel = alloc2float(mz,mx);
                fread(vel[0],sizeof(float),mz*mx,vfp);
                fclose(vfp);

                tcdp_stat = alloc1float(nsamp);
                num = alloc1float(nsamp);
                for(i=0;i<nsamp;i++){
                      tcdp_stat[i] = 0.0;
                      num[i] = 0.0;
                }
        }

        if (hdrs==4){
                getparint("nsamp",&nsamp);
                getparfloat("fx",&fx);
                getparfloat("dx",&dx); 
                tcdp_arr = alloc1float(nsamp);
                if((g3=efopen("tcdp.b","rb"))==NULL)
                        err("cannot open tcdp.b\n");
                efread(tcdp_arr, sizeof(float),nsamp,g3);

                num_arr = alloc1float(nsamp);
                if((g4=efopen("num.b","rb"))==NULL)
                        err("cannot open num.b\n");
                efread(num_arr, sizeof(float),nsamp,g4);

        }
        checkpars();

	/* Loop on traces */	
	io = 0; is = 0;
	do {
		int temp = SGN(intrace.scalel)*log10(abs((int) intrace.scalel));
		float scale;
                scale = pow(10., (float)temp);
		
		/* copy and adjust header */
		memcpy( (void *) &outtrace, (const void *) &intrace, HDRBYTES);
	
		/* compute static correction if necessary */
		if(!hdrs) {
		    	tsd = scale *
			(-intrace.selev + intrace.sdel + intrace.sdepth)/v1;
			trd = tsd - intrace.sut/1000.0;
			tstat = tsd + trd +
				scale * (intrace.selev - intrace.gelev)/v0;

		/* else, read statics from headers */
		} else { 

			if (hdrs==2) {
				ir = is + io;
				if (is < ns) tsd = sou_statics[is]/1000.0;
				if (ir >= 0 && ir < nr)
					trd = rec_statics[ir]/1000.0;

				intrace.tstat =  NINT(tsd*1000.0 +  trd*1000.0);
				io ++;
				if (io > no-1) {
					io = 0; is++;
				}
			}

                        if(hdrs==3){
                              is_c = (int)((((float)intrace.sx)-fx)/dx);
                              z_blvl_s = blvl_arr[is_c];
                              z_refr_s = refr_arr[is_c];
                              
                              ir_c = (int)((((float)intrace.gx)-fx)/dx);
                              z_blvl_r = blvl_arr[ir_c];
                              z_refr_r = refr_arr[ir_c];

                              tsd = 0.0;
                              trd = 0.0;
                              is_c = (int)((((float)intrace.sx)-fx)/dx);
                              ir_c = (int)((((float)intrace.gx)-fx)/dx);
                              for(j=0;j<mz-1;j++){
                                 tsd = tsd + dzv/vel[is_c][j];
                                 trd = trd + dzv/vel[ir_c][j];
                              }
                              tsd = tsd - (z_blvl_s-z_refr_s)/V_r;
                              trd = trd - (z_blvl_r-z_refr_r)/V_r;

                              intrace.tstat = NINT(tsd*1000.0+trd*1000.0);

                              ic_c = (int)((((float)intrace.cdp)-fx)/dx);
                              tcdp_stat[ic_c] = tcdp_stat[ic_c] + tsd + trd;
                              num[ic_c] = num[ic_c] + 1.0;
                        }

                        if(hdrs==4){
                              ic_c = (int)((((float)intrace.cdp)-fx)/dx);
                              if(num_arr[ic_c]>=0.5){
                                 bulk = NINT(1000.0*tcdp_arr[ic_c]/num_arr[ic_c]);
                                 res = intrace.tstat - bulk;
                              }
                              if(num_arr[ic_c]<=0.5){
                                 bulk = 0;
                                 res = 0;
                              }
                        }


			/* if total statics not supplied, calculate it */
			if(intrace.tstat==0 && hdrs!=3 && hdrs!=4){
				outtrace.tstat = intrace.sstat+intrace.gstat;
				tstat = outtrace.tstat/1000.0;
			} 
                        /* for hdrs==5, here is where tstat is calculated */
                        if(intrace.tstat!=0 && hdrs!=3 && hdrs!=4){
				tstat = intrace.tstat/1000.0;
			}
                        if(hdrs==3){
                                tstat = 0.0;
                        }
                        if(hdrs==4){
                                tstat = res/1000.0;
                        }
		}
		
		/* Compute output times */
		for (itime=0; itime<nt; ++itime)
			t[itime] = tmin + itime*dt + sign*tstat;

		/* sinc interpolate new data */
		ints8r(nt, dt, tmin, intrace.data, 
				0.0, 0.0, nt, t, outtrace.data);
		
		/* set header field for output trace */
		if(hdrs!=1 && hdrs!=3 && hdrs!=4 && hdrs!=5){

			/* value is added to existing header values */
			/* this permits multiple static corrections */
			outtrace.sstat += (1000.0 * tsd);
			outtrace.gstat += (1000.0 * trd);
			outtrace.tstat += (1000.0 * tstat);
		}
                if(hdrs==3){
                        outtrace.sstat = 0;
                        outtrace.gstat = 0;
                        outtrace.tstat = intrace.tstat;
                }
                if(hdrs==4){
                        outtrace.sstat = 0;
                        outtrace.gstat = res;
                        outtrace.tstat = bulk;
                }
                if(hdrs==5){
                        outtrace.tstat = intrace.tstat;
                        outtrace.sstat = intrace.sstat;
                        outtrace.gstat = intrace.gstat;
                }
		
		puttr(&outtrace);
	} while (gettr(&intrace));

        if(hdrs==3){
           fwrite(tcdp_stat,sizeof(float),nsamp,g1);
           fwrite(num,sizeof(float),nsamp,g2);
           fclose(fpb);
           fclose(fpf);
           fclose(g1);
           fclose(g2);
        }
        if(hdrs==4){
           fclose(g3);
           fclose(g4);
        }

	return(CWP_Exit());
}
