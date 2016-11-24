/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "posterioriMain.h"
/*********************** self documentation **********************/
char *sdoc[] = {
"                                                                           ",
"  posteriori.c                                                             ",
"                                                                           ",
"  This program computes the a-posteriori model covariance matrix resulting ",
"  from a Bayesian calculation which assumes Gaussian uncertainties and     ",
"  resorts to a linearization of the forward modeling operator about the    ",
"  MAP (maximum a posteriori) model.                                        ",
"  This matrix is given by (See Tarantola's Inverse Theory book, page 76,   ",
"  equation 1.109):                                                         ",
"                  CM' = [G^T CD G + CM^-1]^-1,                             ",
"  where:                                                                   ",
"  G:  Linearized forward modeling operator about MAP model. Here the       ",
"      elastic reflectivity method is used as the forward modeling          ",
"      operator                                                             ",
"  CD: Data covariance matrix                                               ",
"  CM: Model covariance matrix                                              ",
"  The input parameters for this code are as follows.                       ",
"                                                                           ",
"  model=[model] MAP elastic model file. This is the initial model used in  ",
"                optimization. This is an ASCII file that should contain    ",
"                one row per layer of the model in the following format:    ",
"                                                                           ",
"                Thick(km)  Rho(g/cm^3)  Vp(km/s)  Qp  Vs(km/s)  Qs         ",
"                Where:                                                     ",
"                Thick: Layer thickness                                     ",
"                Rho:   Layer density                                       ",
"                Vp:    P-wave velocity                                     ",
"                Qp:    P-wave quality factor                               ",
"                Vs:    S-wave velocity                                     ",
"                Qs:    S-wave quality factor                               ",
"  corrData=[corrdata]                                                      ",
"                Inverse of data covariance matrix.                         ",
"  impedance=[0] If set to 1, calculation is carried out in the impedance-  ",
"                -density domain. Otherwise it is carried out in the        ",
"                velocity-density domain.                                   ",
"  p=[1]         P-wave (velocity or impedance) active.                     ",
"  s=[1]         S-wave (velocity or impedance) active.                     ",
"  r=[1]         Density active.                                            ",
"  prior=[0]     If set to 1 prior information required.                    ",
"  corrP=[covP]  Inverse of the P-wave model covariance matrix.             ",
"  corrS=[covS]  Inverse of the S-wave model covariance matrix.             ",
"  corrR=[covR]  Inverse of the density model covariance matrix.            ",
"  targetbeg=[.5]Begin of the target depth interval (km).                   ",
"  targetend=[1] End of the target depth interval (km).                     ",
"  dz=0.5        Depth discretization level in the target zone (km).        ",
"  t1=[0]        Begining of the time misfit window (s).                    ",
"  t2=[1]        End of the time misfit window (s).                         ",
"  recfile=[NULL]File with receiver offsets. The format is one offset       ",
"                line. If none is specified (default), the acquisition      ",
"                is end on.                                                 ",
"  directwave=1  Direct wave flag. If zero direct wave is not generated     ",
"  r1=[0]        Minimum source-receiver offset (km)                        ",
"  nr=[48]       Number of receivers                                        ",
"  dr=[0.025]    Receiver spacing (km)                                      ",
"  zs=[0.001]    Source depth (km)                                          ",
"  u1=[0]        Initial horizontal slowness (s/km)                         ",
"  u2=[1]        Final horizontal slowness (s/km)                           ",
"  nu=[100]      Number of slownesses                                       ",
"  f1=[2]        Initial frequency for modeling (Hz)                        ",
"  f2=[50]       Final frequency for modeling (Hz)                          ",
"  dt=[0.004]    Time sampling interval (0.004)                             ",
"  tmod=[8]      Maximum modeling time (s)                                  ",
"  F1=[0]        EAST-WEST point force coordinate                           ",
"  F2=[0]        NORTH-SOUTH point force coordinate                         ",
"  F3=[1]        VERTICAL point force coordinate                            ",
"  hanning=[1]   Final trace is convolved with a hanning window             ",
"  wu=[5]        Percentual of slowness interval that is windowed           ",
"  ww=[5]        Percentual of frequency interval that is windowed          ",
"  tau=[50]      Wrap-around attenuation parameter                          ",
"  verbose=[0]   =1 print useful information                                ",
"                                                                           ",
"  Output data:                                                             ",
"  postfile=[posteriori]                                                    ",
"                A posteriori matrix binary file. The dimension of this     ",
"                matrix is the number of parameters being inverted for.     ",
"  frechetfile=[frechet]                                                    ",
"                If specified the programs generates a binary file with the ",
"                Frechet derivatives. The number of traces in this file is  ",
"                the number of traces being modeled times the number of     ",
"                parameters being inverted for. The length of each trace is ",
"                the misfit window.                                         ",
"                                                                           ",
"                                 Author: Wences Gouveia - 96-08-01         ",
"                                 Center For Wave Phenomena                 ",
"                                 Colorado School of Mines                  ",
NULL};   
/************************ end self doc ***********************************/ 
  void main (int argc, char **argv)
{
   /* declaration of variables */
   FILE *fp, *gp;                /* file pointers */
   char *orientation = " ";      /* orientation of recordings */
   char *recFile = " ";          /* receiver location file */  
   char *postFile = " ";         /* posteriori file */
   char *modelFile = " ";        /* elastic model file */
   char *corrDataFile = " ";     /* data covariance file */
   char *corrModelFile[3];       /* model covariance file */
   char *frechetFile = " ";      /* frechet derivative file */
   int verbose;                  /* verbose flag */
   int noFrechet;                /* if 1 don't store Frechet derivatives */
   int i, j, k, iU, iParam, offset, iR, shift;
                                 /* counters */
   int wL;                       /* taper length */
   int nParam;                   /* number of parameters altogether */
   int numberParImp;             /* number of distinct parameters in */
                                 /* impedance inversion */
   float dZ;                     /* layer thickness within target zone */
   float F1, F2, F3;             /* source components */
   float depth;                  /* current depth used in defining limits */
                                 /* for Frechet derivatives */
   float fR;                     /* reference frequency */
   float percU;                  /* amount of slowness windowing */
   float percW;                  /* amount of frequency windowing */
   float limZ[2];                /* target interval (Km) */
   float tMod;                   /* maximum modeling time */
   float phi;                    /* azimuth angle */
   float *buffer1, *buffer2;     /* auxiliary buffers */
   float **CmPost;               /* posteriori model covariance */
   float **CmPostInv;            /* posteriori model covariance - inverse */

   /* allocing for orientation */
   orientation = malloc(1);
   
   /* complex Zero */
   zeroC = cmplx(0, 0);

   /* getting input parameters */
   initargs(argc, argv);
   requestdoc(0);

   /* seismic data and model parameters */
   if (!getparstring("model", &modelFile)) modelFile = "model";
   if (!getparstring("postfile", &postFile)) postFile = "posteriori";
   if (!getparstring("corrData", &corrDataFile)) corrDataFile = "corrdata";
   
   if (!getparint("impedance", &IMPEDANCE)) IMPEDANCE = 0;
   if (!getparstring("frechetfile", &frechetFile)) noFrechet = 0;
   else noFrechet = 1;
   if (!getparint("prior", &PRIOR)) PRIOR = 1;
   if (IMPEDANCE)
   {
     if (!getparint("p", &ipFrechet)) vpFrechet = 1;
     if (!getparint("s", &isFrechet)) vsFrechet = 1;
     if (!getparint("r", &rhoFrechet)) rhoFrechet = 1;
   }
   else
   {
     if (!getparint("p", &vpFrechet)) vpFrechet = 1;
     if (!getparint("s", &vsFrechet)) vsFrechet = 1;
     if (!getparint("rho", &rhoFrechet)) rhoFrechet = 1;
   }
   
   /* a couple of things to use later in chain rule */
   if (!IMPEDANCE)
   {
      ipFrechet = 0;      isFrechet = 0;
   }
   else
   {
      if (ipFrechet && !isFrechet)
      {
	 vpFrechet = 1;	  vsFrechet = 0;
      }
      if (!ipFrechet && isFrechet)
      {
	 vpFrechet = 0;	  vsFrechet = 1;
      }
      if (!ipFrechet && !isFrechet)
      {
	 vpFrechet = 0;	  vsFrechet = 0;
      }
      if (ipFrechet && isFrechet)
      {
	 vpFrechet = 1;	  vsFrechet = 1;
      }
      if (rhoFrechet)
      {
	 vpFrechet = 1;	  vsFrechet = 1;   rhoFrechet = 1;
      }
   }
      
   if (!ipFrechet && ! isFrechet && !rhoFrechet && !vpFrechet && !vsFrechet)
      err("No inverse unknowns to work with!\n");

   numberPar = vpFrechet + vsFrechet + rhoFrechet;
   numberParImp = ipFrechet + isFrechet + rhoFrechet;

   if (PRIOR)
   {
      if (vpFrechet || ipFrechet)
      {
	 if (!getparstring("corrP", &corrModelFile[0])) 
	    corrModelFile[0] = "covP";
      }
      if (vsFrechet || isFrechet) 
      {
	 if (!getparstring("corrS", &corrModelFile[1])) 
	    corrModelFile[1] = "covS";
      }
      
      if (rhoFrechet) 
      {
	 if (!getparstring("corrR", &corrModelFile[2])) 
	    corrModelFile[2] = "covR";
      }
   }
   
   if (!getparstring("orientation", &orientation)) orientation[0] = 'Z';
   if (orientation[0] == 'z' || orientation[0] == 'Z')
   {
      VERTICAL = 1; RADIAL = 0;
   }
   else
   {
      VERTICAL = 0; RADIAL = 1;
   }
   
   if (!getparfloat("dz", &dZ)) dZ = .5;
   if (!getparfloat("targetbeg", &limZ[0])) limZ[0] = 0.5; 
   if (!getparfloat("targetend", &limZ[1])) limZ[1] = 1.0;

   /* geometry */
   if (!getparfloat("r1", &r1)) r1 = 0.25;
   if (!getparint("nr", &nR)) nR = 48;
   if (!getparfloat("dr", &dR)) dR = .025;
   if (!getparfloat("zs", &zs)) zs = .001;
   if (!getparfloat("F1", &F1)) F1 = 0;
   if (!getparfloat("F2", &F2)) F2 = 0;
   if (!getparfloat("F3", &F3)) F3 = 1;

   /* modeling */
   if (!getparstring("receiverfile", &recFile)) recFile = " ";
   if (!getparfloat("u1", &u1)) u1 = 0.0;
   if (!getparfloat("u2", &u2)) u2 = 1.;
   if (!getparint("directwave", &directWave)) directWave = 1;
   if (!getparfloat("tau", &tau)) err("Specify tau!\n");
   if (!getparint("nu", &nU)) nU = 1000;
   if (!getparfloat("f1", &f1)) f1 = 2;
   if (!getparfloat("f2", &f2)) f2 = 50;
   if (!getparfloat("dt", &dt)) dt = 0.004;
   if (!getparfloat("tmod", &tMod)) tMod = 8;
   if (!getparfloat("t1", &t1)) t1 = 0;
   if (!getparfloat("t2", &t2)) t2 = tMod;
   if (!getparint("hanning", &hanningFlag)) hanningFlag = 1;
   if (!getparfloat("wu", &percU)) percU = 10; percU /= 100;
   if (!getparfloat("ww", &percW)) percW = 25; percW /= 100;

   /* dialogue */
   if (!getparint("verbose", &verbose)) verbose = 0;

   /* checking number of receivers */
   fp = fopen(recFile, "r");
   if (fp != NULL)
   {
      nR = 0;
      while (fscanf(fp, "%f\n", &auxm1) != EOF) nR++;
   }
   fclose(fp);

   /* some hard-coded parameters */
   fR = 1; wR = 2 * PI * fR;         /* reference frequency */
   
   /* how many layers */
   fp = fopen(modelFile,"r");
   if (fp == NULL)
      err("No model file!\n");
   
   nL = 0;
   depth = 0;
   while (fscanf(fp, "%f %f %f %f %f %f\n", 
		 &aux, &aux, &aux, &aux, &aux, &aux) != EOF)
      nL++;
   nL--;

   /* considering the unknown layers */
   limRange = NINT((limZ[1] - limZ[0]) / dZ);

   if (verbose)
   {
      fprintf(stderr,"Number of layers: %d\n", nL + 1);
      fprintf(stderr,"Number of layers in target zone: %d\n", limRange);
   }


   if (IMPEDANCE)
   {
      nParam = numberParImp * limRange;
   }
   else
   {
      nParam = numberPar * limRange;
   }

   /* basic time-frequency stuff */
   nSamples = NINT(tMod / dt) + 1;
   nSamples = npfar(nSamples);

   /* length of time misfit */
   nDM = NINT((t2 - t1) / dt) + 1;

   /* maximum time for modeling */
   tMod = dt * (nSamples - 1);
   dF = 1. / (tMod);

   /* adjusting f1 and f2 */
   aux = dF;
   while (aux < f1)
      aux += dF;
   f1 = aux;
   while (aux < f2)
      aux += dF;
   f2 = aux;
   
   nF = NINT((f2 - f1) / dF); 
   if (nF%2 == 0) 
   {
      f2 += dF;
      nF++;
   }

   /* memory allocation */
   alpha = alloc1float(nL + 1);
   beta = alloc1float(nL + 1);
   rho = alloc1float(nL + 1);
   qP = alloc1float(nL + 1);
   qS = alloc1float(nL + 1);
   thick = alloc1float(nL + 1);
   recArray = alloc1float(nR);

   PSlowness = alloc2complex(2, nL + 1);
   SSlowness = alloc2complex(2, nL + 1);
   S2Velocity = alloc2complex(2, nL + 1);

   CD = alloc1float(nDM * (nDM + 1) / 2);
   if (PRIOR)
   {
      if(vpFrechet || ipFrechet)
	 CMP = alloc1float(limRange * (limRange + 1) / 2);
      if(vsFrechet || isFrechet)
	 CMS = alloc1float(limRange * (limRange + 1) / 2);
      if(rhoFrechet)
	 CMrho = alloc1float(limRange * (limRange + 1) / 2);
   }
   
   /* FRECHET derivative operator F */
   F = alloc2float(nR * nDM, numberPar * limRange);

   if (IMPEDANCE)
      CmPostInv = 
	 alloc2float(numberParImp * limRange, numberParImp * limRange);
   else
      CmPostInv = alloc2float(numberPar * limRange, numberPar * limRange);

   v1 = alloc2complex(2, numberPar * limRange + 1);
   v2 = alloc2complex(2, numberPar * limRange + 1);
   DmB = alloc3complex(4, numberPar * (limRange + 2), nL);
   derFactor = alloc2complex(2, nL + 1);
   aux11 = alloc2complex(nR, numberPar * limRange);
   aux12 = alloc2complex(nR, numberPar * limRange);
   aux21 = alloc2complex(nR, numberPar * limRange);
   aux22 = alloc2complex(nR, numberPar * limRange);
   aux11Old = alloc2complex(nR, numberPar * limRange);
   aux12Old = alloc2complex(nR, numberPar * limRange);
   aux21Old = alloc2complex(nR, numberPar * limRange);
   aux22Old = alloc2complex(nR, numberPar * limRange);

   /* reading receiver configuration */
   fp = fopen(recFile, "r");
   if (fp == NULL)
   {
      /* standard end-on */
      if (verbose) fprintf(stderr, "No receiver file available\n");
      for (i = 0; i < nR; i++)
      {
         recArray[i] = r1 + i * dR;
      }
   }
   else   
   {
      if (verbose) fprintf(stderr, "Reading receiver file %s\n", recFile);
      for (i = 0; i < nR; i++)
      {
         fscanf(fp, "%f\n", &recArray[i]);
      }
   }
   fclose(fp);
   
   /* reading the model file */
   fp = fopen(modelFile,"r");
   if (verbose)      
     fprintf(stderr,"  Thickness     rho     vP     qP    vS     qS\n");
   for (k = 0; k < nL + 1; k++)
   {
      fscanf(fp, "%f %f %f %f %f %f\n", &thick[k], &rho[k], &alpha[k], 
	     &qP[k], &beta[k], &qS[k]);
      if (verbose)
	fprintf(stderr,"   %7.4f      %4.3f   %3.2f  %5.1f  %3.2f  %5.1f\n",
		thick[k], rho[k], alpha[k], qP[k], beta[k], qS[k]);
   }
   fclose(fp);

   /* setting lim[0] and lim[1] */
   for (depth = thick[0], i = 1; i <= nL; depth += thick[i], i++)
   {
      if (NINT(depth / dZ) <= NINT(limZ[0] / dZ)) lim[0] = i;
      if (NINT(depth / dZ) < NINT(limZ[1] / dZ)) lim[1] = i;
   }
   lim[1]++;

   /* some modeling parameters */
   /* slowness increment */
   dU = (u2 - u1) / (float) nU;

   /* computing the window length for the slowness domain */
   epslon1 = (u2 - u1) * percU;
   wL = NINT(epslon1 / dU);
   wL = 2 * wL + 1;
   u2 += epslon1;
   nU = NINT((u2 - u1) / dU);    /* new nU to preserve last slowness */
                                 /* w/o being windowed */
   taper = alloc1float(nU);

   /* building window for slowness integration */
   for (i = (wL - 1) / 2, iU = 0; iU < nU; iU++)
   {
      taper[iU] = 1;
      if (iU >= nU - (wL - 1) / 2)
      {
         i++;
	 taper[iU] =
	    .42 - .5 * cos(2 * PI * (float) i / ((float) (wL - 1))) +
            .08 * cos(4 * PI * (float) i / ((float) (wL - 1)));
      }
   }

   /* filtering in frequency domain */
   filter(percW);
   
   /* building frequency filtering */
   /* I will assume that the receivers are in line (at z = 0) so phi = 0 */
   phi = 0;
   epslon1 = F3;
   epslon2 = F1 * cos(phi) + F2 * sin(phi);

   /* correction for the 1st layer */
   thick[0] -= zs;

   /* imaginary part of frequency for damping wrap-around */
   tau = log(tau) / tMod;
   if (tau > TAUMAX)
      tau = TAUMAX;

   /* normalization for the complex slowness */
   if (f1 > 7.5)
      wRef = f1 * 2 * PI;
   else
      wRef = 7.5 * 2 * PI;

   /* reading data and model covariance matrixes */
   inputCovar(corrDataFile, corrModelFile);
   
   /* starting inverse procedure */
   /* FRECHET derivative matrix  */
      gradient();
   
   if (!noFrechet)
   {
      fp = fopen(frechetFile, "w");
      for (i = 0; i < numberPar * limRange; i++)
      {
	 fwrite(&F[i][0], sizeof(float), nR * nDM, fp);
      }
      fclose(fp);
   }

   /* building a-posteriori model covariance matrix */
   /* prior information is used */
   buffer1 = alloc1float(nDM);
   buffer2 = alloc1float(nDM * nR);

   if (verbose) fprintf(stderr, "Building posteriori covariance...\n");

   for (iParam = 0; iParam < nParam; iParam++)
   {
      for (i = 0; i < nDM; i++)
      {
	 for (offset = i, k = 0; k < nDM; k++)
	 {
	    buffer1[k] = CD[offset];
	    offset += MAX(SGN0(i - k) * (nDM - 1 - k), 1);
	 }

	 /* doing the product CD F */
  	 for (iR = 0; iR < nR; iR++)
	 {
	    buffer2[iR * nDM + i] = 0;
	    for (k = 0; k < nDM; k++)  
	    {
	       buffer2[iR * nDM + i] += buffer1[k] * 
		                        F[iParam][iR * nDM + k];
	    }
 	 }
      }
      
      for (j = 0; j < nParam; j++)
      {
	 CmPostInv[j][iParam] = 0;
	 for (k = 0; k < nDM * nR; k++)
	 {
	    CmPostInv[j][iParam] += buffer2[k] * F[j][k];
	 }
      }
   }

   if (verbose) 
     fprintf(stderr, "Posteriori covariance built. Including prior...\n");

   free1float(buffer1);
   buffer1 = alloc1float(nParam);
   /* including prior covariance matrix */
   if (PRIOR)
   {
       shift = 0;
      if (IMPEDANCE)
      {
	 if (ipFrechet)
	 {
	    for (iParam = 0; iParam < limRange; iParam++)
	    {
	       for (offset = iParam, k = 0; k < limRange; k++)
	       {
		  buffer1[k] = CMP[offset];
		  offset += MAX(SGN0(iParam - k) * (limRange - 1 - k), 1);
	       }
	       
	       for (k = 0; k < limRange; k++) 
	       {
		  CmPostInv[iParam][k] += buffer1[k];
	       }
	    }
            shift += limRange;
	 }
      }
      else
      {
	 if (vpFrechet)
	 {
	    for (iParam = 0; iParam < limRange; iParam++)
	    {
	       for (offset = iParam, k = 0; k < limRange; k++)
	       {
		  buffer1[k] = CMP[offset];
		  offset += MAX(SGN0(iParam - k) * (limRange - 1 - k), 1);
	       }
	       
	       for (k = 0; k < limRange; k++) 
	       {
		  CmPostInv[iParam][k] += buffer1[k];
	       }
	    }
            shift += limRange;
	 }
      }

      if (IMPEDANCE)
      {
	 if (isFrechet)
	 {
	    for (iParam = 0; iParam < limRange; iParam++)
	    {
	       for (offset = iParam, k = 0; k < limRange; k++)
	       {
		  buffer1[k] = CMS[offset];
		  offset += MAX(SGN0(iParam - k) * (limRange - 1 - k), 1);
	       }
	       
	       for (k = 0; k < limRange; k++)
	       {
		  CmPostInv[iParam + shift][k + shift] += buffer1[k];
	       }
	    }
            shift += limRange;
	 }
      }
      else
      {
	 if (vsFrechet)
	 {
	    for (iParam = 0; iParam < limRange; iParam++)
	    {
	       for (offset = iParam, k = 0; k < limRange; k++)
	       {
		  buffer1[k] = CMS[offset];
		  offset += MAX(SGN0(iParam - k) * (limRange - 1 - k), 1);
	       }
	       
	       for (k = 0; k < limRange; k++)
	       {
		  CmPostInv[iParam + shift][k + shift] += buffer1[k];
	       }
	    }
            shift += limRange;
	 }
      }

      if (rhoFrechet)
      {
	 for (iParam = 0; iParam < limRange; iParam++)
	 {
	    for (offset = iParam, k = 0; k < limRange; k++)
	    {
	       buffer1[k] = CMrho[offset];
	       offset += MAX(SGN0(iParam - k) * (limRange - 1 - k), 1);
	    }
	    
	    for (k = 0; k < limRange; k++) 
	    {
	       CmPostInv[iParam + shift][k + shift] += buffer1[k];
	    }
	 }
      }
   }

   if (verbose) fprintf(stderr, "Prior included. Inverting matrix...\n");

   /* freeing memory */
   free1float(buffer1);
   free1float(buffer2);
   free1float(alpha);
   free1float(beta);
   free1float(rho);
   free1float(qP);
   free1float(qS);
   free1float(thick);
   free2complex(PSlowness);
   free2complex(SSlowness);
   free2complex(S2Velocity);
   free1float(CD);
   free1float(CMP);
   free1float(CMS);
   free1float(CMrho);
   free2float(F);
   free2complex(v1);
   free2complex(v2);
   free3complex(DmB);
   free2complex(derFactor); 
   free2complex(aux11);
   free2complex(aux12);
   free2complex(aux21);
   free2complex(aux22); 
   free2complex(aux11Old);
   free2complex(aux12Old);
   free2complex(aux21Old);
   free2complex(aux22Old); 

   /* inverting the matrix */
   CmPost = alloc2float(nParam, nParam);
   for (i = 0; i < nParam; i++) for (j = 0; j < nParam; j++)
      CmPostInv[i][j] = CmPost[i][j];
   inverse_matrix(nParam, CmPostInv);

   if (verbose) fprintf(stderr, "Done with inverse matrix routine.\n");
   
   buffer1 = alloc1float(nParam);
   gp = fopen(postFile, "w");
   for (i = 0; i < nParam; i++)
   {
      fwrite(CmPostInv[i], sizeof(float), nParam, gp);
   }
   fclose(fp);
}
