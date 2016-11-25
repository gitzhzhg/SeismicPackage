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
/*------------------------------------------------------------------
 *USER DOC
 *Name   : definedColor
 *Purpose: Load in a pre-defined color scheme                      
 *
 *Author :  Michael L. Sherrill, Hauge
 *Date   : 02/92 (C++ version 4/97)
 *
 * Function Definition:
 * void definedColor(float       *cbar,
 *                    long         barnumber,
 *                    long         *numcolors,
 *                    long         *use_amplitudes,
 *                    long         maxcolors)
 *
 * cbar      in         Float array to hold rgb values.
 * barnumber in         ID of color scheme.            
 * numcolors out        Number of colors returned.   
 * use_amplitudes out   Flag to indicate amplitude values are in rgb scheme.
 * maxcolors in         Fit gray scale into this number allowed. 
 *
 *NOTES:
 *
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *END DOC
 *------------------------------------------------------------------*/


#include "plot_image.hh"

void PlotImage::definedColor(float       *cbar, 
                             long         barnumber,
                             long         *numcolors,
                             long         *use_amplitudes,
                             long         maxcolors)


{
 long i = 0;
 long j, k;
 // long integerval;
 float byte_ratio;
 float rgb_ratio;
 float amplitude = 0.0;
 float x0, x1, xa, xin ;


 //For now we are going limit the number of gray colors to 32.
 //If we allow it to do more and the user mixes a multi-file color
 //plots with 32 colors the mapping gets messed up since they share
 //the same color map. Note this is a problem with any color bar
 //that has more than 32 colors and we are doing the multi-file option.
 maxcolors = 32;

/****************************standard 32 color bar*************************/
 if(barnumber == STANDARD)
 {     /*red*/      /*green*/    /*blue*/   /*trace amplitude if used*/
  cbar[i]=  0.000; cbar[++i]=0.000; cbar[++i]=0.250; cbar[++i]=0.000;
  cbar[++i]=0.000; cbar[++i]=0.246; cbar[++i]=0.479; cbar[++i]=0.000;
  cbar[++i]=0.000; cbar[++i]=0.310; cbar[++i]=0.560; cbar[++i]=0.000;
  cbar[++i]=0.000; cbar[++i]=0.497; cbar[++i]=0.749; cbar[++i]=0.000;
  cbar[++i]=0.140; cbar[++i]=0.640; cbar[++i]=0.860; cbar[++i]=0.000;
  cbar[++i]=0.400; cbar[++i]=0.740; cbar[++i]=1.000; cbar[++i]=0.000;
  cbar[++i]=0.497; cbar[++i]=0.870; cbar[++i]=1.000; cbar[++i]=0.000;
  cbar[++i]=0.770; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.000;
  cbar[++i]=0.319; cbar[++i]=0.740; cbar[++i]=0.630; cbar[++i]=0.000;
  cbar[++i]=0.310; cbar[++i]=0.550; cbar[++i]=0.500; cbar[++i]=0.000;
  cbar[++i]=0.000; cbar[++i]=0.430; cbar[++i]=0.370; cbar[++i]=0.000;
  cbar[++i]=0.410; cbar[++i]=0.450; cbar[++i]=0.280; cbar[++i]=0.000;
  cbar[++i]=0.280; cbar[++i]=0.500; cbar[++i]=0.310; cbar[++i]=0.000;
  cbar[++i]=0.340; cbar[++i]=0.510; cbar[++i]=0.280; cbar[++i]=0.000;
  cbar[++i]=0.410; cbar[++i]=0.580; cbar[++i]=0.390; cbar[++i]=0.000;
  cbar[++i]=0.690; cbar[++i]=0.670; cbar[++i]=0.000; cbar[++i]=0.000;
  cbar[++i]=0.810; cbar[++i]=0.770; cbar[++i]=0.370; cbar[++i]=0.000;
  cbar[++i]=0.920; cbar[++i]=0.930; cbar[++i]=0.460; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.250; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.780; cbar[++i]=0.139; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.685; cbar[++i]=0.139; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.497; cbar[++i]=0.000; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.395; cbar[++i]=0.140; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.319; cbar[++i]=0.000; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.140; cbar[++i]=0.000; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.395; cbar[++i]=0.319; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.750; cbar[++i]=1.000; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.497; cbar[++i]=0.000;
  cbar[++i]=0.748; cbar[++i]=0.000; cbar[++i]=0.463; cbar[++i]=0.000;
  cbar[++i]=0.500; cbar[++i]=0.000; cbar[++i]=0.312; cbar[++i]=0.000;
  cbar[++i]=0.250; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.000;
  cbar[++i]=0.125; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.000;   
 }

/*******standard color bar 32 colors, has white as first color***********/
 if(barnumber == STANDARD2)
 {     /*red*/      /*green*/    /*blue*/   /*trace amplitude if used*/
  cbar[i]=  1.000; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.000;
  cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.250; cbar[++i]=0.000;
  cbar[++i]=0.000; cbar[++i]=0.246; cbar[++i]=0.479; cbar[++i]=0.000;
  cbar[++i]=0.000; cbar[++i]=0.310; cbar[++i]=0.560; cbar[++i]=0.000;
  cbar[++i]=0.000; cbar[++i]=0.497; cbar[++i]=0.749; cbar[++i]=0.000;
  cbar[++i]=0.140; cbar[++i]=0.640; cbar[++i]=0.860; cbar[++i]=0.000;
  cbar[++i]=0.400; cbar[++i]=0.740; cbar[++i]=1.000; cbar[++i]=0.000;
  cbar[++i]=0.497; cbar[++i]=0.870; cbar[++i]=1.000; cbar[++i]=0.000;
  cbar[++i]=0.770; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.000;
  cbar[++i]=0.319; cbar[++i]=0.740; cbar[++i]=0.630; cbar[++i]=0.000;
  cbar[++i]=0.310; cbar[++i]=0.550; cbar[++i]=0.500; cbar[++i]=0.000;
  cbar[++i]=0.000; cbar[++i]=0.430; cbar[++i]=0.370; cbar[++i]=0.000;
  cbar[++i]=0.410; cbar[++i]=0.450; cbar[++i]=0.280; cbar[++i]=0.000;
  cbar[++i]=0.280; cbar[++i]=0.500; cbar[++i]=0.310; cbar[++i]=0.000;
  cbar[++i]=0.340; cbar[++i]=0.510; cbar[++i]=0.280; cbar[++i]=0.000;
  cbar[++i]=0.410; cbar[++i]=0.580; cbar[++i]=0.390; cbar[++i]=0.000;
  cbar[++i]=0.690; cbar[++i]=0.670; cbar[++i]=0.000; cbar[++i]=0.000;
  cbar[++i]=0.810; cbar[++i]=0.770; cbar[++i]=0.370; cbar[++i]=0.000;
  cbar[++i]=0.920; cbar[++i]=0.930; cbar[++i]=0.460; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.250; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.780; cbar[++i]=0.139; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.685; cbar[++i]=0.139; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.497; cbar[++i]=0.000; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.395; cbar[++i]=0.140; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.319; cbar[++i]=0.000; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.140; cbar[++i]=0.000; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.395; cbar[++i]=0.319; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.750; cbar[++i]=1.000; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.497; cbar[++i]=0.000;
  cbar[++i]=0.748; cbar[++i]=0.000; cbar[++i]=0.463; cbar[++i]=0.000;
  cbar[++i]=0.500; cbar[++i]=0.000; cbar[++i]=0.312; cbar[++i]=0.000;
  cbar[++i]=0.125; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.000;   
 }


/******************* ICP blue->white->red ***********************************/
 if( barnumber == WHITERED )    /*thirty two colors used*/
 {
    x0  =  0.00 ;
    x1  =  1.00 ;

    xin =  1.0/16.0 ;
    xa  =  0.00 ;
     i  =  0 ;
   for( j=0; j<16; j++ )      /*    Blue to White   */
   {
       cbar[ i ] = xa ;
       cbar[i+1] = xa ;
       cbar[i+2] = x1 ;
       cbar[i+3] = x0 ;
       xa  =  xa + xin ;
       i   =  i + 4 ;       }

    xin =  1.0/16.0 ;
    xa  =  1.00 - xin ;
     i  =  64 ;

   for( j=16; j<32; j++ )      /*   White to Red  */
   {
       cbar[ i ] = x1 ;
       cbar[i+1] = xa ;
       cbar[i+2] = x1 ;
       cbar[i+3] = x0 ;
       xa  =  xa - xin ;
       i   =  i + 4 ;       }
 }



/****************ICP blue->white->gray->black->yellow *********************/
 if( barnumber == BLUEYELLOW1 )  /*thirty two colors used*/
 {
    x0  =  0.00 ;
    x1  =  1.00 ;
    xin =  0.10 ;
    xa  =  0.00 ;
     i  =  0 ;
   for( j=0; j<11; j++ )      /*    Blue to White   */
   {
       cbar[ i ] = xa ;
       cbar[i+1] = xa ;
       cbar[i+2] = x1 ;
       cbar[i+3] = x0 ;
       xa  =  xa + xin ;
       i   =  i + 4 ;       }
    xin =  1.0 / 11.0 ;
    xa  =  x1 - xin ;
     i  =  44 ;
   for( j=11; j<22; j++ )      /*    White to Black   */
   {
       cbar[ i ] = xa ;
       cbar[i+1] = xa ;
       cbar[i+2] = xa ;
       cbar[i+3] = x0 ;
       xa  =  xa - xin ;
       i   =  i + 4 ;       }
    xin =  0.10 ;
    xa  =  xin ;
     i  =  88 ;
   for( j=22; j<32; j++ )      /*    Black to Yellow  */
   {
       cbar[ i ] = xa ;
       cbar[i+1] = xa ;
       cbar[i+2] = x0 ;
       cbar[i+3] = x0 ;
       xa  =  xa + xin ;
       i   =  i + 4 ;       }
 }




/*******************ICP blue->white, black->yellow  **********************/
 if( barnumber == BLUEYELLOW2 )        /*thirty one colors used*/
 {
    x0  =  0.00 ;
    x1  =  1.00 ;
    xin =  1.0/15.0 ;
    xa  =  0.00 ;
     i  =  0 ;
   for( j=0; j<16; j++ )      /*    Blue to White   */
   {
       cbar[ i ] = xa ;
       cbar[i+1] = xa ;
       cbar[i+2] = x1 ;
       cbar[i+3] = x0 ;
       xa  =  xa + xin ;
       i   =  i + 4 ;       }
    xin =  1.0/15.0 ;
    xa  =  0.00  ;
     i  =  64 ;
   for( j=16; j<31; j++ )      /*    Black to Yellow   */
   {
       cbar[ i ] = xa ;
       cbar[i+1] = xa ;
       cbar[i+2] = x0 ;
       cbar[i+3] = x0 ;
       xa  =  xa + xin ;
       i   =  i + 4 ;       }
 }

/*********************semblance plot color scheme****************************/
 if(barnumber == SEMBLANCE)//32 color
 {     /*red*/      /*green*/    /*blue*/   /*trace amplitude if used*/
 i = 0;
 cbar[i]   = 0.071; cbar[++i] = 0.085; cbar[++i] = 0.057; cbar[++i] = 0.0;
 cbar[++i] = 0.142; cbar[++i] = 0.171; cbar[++i] = 0.114; cbar[++i] = 0.0;
 cbar[++i] = 0.213; cbar[++i] = 0.256; cbar[++i] = 0.171; cbar[++i] = 0.0;
 cbar[++i] = 0.285; cbar[++i] = 0.342; cbar[++i] = 0.228; cbar[++i] = 0.0;
 cbar[++i] = 0.427; cbar[++i] = 0.513; cbar[++i] = 0.342; cbar[++i] = 0.0;
 cbar[++i] = 0.499; cbar[++i] = 0.599; cbar[++i] = 0.399; cbar[++i] = 0.0;
 cbar[++i] = 0.571; cbar[++i] = 0.514; cbar[++i] = 0.342; cbar[++i] = 0.0;
 cbar[++i] = 0.713; cbar[++i] = 0.343; cbar[++i] = 0.228; cbar[++i] = 0.0;
 cbar[++i] = 0.785; cbar[++i] = 0.257; cbar[++i] = 0.171; cbar[++i] = 0.0;
 cbar[++i] = 0.856; cbar[++i] = 0.172; cbar[++i] = 0.114; cbar[++i] = 0.0;
 cbar[++i] = 0.927; cbar[++i] = 0.086; cbar[++i] = 0.057; cbar[++i] = 0.0;
 cbar[++i] = 0.999; cbar[++i] = 0.000; cbar[++i] = 0.000; cbar[++i] = 0.0;
 cbar[++i] = 0.857; cbar[++i] = 0.142; cbar[++i] = 0.000; cbar[++i] = 0.0;
 cbar[++i] = 0.714; cbar[++i] = 0.285; cbar[++i] = 0.000; cbar[++i] = 0.0;
 cbar[++i] = 0.572; cbar[++i] = 0.427; cbar[++i] = 0.000; cbar[++i] = 0.0;
 cbar[++i] = 0.429; cbar[++i] = 0.570; cbar[++i] = 0.000; cbar[++i] = 0.0;
 cbar[++i] = 0.286; cbar[++i] = 0.713; cbar[++i] = 0.000; cbar[++i] = 0.0;
 cbar[++i] = 0.144; cbar[++i] = 0.855; cbar[++i] = 0.000; cbar[++i] = 0.0;
 cbar[++i] = 0.001; cbar[++i] = 0.998; cbar[++i] = 0.000; cbar[++i] = 0.0;
 cbar[++i] = 0.000; cbar[++i] = 0.857; cbar[++i] = 0.142; cbar[++i] = 0.0;
 cbar[++i] = 0.000; cbar[++i] = 0.714; cbar[++i] = 0.285; cbar[++i] = 0.0;
 cbar[++i] = 0.000; cbar[++i] = 0.572; cbar[++i] = 0.427; cbar[++i] = 0.0;
 cbar[++i] = 0.000; cbar[++i] = 0.429; cbar[++i] = 0.570; cbar[++i] = 0.0;
 cbar[++i] = 0.000; cbar[++i] = 0.286; cbar[++i] = 0.713; cbar[++i] = 0.0;
 cbar[++i] = 0.000; cbar[++i] = 0.144; cbar[++i] = 0.855; cbar[++i] = 0.0; 
 cbar[++i] = 0.000; cbar[++i] = 0.001; cbar[++i] = 0.998; cbar[++i] = 0.0;
 cbar[++i] = 0.000; cbar[++i] = 0.000; cbar[++i] = 0.857; cbar[++i] = 0.0;
 cbar[++i] = 0.000; cbar[++i] = 0.000; cbar[++i] = 0.714; cbar[++i] = 0.0;
 cbar[++i] = 0.000; cbar[++i] = 0.000; cbar[++i] = 0.572; cbar[++i] = 0.0;
 cbar[++i] = 0.000; cbar[++i] = 0.000; cbar[++i] = 0.450; cbar[++i] = 0.0;
 cbar[++i] = 0.000; cbar[++i] = 0.000; cbar[++i] = 0.400; cbar[++i] = 0.0;
 cbar[++i] = 0.000; cbar[++i] = 0.000; cbar[++i] = 0.350; cbar[++i] = 0.0;
 }


/******************* Second semblance color bar **************************/
 if( barnumber == SEMBLANCE2 )
 {
  cbar[i]  =1.000; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.0;
  cbar[++i]=0.756; cbar[++i]=0.870; cbar[++i]=1.000; cbar[++i]=0.0;
  cbar[++i]=0.569; cbar[++i]=0.811; cbar[++i]=1.000; cbar[++i]=0.0;
  cbar[++i]=0.316; cbar[++i]=0.773; cbar[++i]=1.000; cbar[++i]=0.0;
  cbar[++i]=0.002; cbar[++i]=0.756; cbar[++i]=0.938; cbar[++i]=0.0;
  cbar[++i]=0.001; cbar[++i]=0.679; cbar[++i]=0.679; cbar[++i]=0.0;
  cbar[++i]=0.001; cbar[++i]=0.607; cbar[++i]=0.458; cbar[++i]=0.0;
  cbar[++i]=0.184; cbar[++i]=1.000; cbar[++i]=0.581; cbar[++i]=0.0;
  cbar[++i]=0.002; cbar[++i]=0.946; cbar[++i]=0.164; cbar[++i]=0.0;
  cbar[++i]=0.157; cbar[++i]=0.821; cbar[++i]=0.001; cbar[++i]=0.0;
  cbar[++i]=0.317; cbar[++i]=0.700; cbar[++i]=0.002; cbar[++i]=0.0;
  cbar[++i]=0.392; cbar[++i]=0.603; cbar[++i]=0.002; cbar[++i]=0.0;
  cbar[++i]=0.423; cbar[++i]=0.521; cbar[++i]=0.001; cbar[++i]=0.0;
  cbar[++i]=0.830; cbar[++i]=0.867; cbar[++i]=0.003; cbar[++i]=0.0;
  cbar[++i]=0.840; cbar[++i]=0.766; cbar[++i]=0.001; cbar[++i]=0.0;
  cbar[++i]=0.836; cbar[++i]=0.673; cbar[++i]=0.001; cbar[++i]=0.0;
  cbar[++i]=0.825; cbar[++i]=0.589; cbar[++i]=0.001; cbar[++i]=0.0;
  cbar[++i]=0.813; cbar[++i]=0.511; cbar[++i]=0.001; cbar[++i]=0.0;
  cbar[++i]=0.806; cbar[++i]=0.438; cbar[++i]=0.001; cbar[++i]=0.0;
  cbar[++i]=1.000; cbar[++i]=0.821; cbar[++i]=0.674; cbar[++i]=0.0;
  cbar[++i]=1.000; cbar[++i]=0.723; cbar[++i]=0.576; cbar[++i]=0.0;
  cbar[++i]=1.000; cbar[++i]=0.630; cbar[++i]=0.529; cbar[++i]=0.0;
  cbar[++i]=1.000; cbar[++i]=0.544; cbar[++i]=0.529; cbar[++i]=0.0;
  cbar[++i]=1.000; cbar[++i]=0.463; cbar[++i]=0.576; cbar[++i]=0.0;
  cbar[++i]=1.000; cbar[++i]=0.386; cbar[++i]=0.677; cbar[++i]=0.0;
  cbar[++i]=1.000; cbar[++i]=0.817; cbar[++i]=0.960; cbar[++i]=0.0;
  cbar[++i]=0.961; cbar[++i]=0.726; cbar[++i]=1.000; cbar[++i]=0.0;
  cbar[++i]=0.856; cbar[++i]=0.655; cbar[++i]=1.000; cbar[++i]=0.0;
  cbar[++i]=0.747; cbar[++i]=0.592; cbar[++i]=1.000; cbar[++i]=0.0;
  cbar[++i]=0.635; cbar[++i]=0.536; cbar[++i]=1.000; cbar[++i]=0.0;
  cbar[++i]=0.521; cbar[++i]=0.486; cbar[++i]=1.000; cbar[++i]=0.0;
  cbar[++i]=0.841; cbar[++i]=0.851; cbar[++i]=1.000; cbar[++i]=0.0;
 }



/********************9 colors for velocity contours**************************/
 if(barnumber==CONTOUR)
 {     /*red*/      /*green*/    /*blue*/   /*trace amplitude if used*/
  cbar[i]=  0.000; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.620; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.400; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.500; cbar[++i]=0.500; cbar[++i]=0.00;
  cbar[++i]=0.700; cbar[++i]=0.700; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.410; cbar[++i]=0.450; cbar[++i]=0.280; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.430; cbar[++i]=0.370; cbar[++i]=0.00;
  cbar[++i]=0.310; cbar[++i]=0.550; cbar[++i]=0.500; cbar[++i]=0.00;
  cbar[++i]=0.319; cbar[++i]=0.740; cbar[++i]=0.630; cbar[++i]=0.00;
  cbar[++i]=0.500; cbar[++i]=0.000; cbar[++i]=0.312; cbar[++i]=0.00;
  cbar[++i]=0.748; cbar[++i]=0.000; cbar[++i]=0.463; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.497; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.750; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.497; cbar[++i]=0.870; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.770; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.00;
 }



/***************************standard gray scale**************************/
 if(barnumber==GRAY)         /*variable number of colors used*/
 {
   byte_ratio = 255.0 / maxcolors;
   rgb_ratio  = 1.0 / maxcolors;
   for(j=0;j<maxcolors;j++)
   {
     for(k=0;k<3;k++)
     {
       cbar[i] = (maxcolors-j) * rgb_ratio;
       i++;
     }
     //integerval = (long)(j * byte_ratio);
     //cbar[i] = integerval;
     cbar[i] = 0.0;
     i++;
   }
   *use_amplitudes = False;
 }




/**************** Allistair Brown Color Scheme from old GWS **************/
 if(barnumber==ALLISTAIR1)
 {     /*red*/        /*green*/        /*blue*/     /*trace amplitude if used*/
  cbar[i]  =1.000; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.750; cbar[++i]=0.750; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.750; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.750; cbar[++i]=0.375; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.500; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.250; cbar[++i]=0.00;
  cbar[++i]=0.500; cbar[++i]=1.000; cbar[++i]=0.125; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.750; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.500; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.00;
 }



/**************** Allistair Brown Color Scheme from old GWS **************/
 if(barnumber==ALLISTAIR2)
 {     /*red*/        /*green*/        /*blue*/     /*trace amplitude if used*/
  cbar[i]=  1.000; cbar[++i]=0.500; cbar[++i]=0.500; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.500; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.500; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.500; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.250; cbar[++i]=0.000; cbar[++i]=0.750; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.00;
 }



/**************** Allistair Brown Color Scheme from old GWS **************/
 if(barnumber==ALLISTAIR3)
 {     /*red*/        /*green*/        /*blue*/     /*trace amplitude if used*/
  cbar[i]=  1.000; cbar[++i]=0.750; cbar[++i]=0.750; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.500; cbar[++i]=0.500; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.625; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.620; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.500; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.750; cbar[++i]=1.000; cbar[++i]=0.375; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.875; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.375; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.750; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.750; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.750; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.750; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.500; cbar[++i]=0.000; cbar[++i]=0.500; cbar[++i]=0.00;
 }



 if(barnumber==ALLISTAIR4)
 {     /*red*/        /*green*/        /*blue*/     /*trace amplitude if used*/
  cbar[i]=  0.500; cbar[++i]=0.000; cbar[++i]=0.500; cbar[++i]=0.00;
  cbar[++i]=0.750; cbar[++i]=0.250; cbar[++i]=0.750; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.500; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.500; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.750; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.500; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.750; cbar[++i]=0.250; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.875; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.500; cbar[++i]=1.000; cbar[++i]=0.250; cbar[++i]=0.00;
  cbar[++i]=0.688; cbar[++i]=0.563; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.688; cbar[++i]=0.313; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.500; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.00;
 }




 if(barnumber==ALLISTAIR5)
 {     /*red*/        /*green*/        /*blue*/     /*trace amplitude if used*/
  cbar[i]=  1.000; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.933; cbar[++i]=0.000; cbar[++i]=0.067; cbar[++i]=0.00;
  cbar[++i]=0.867; cbar[++i]=0.134; cbar[++i]=0.134; cbar[++i]=0.00;
  cbar[++i]=0.800; cbar[++i]=0.267; cbar[++i]=0.200; cbar[++i]=0.00;
  cbar[++i]=0.733; cbar[++i]=0.400; cbar[++i]=0.267; cbar[++i]=0.00;
  cbar[++i]=0.667; cbar[++i]=0.534; cbar[++i]=0.333; cbar[++i]=0.00;
  cbar[++i]=0.600; cbar[++i]=0.667; cbar[++i]=0.400; cbar[++i]=0.00;
  cbar[++i]=0.533; cbar[++i]=0.800; cbar[++i]=0.467; cbar[++i]=0.00;
  cbar[++i]=0.467; cbar[++i]=0.933; cbar[++i]=0.533; cbar[++i]=0.00;
  cbar[++i]=0.400; cbar[++i]=0.800; cbar[++i]=0.600; cbar[++i]=0.00;
  cbar[++i]=0.333; cbar[++i]=0.667; cbar[++i]=0.667; cbar[++i]=0.00;
  cbar[++i]=0.267; cbar[++i]=0.533; cbar[++i]=0.733; cbar[++i]=0.00;
  cbar[++i]=0.200; cbar[++i]=0.400; cbar[++i]=0.800; cbar[++i]=0.00;
  cbar[++i]=0.133; cbar[++i]=0.267; cbar[++i]=0.867; cbar[++i]=0.00;
  cbar[++i]=0.067; cbar[++i]=0.133; cbar[++i]=0.933; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.00;
 }



 if(barnumber==ALLISTAIR6)
 {     /*red*/        /*green*/        /*blue*/     /*trace amplitude if used*/
  cbar[i]=  0.875; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.251; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.376; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.502; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.750; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.875; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.878; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.749; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.502; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.251; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.502; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.749; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.502; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.251; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.749; cbar[++i]=0.00;
 }



 if(barnumber==ALLISTAIR7)
 {     /*red*/        /*green*/        /*blue*/     /*trace amplitude if used*/
  cbar[i]  =0.000; cbar[++i]=0.000; cbar[++i]=0.749; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.251; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.502; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.749; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.502; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.251; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.502; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.749; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.878; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.875; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.750; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.502; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.376; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.251; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.875; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.00;
 }




 if(barnumber==ALLISTAIR8)
 {     /*red*/        /*green*/        /*blue*/     /*trace amplitude if used*/
  cbar[i]  =1.000; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.875; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.251; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.376; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.400; cbar[++i]=0.200; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.502; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.749; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=0.875; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.878; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.749; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.502; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.251; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.502; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.749; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.502; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.251; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.00;
  cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.749; cbar[++i]=0.00;
  cbar[++i]=0.298; cbar[++i]=0.000; cbar[++i]=0.400; cbar[++i]=0.00;
 }


/********************9 colors for cfg normalized binning*********************/
if(barnumber==SECTOR)
  {
  cbar[i]  =0.000; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.000;
  cbar[++i]=1.000; cbar[++i]=0.140; cbar[++i]=0.000; cbar[++i]=1.000;
  cbar[++i]=1.000; cbar[++i]=0.685; cbar[++i]=0.139; cbar[++i]=2.000;
  cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.250; cbar[++i]=3.000;
  cbar[++i]=0.748; cbar[++i]=0.000; cbar[++i]=0.463; cbar[++i]=4.000;
  cbar[++i]=0.800; cbar[++i]=0.800; cbar[++i]=0.800; cbar[++i]=5.000;
  cbar[++i]=0.690; cbar[++i]=0.670; cbar[++i]=0.000; cbar[++i]=6.000;
  cbar[++i]=0.000; cbar[++i]=0.497; cbar[++i]=0.749; cbar[++i]=7.000;
  cbar[++i]=0.497; cbar[++i]=0.920; cbar[++i]=1.000; cbar[++i]=8.000;
  cbar[++i]=0.340; cbar[++i]=0.510; cbar[++i]=0.280; cbar[++i]=9.000;
  }





/**********************medram color bar***************************************/
 if(barnumber==MEDRAM)
 {     /*red*/      /*green*/    /*blue*/   /*trace amplitude if used*/
  cbar[i]=  0.000; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]= -10.0;
  cbar[++i]=0.000; cbar[++i]=0.440; cbar[++i]=1.000; cbar[++i]= -7.00;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]= -4.00;
  cbar[++i]=0.600; cbar[++i]=0.600; cbar[++i]=0.600; cbar[++i]= -1.00;
  cbar[++i]=0.750; cbar[++i]=0.750; cbar[++i]=0.750; cbar[++i]=  1.00;
  cbar[++i]=0.600; cbar[++i]=0.600; cbar[++i]=0.600; cbar[++i]=  4.00;
  cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=  7.00;
  cbar[++i]=0.930; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]= 10.00;
  cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=999999.0;
 }


/********************** SAAT color bar ***************************************/
if(barnumber == SAAT)
 {     /*red*/      /*green*/    /*blue*/   /*trace amplitude if used*/
  cbar[i]=  0.000; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=-999999.0;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=-25.000;
  cbar[++i]=0.996; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=-24.000;
  cbar[++i]=0.992; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=-23.000;
  cbar[++i]=0.988; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=-22.000;
  cbar[++i]=0.980; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=-21.000;
  cbar[++i]=0.227; cbar[++i]=0.851; cbar[++i]=0.125; cbar[++i]=-20.000;
  cbar[++i]=0.227; cbar[++i]=0.851; cbar[++i]=0.125; cbar[++i]=-19.231;
  cbar[++i]=0.231; cbar[++i]=0.851; cbar[++i]=0.125; cbar[++i]=-18.462;
  cbar[++i]=0.231; cbar[++i]=0.851; cbar[++i]=0.125; cbar[++i]=-17.692;
  cbar[++i]=0.231; cbar[++i]=0.851; cbar[++i]=0.125; cbar[++i]=-16.923;
  cbar[++i]=0.235; cbar[++i]=0.851; cbar[++i]=0.125; cbar[++i]=-16.154;
  cbar[++i]=0.235; cbar[++i]=0.851; cbar[++i]=0.125; cbar[++i]=-15.384;
  cbar[++i]=0.239; cbar[++i]=0.851; cbar[++i]=0.125; cbar[++i]=-14.615;
  cbar[++i]=0.239; cbar[++i]=0.851; cbar[++i]=0.125; cbar[++i]=-13.846;
  cbar[++i]=0.239; cbar[++i]=0.851; cbar[++i]=0.126; cbar[++i]=-13.077;
  cbar[++i]=0.243; cbar[++i]=0.851; cbar[++i]=0.126; cbar[++i]=-12.308;
  cbar[++i]=0.243; cbar[++i]=0.851; cbar[++i]=0.126; cbar[++i]=-11.538;
  cbar[++i]=0.247; cbar[++i]=0.851; cbar[++i]=0.126; cbar[++i]=-10.769;
  cbar[++i]=1.000; cbar[++i]=0.902; cbar[++i]=0.000; cbar[++i]=-10.000;
  cbar[++i]=1.000; cbar[++i]=0.910; cbar[++i]=0.000; cbar[++i]=-9.000;
  cbar[++i]=1.000; cbar[++i]=0.918; cbar[++i]=0.000; cbar[++i]=-8.000;
  cbar[++i]=0.020; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=-7.000;
  cbar[++i]=0.012; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=-6.250;
  cbar[++i]=0.004; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=-5.500;
  cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=-4.750;
  cbar[++i]=0.361; cbar[++i]=0.361; cbar[++i]=0.400; cbar[++i]=-4.000;
  cbar[++i]=0.478; cbar[++i]=0.514; cbar[++i]=0.522; cbar[++i]=-3.200;
  cbar[++i]=0.604; cbar[++i]=0.639; cbar[++i]=0.620; cbar[++i]=-2.400;
  cbar[++i]=0.741; cbar[++i]=0.761; cbar[++i]=0.729; cbar[++i]=-1.600;
  cbar[++i]=0.882; cbar[++i]=0.878; cbar[++i]=0.863; cbar[++i]=-0.800;
  cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.000;  
  cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.000;
  cbar[++i]=0.882; cbar[++i]=0.878; cbar[++i]=0.863; cbar[++i]=0.800;
  cbar[++i]=0.741; cbar[++i]=0.761; cbar[++i]=0.729; cbar[++i]=1.600;
  cbar[++i]=0.604; cbar[++i]=0.630; cbar[++i]=0.620; cbar[++i]=2.400;
  cbar[++i]=0.478; cbar[++i]=0.514; cbar[++i]=0.522; cbar[++i]=3.200;
  cbar[++i]=0.361; cbar[++i]=0.361; cbar[++i]=0.400; cbar[++i]=4.000;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=4.750;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=5.500;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=6.250;
  cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=7.000;
  cbar[++i]=0.980; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=8.000;
  cbar[++i]=0.992; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=9.000;
  cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=0.000; cbar[++i]=10.000;
  cbar[++i]=1.000; cbar[++i]=0.702; cbar[++i]=.2470; cbar[++i]=10.770;
  cbar[++i]=1.000; cbar[++i]=0.702; cbar[++i]=0.247; cbar[++i]=11.539;
  cbar[++i]=1.000; cbar[++i]=0.702; cbar[++i]=0.247; cbar[++i]=12.308;
  cbar[++i]=1.000; cbar[++i]=0.702; cbar[++i]=0.247; cbar[++i]=13.077;
  cbar[++i]=1.000; cbar[++i]=0.702; cbar[++i]=0.247; cbar[++i]=13.846;
  cbar[++i]=1.000; cbar[++i]=0.702; cbar[++i]=0.247; cbar[++i]=14.615;
  cbar[++i]=1.000; cbar[++i]=0.702; cbar[++i]=0.247; cbar[++i]=15.385;
  cbar[++i]=1.000; cbar[++i]=0.702; cbar[++i]=0.247; cbar[++i]=16.154;
  cbar[++i]=1.000; cbar[++i]=0.702; cbar[++i]=0.247; cbar[++i]=16.923;
  cbar[++i]=1.000; cbar[++i]=0.702; cbar[++i]=0.247; cbar[++i]=17.692;
  cbar[++i]=1.000; cbar[++i]=0.702; cbar[++i]=0.251; cbar[++i]=18.462;
  cbar[++i]=1.000; cbar[++i]=0.702; cbar[++i]=0.251; cbar[++i]=19.231;
  cbar[++i]=1.000; cbar[++i]=0.702; cbar[++i]=0.251; cbar[++i]=20.000;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=21.000;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=22.000;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=23.000;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=24.000;
  cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=25.000;
  cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=0.000; cbar[++i]=999999.0;
 }


/********************** SAAT color bar ***************************************/
if(barnumber == BLUEWHITERED)
 {     /*red*/      /*green*/    /*blue*/   /*trace amplitude if used*/
  cbar[i]=  0.000; cbar[++i]=0.000; cbar[++i]=1.000; cbar[++i]=4.419;    
  cbar[++i]=0.067; cbar[++i]=0.067; cbar[++i]=1.000; cbar[++i]=-4.124;
  cbar[++i]=0.133; cbar[++i]=0.133; cbar[++i]=1.000; cbar[++i]=-3.829;
  cbar[++i]=0.200; cbar[++i]=0.200; cbar[++i]=1.000; cbar[++i]=-3.535;
  cbar[++i]=0.267; cbar[++i]=0.267; cbar[++i]=1.000; cbar[++i]=-3.240;
  cbar[++i]=0.333; cbar[++i]=0.333; cbar[++i]=1.000; cbar[++i]=-2.946;
  cbar[++i]=0.400; cbar[++i]=0.400; cbar[++i]=1.000; cbar[++i]=-2.651;
  cbar[++i]=0.467; cbar[++i]=0.467; cbar[++i]=1.000; cbar[++i]=-2.357;
  cbar[++i]=0.533; cbar[++i]=0.533; cbar[++i]=1.000; cbar[++i]=-2.062;
  cbar[++i]=0.600; cbar[++i]=0.600; cbar[++i]=1.000; cbar[++i]=-1.767;
  cbar[++i]=0.667; cbar[++i]=0.667; cbar[++i]=1.000; cbar[++i]=-1.473;
  cbar[++i]=0.733; cbar[++i]=0.733; cbar[++i]=1.000; cbar[++i]=-1.178;
  cbar[++i]=0.800; cbar[++i]=0.800; cbar[++i]=1.000; cbar[++i]=-0.884;
  cbar[++i]=0.867; cbar[++i]=0.867; cbar[++i]=1.000; cbar[++i]=-0.589;
  cbar[++i]=0.933; cbar[++i]=0.933; cbar[++i]=1.000; cbar[++i]=-0.295;
  cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]=1.000; cbar[++i]= 0.000;
  cbar[++i]=1.000; cbar[++i]=0.938; cbar[++i]=0.938; cbar[++i]= 0.295;
  cbar[++i]=1.000; cbar[++i]=0.875; cbar[++i]=0.875; cbar[++i]= 0.589;
  cbar[++i]=1.000; cbar[++i]=0.812; cbar[++i]=0.812; cbar[++i]= 0.884;
  cbar[++i]=1.000; cbar[++i]=0.750; cbar[++i]=0.750; cbar[++i]= 1.178;
  cbar[++i]=1.000; cbar[++i]=0.688; cbar[++i]=0.688; cbar[++i]= 1.473;
  cbar[++i]=1.000; cbar[++i]=0.625; cbar[++i]=0.625; cbar[++i]= 1.767;
  cbar[++i]=1.000; cbar[++i]=0.562; cbar[++i]=0.562; cbar[++i]= 2.062;
  cbar[++i]=1.000; cbar[++i]=0.500; cbar[++i]=0.500; cbar[++i]= 2.357;
  cbar[++i]=1.000; cbar[++i]=0.438; cbar[++i]=0.438; cbar[++i]= 2.651;
  cbar[++i]=1.000; cbar[++i]=0.375; cbar[++i]=0.375; cbar[++i]= 2.946;
  cbar[++i]=1.000; cbar[++i]=0.312; cbar[++i]=0.312; cbar[++i]= 3.240;
  cbar[++i]=1.000; cbar[++i]=0.250; cbar[++i]=0.250; cbar[++i]= 3.535;
  cbar[++i]=1.000; cbar[++i]=0.188; cbar[++i]=0.188; cbar[++i]= 3.829;
  cbar[++i]=1.000; cbar[++i]=0.125; cbar[++i]=0.125; cbar[++i]= 4.124;
  cbar[++i]=1.000; cbar[++i]=0.062; cbar[++i]=0.062; cbar[++i]= 4.419;
 }


/*compute number of colors and see if there are any amplitude values */
 ++i;
 *numcolors = i / 4;

 if(barnumber != GRAY) 
    {
    j = 3;
    for(i=0;i<*numcolors-1;i++)
      {
      amplitude = cbar[j];
      if(amplitude) *use_amplitudes = True;
      j += 4;
      }
    }

}
