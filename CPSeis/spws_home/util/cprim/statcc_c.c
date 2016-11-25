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

                       /*    statcc_c.c    */

      /*  this is a c-language version of statcc.f           */
      /*  converted from fortran 8/08/96                     */
      /*  see documentation in ~spws/util/cpsprim/statcc.f   */

#include "cprim.h"
#include "named_constants.h"


void statcc_c (float SHFT, int N, const float *A, float *B) 
{
      int   L, LA, LB, LC, LD, I, I1, I2;
      float F, G, F2, G2, FACTOR, WA, WB, WC, WD;

/*----------SHIFT IS NEGATIVE----------*/

      if(SHFT < 0.0)
           {
           L  = -SHFT;
           F  = -SHFT - L;
           G  = 1.0 - F;
           LA = L - 1;
           }

/*----------SHIFT IS POSITIVE----------*/

      else if(SHFT > 0.0)
           {
           L  = -SHFT;
           G  = SHFT + L;
           F  = 1.0 - G;
           LA = L - 2;
           }

/*----------SHIFT IS ZERO----------*/

      else
           {
           for(I = 0; I < N; I++) { B[I] = A[I]; }
           return;
           }

/*----------GET WEIGHTS----------*/

      LB     = LA + 1;
      LC     = LA + 2;
      LD     = LA + 3;
      F2     = F * F;
      G2     = G * G;
      FACTOR = 1.0 / (F2 + G2 + 1.0);
      WA     = -G2 * F           * FACTOR;
      WB     = (G2 * F + G2 + G) * FACTOR;
      WC     = (F2 * G + F2 + F) * FACTOR;
      WD     = -F2 * G           * FACTOR;

/*----------GET READY TO APPLY SHIFT----------*/

      I1 = MaximumValue(1, 1 - LA) - 1;
      I2 = MinimumValue(N, N - LD) - 1;
      if(I1 > I2)
           {
           for(I = 0; I < N; I++) { B[I] = 0.0; }
           return;
           }
      if(I1 > 0)
           {
           for(I = 0; I < I1; I++) { B[I] = 0.0; }
           }
      if(I2 < N - 1)
           {
           for(I = I2 + 1; I < N; I++) { B[I] = 0.0; }
           }

/*----------APPLY SHIFT----------*/

      for(I = I1; I <= I2; I++)
           {
           B[I] = WA * A[I + LA] +
                  WB * A[I + LB] +
                  WC * A[I + LC] +
                  WD * A[I + LD];
           }
} 

