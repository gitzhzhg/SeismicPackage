!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- mth.f90 --------------------------------!!
!!------------------------------- mth.f90 --------------------------------!!
!!------------------------------- mth.f90 --------------------------------!!


!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : mth
! Category   : math
! Written    : 1999-08-20   by: Carl Dorman
! Revised    : 2004-03-15   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Repository for miscellaneous mathematical subprograms
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!  mth_ismax is an integer function that searches a vector of real elements
!  for the first occurrence of the maximum value contained in the vector.
!
!  mth_ismin is an integer function that searches a vector of real elements
!  for the first occurrence of the minimum value contained in the vector.
!
!  mth_isamax is an integer function that searches a vector of real elements
!  for the first occurrence of the maximum absolute value in the vector.
!
!  mth_isamin is an integer function that searches a vector of real elements
!  for the first occurrence of the minimum absolute value in the vector.
!
!  mth_icamin is an integer function that searches a vector of complex elements
!  for the first occurrence of the minimum absolute value in the vector.
!
!  mth_icamax is an integer function that searches a vector of complex elements
!  for the first occurrence of the maximum absolute value in the vector.
!
!  mth_search_equal is an integer function that searches a vector of integer
!  or real or double precision elements for the first occurrence of an element
!  which matches the target.
!
!  mth_search_unequal is an integer function that searches a vector of integer
!  or real or double precision elements for the first occurrence of an element
!  which does not match the target.
!
!  The functions mth_ismax, mth_ismin, mth_isamax, mth_isamin, mth_icamax,
!  mth_icamin, mth_search_equal, and mth_search_unequal can search vectors
!  in either ascending or descending order.
!  If the increment is positive, the vector is searched in ascending order;
!  if negative, descending.  The user may also specify an increment of any
!  integer value.  Hence, if it is desired to search every element, the
!  increment would be specified as '1', if every other element, '2',
!  if every third element, '3' and so on.                          
!                                                                        
!  The absolute value of a complex element is calculated in the standard way:
!
!          absolute_value(x) = sqrt(real(x)**2 + imag(x)**2).
!
!  Hence, if the complex number x is equal to (3.0 + 4.0i), then
!
!          absolute_value(3.0 + 4.0i) = sqrt(3.0**2 + 4.0**2) = 5.0
!
!  The logical function mth_ameq returns .true. if the two real arguments
!  (value1 and value2) are ALmost EQual (+/- EPSILON, the third argument).
!
!          equal = mth_ameq(1.99999, 2.00000, 1.0e-5)
!
!  The integer function mth_compare compares two real arguments and
!  returns -1, 0 or +1 if the first argument is much less than,
!  almost equal to or much greater than the second argument.
!  This function is similar to mth_ameq, except EPSILON is implicit.
!
!          compare = mth_compare(1.99999, 2.00000)
!
!  The real function mth_divide_clip performs robust division of two
!  reals, clipping the result to a specified limit. This replaces the
!  xxxxDIVM routines in many of the Cray processes written by Bill Harlan
!  (e.g., RMULDIVM, MDIPDIVM, etc.).
!
!  Additional routines in this module, including random number generators,
!  are documented in individual sections below.
!-------------------------------------------------------------------------------
!  The function mth_random_number is overloaded with an integer and a 
!  floating point version.  This functions allow independent sequences of random
!  numbers to be generated by providing them a place to store a seed value
!  which specifies the starting seed value with the first call and a place to
!  store the seed value to be used for generating the next random number.
!  The initial value of seed is arbitrary but preferably positive.
!  
!  The integer version is ran=mth_random_number(seed, max) 
!    seed and max are integers. seed is inout-output.  
!    max(input) specifies the range of the resulting random number, 
!    so the numbers returned will be in the range from 1 and max inclusively.
!
!  The floating point version is ran=mth_random_number(seed)
!    seed(input-output) is an integer.  
!    the results are between 0. and 1., inclusively.
!
!  These routines produce identical results of associated c versions found in
!  mth_crou.c, and in fact, they call them for their calculations.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!                  o                          i     i      i     i
!                index = mth_ismax          ( n,    x,    inc)
!                index = mth_ismin          ( n,    x,    inc)
!                index = mth_isamax         ( n,    x,    inc)
!                index = mth_isamin         ( n,    x,    inc)
!                index = mth_icamax         ( n,    c,    inc)
!                index = mth_icamin         ( n,    c,    inc)
!                index = mth_search_equal   ( n,    a,    inc, target)
!                index = mth_search_unequal ( n,    a,    inc, target)
!
!                  o               i   i      i
!                equal = mth_ameq (r1, r2, epsilon)
!                equal = mth_ameq (d1, d2, epsilon)
!
!                   o                   i   i
!                compare = mth_compare (r1, r2)
!                compare = mth_compare (d1, d2)
!
! Arguments for function mth_ismax, mth_ismin, mth_isamax, mth_isamin,
!    mth_icamax, mth_icamin, mth_search_equal, mth_search_unequal:        
!                                  
! integer              index   =  variable to receive results of function.
!                                                                         
! integer                  n   =  number of elements in the real vector x   
!                                   or in the complex vector c; if n is    
!                                   less than or equal to zero, the vector  
!                                   will not be searched and the function  
!                                   value will be returned as zero.       
!                                                                          
! real                     x   =  name of the array containing the real
!                                   vector x.                           
!                                                                        
! double precision         c   =  name of the array containing the double
!                                   precision vector c.      
!                                                                        
! integer or real or       a   =  name of the array containing the integer
!  double precision or              or real or double precision or character
!  character(len=*)                 vector a. 
!                                                                        
! integer                inc   =  increment for which the vector will be
!                                   searched; if inc is positive, the vector
!                                   will be searched in ascending order; if 
!                                   inc is negative, the vector will be
!                                   searched in descending order; if inc is 
!                                   zero, the vector will not be searched
!                                   and the function value will be returned
!                                   as zero.     
!                                                                          
! integer or real or    target =  integer or real or double precision or
!  double precision or              character target to be compared with each
!  character(len=*)                 array element.
!                                                                        
! Arguments for functions mth_ameq and mth_compare:
!
! real              r1   =  first value in comparison.
! real              r2   =  second value in comparison.
! double precision  d1   =  first value in comparison.
! double precision  d2   =  second value in comparison.
! real              epsilon  =  tolerance for comparison.
!
!-------------------------------------------------------------------------------
! Binning functions:
!
!               o                            i      i     i
!             vinc    = mth_bin_increment (vinit, vlast, ntot     )
!
!             ibin    = mth_bin_number    (vinit, vinc , val      )
!             ntot    = mth_bin_number    (vinit, vinc , val=vlast)
!
!             vcenter = mth_bin_center    (vinit, vinc , ibin     )
!             vlast   = mth_bin_center    (vinit, vinc , ibin=ntot)
!
!
! real or double  vinit   = value for the center of the first bin.
! real or double  vlast   = value for the center of the last  bin.
! real or double  val     = any value to be binned.
! integer         ntot    = total number of bins.
! integer         ibin    = bin number for the specified value.
! real or double  vinc    = bin increment (also bin width) (cannot be zero).
! real or double  vcenter = value for the center of the requested bin.
!
!
! Bin number  1   corresponds to the bin whose center is VINIT.
! Bin number NTOT corresponds to the bin whose center is VLAST.
!
! If VINC is positive, VLAST will be greater than VINIT.
! If VINC is negative, VLAST will be smaller than VINIT.
!
! Bin numbers less than 1 and greater than NTOT are allowed.
!
!
! The equations used are these:
!
!             vinc    = (vlast - vinit) / (ntot - 1)
!
!             ibin    = nint ((val   - vinit) / vinc) + 1
!             ntot    = nint ((vlast - vinit) / vinc) + 1
!
!             vcenter = vinit + (ibin - 1) * vinc
!             vlast   = vinit + (ntot - 1) * vinc
!
! If the above functions are not used, any binning code should be equivalent
! to the above equations.  In particular, note that the Fortran intrinsic
! NINT should be used, not INT.
!
! In MTH_BIN_INCREMENT: if NTOT is 0 or 1 then VINC will be set to 1.
! In MTH_BIN_NUMBER:    if VINC is 0 then VINC will be considered to be 1.
!
!-------------------------------------------------------------------------------
! Determine whether a value falls within a pattern:
!
!            o                      i    i     i     i    i
!         included = mth_included (val,vinit,vwidth,vinc,ntot)
!
! logical         included = true if VAL falls within the pattern.
! real or double  val      = any value to be tested.
! real or double  vinit    = value for the center of the first bin.
! real or double  vwidth   = bin width (cannot be zero or negative).
! real or double  vinc     = bin increment (cannot be zero).
! integer         ntot     = total number of bins.
!
! The pattern consists of NTOT bins having widths VWIDTH.
! The bins are centered at values VINIT + (i - 1) * VINC, where i = 1,...,NTOT.
! The absolute value of VINC should be greater than VWIDTH.
! The absolute value of VINC need not be a multiple of VWIDTH.
!
! The value is considered to fall within the pattern if it falls within any
! of the specified bins.
!
!-------------------------------------------------------------------------------
! Random number generators:
!                                          opt
!                                  i        i
!                call mth_ranset (seed, homegrown)
!
!                                        opt
!                  o                      i
!                random_x = mth_ranf (homegrown)
!
!                                                          opt
!                                      i     o     o        i
!                call mth_gauss_ranf (sdev, ranx, rany, homegrown)
!
!                                                   opt
!                  o                        i        i
!                random_x = mth_dexp_ranf (sdev, homegrown)
!
!
! real      random_x  =  pseudo random number.
! integer       seed  =  seed for random number generator.
! real          sdev  =  standard deviation for gaussian distribution.
! real          ranx  =  pseudo random value in X direction.
! real          rany  =  pseudo random value in Y direction.
! logical  homegrown  =  uses home-grown generator if present and true.
!
!
! MTH_RANSET:
!  This subroutine takes an integer argument and initializes the mth_ranf
!  random number generator seed to a known value, similar to the Cray RANSET
!  subroutine.
!
! MTH_RANF:
!  This function returns a psuedo random number in the range
!  0 <= mth_ranf < 1.0, similar to the Cray RANF function.
!
! MTH_GAUSS_RANF:
!  This subroutine returns the X and Y random values in a Gaussian distribution
!  of specified standard deviation.  It calls the MTH_RANF function.
!
! MTH_DEXP_RANF:
!  This function return a pseudo random number in a double exponential
!  distribution of specified standard deviation.  It calls the MTH_RANF
!  function.
!
! If the optional logical argument HOMEGROWN is absent or false, the
! Fortran-90 routines RANDOM_SEED and RANDOM_NUMBER are used.
!
! If the optional logical argument HOMEGROWN is present and true, a
! platform-independent home-grown random number generator is used.
!
!-------------------------------------------------------------------------------
! To add an array to a cumulative array:
!
!                            i i     b
!              CALL MTH_ADD (X,N,   XSUM)
!
! real    X(N)    = array to be added to XSUM(N).
! integer N       = number of elements in X array.
! real    XSUM(N) = cumulative array to be added to.
!
! XSUM(N) should first be cleared.  Then MTH_ADD can be called
! repeatedly to add arrays to XSUM(N).
!
!           same as:     xsum(1:n) = xsum(1:n) + x(1:n)
!
!-------------------------------------------------------------------------------
! To get the total power or amplitude of one or more arrays:
!
!                                  i i     b
!              CALL MTH_POWER     (X,N,   POWER)
!              CALL MTH_AMPLITUDE (X,N,   AMP)
!
! real    X(N)  = array whose power or amplitude is to be added to POWER or AMP.
! integer N     = number of elements in X array.
! real    POWER = total power (sum of squares of array elements).
! real    AMP   = total amplitude (sum of absolute values of array elements).
!
! POWER or AMP should first be set to zero.  Then MTH_POWER or MTH_AMPLITUDE
! can be called repeatedly to add the total power or amplitude of individual
! arrays to POWER or AMP.
!
!-------------------------------------------------------------------------------
! To power-normalize or amplitude-normalize an array:
!
!                                            i o i
!              CALL MTH_POWER_NORMALIZE     (A,B,N)
!              CALL MTH_AMPLITUDE_NORMALIZE (A,B,N)
!
! real    A(N)  = input array.
! real    B(N)  = output array.
! integer N     = number of elements in A and B arrays.
!
! These routines put a power-normalized or amplitude-normalized version of
! array A into array B.
!
! Power or amplitude normalizing is accomplished by dividing each array element
! by the average power or amplitude of the array elements.
!
! A(n) and B(n) can be same address if desired.
!
!-------------------------------------------------------------------------------
! To get X and Y indices, and a sequential index, from values:
!
!                              i  i    i   i
!        CALL MTH_GET_INDICES (X,XMIN,XINC,NX,
!                              Y,YMIN,YINC,NY,  IX,IY,INDEX)
!                              i  i    i   i    o  o    o
!
!                                          i  i    i   i
!        CALL MTH_GET_CONSTRAINED_INDICES (X,XMIN,XINC,NX,
!                                          Y,YMIN,YINC,NY,  IX,IY,INDEX)
!                                          i  i    i   i    o  o    o
!
! real    X    = value in X direction.
! real    XMIN = value in X direction corresponding to IX = 1.
! real    XINC = increment in value in X direction from one index to the next.
! integer NX   = number of indices in X direction.
!
! real    Y    = value in Y direction.
! real    YMIN = value in Y direction corresponding to IY = 1.
! real    YINC = increment in value in Y direction from one index to the next.
! integer NY   = number of indices in Y direction.
!
! integer IX    = index in X direction corresponding to value X.
! integer IY    = index in Y direction corresponding to value Y.
! integer INDEX = sequential index which ranges from 1 through NX*NY.
!
! As the sequential index changes, the X index changes fastest.
! For example:
! For INDEX between  1   and  NX , IX will = INDEX    and IY will = 1.
! For INDEX between NX+1 and 2*NX, IX will = INDEX-NX and IY will = 2.
!
! XINC and YINC must not be zero.
! The value of X should be between XMIN and XMIN+(NX-1)/XINC.
! The value of Y should be between YMIN and YMIN+(NY-1)/YINC.
!
! If X or Y is out of range:
!   - the corresponding values of IX or IY will be less than 1 or
!       greater than NX or NY.
!   - the value of INDEX will be set to 0 to make it easy to test for
!       this condition.
!
! MTH_GET_CONSTRAINED_INDICES is the same except that valid indices are
! always returned, even if X or Y is out of range.
!
!-------------------------------------------------------------------------------
! To split a sequential index into X and Y indices:
!
!                                       i   i     o  o
!               CALL MTH_SPLIT_INDEX (INDEX,NX,   IX,IY)
!
! integer INDEX = sequential index which ranges from 1 through NX*NY.
! integer NX    = number of indices in X direction.
! integer NY    = number of indices in Y direction (not needed in arguments).
! integer IX    = index in X direction (ranging from 1 through NX).
! integer IY    = index in Y direction (ranging from 1 through NY).
!
! As the sequential index changes, the X index changes fastest.
! For example:
! For INDEX between  1   and  NX , IX will = INDEX    and IY will = 1.
! For INDEX between NX+1 and 2*NX, IX will = INDEX-NX and IY will = 2.
!
!-------------------------------------------------------------------------------
! To constrain a value between limits:
!                                                    opt
!                                       b  i    i     i
!               CALL MTH_CONSTRAIN     (X,XMIN,XMAX,XSTEP)
!               CALL MTH_CONSTRAIN_ODD (X,XMIN,XMAX)
!
! (any type) X    = value to adjust if necessary to lie between XMIN and
!                     XMAX inclusively.
! (any type) XMIN  = minimum (or maximum) allowed value for X.
! (any type) XMAX  = maximum (or minimum) allowed value for X.
! (any type) XSTEP = required step increment for X.
!
! The types of X,XMIN,XMAX,XSTEP can be INTEGER or REAL or DOUBLE PRECISION,
! but must match each other in a single subroutine call.
!
! Both functions constrain X to be between XMIN and XMAX inclusively.
! Note that XMIN and XMAX are interchangeable.
!
! In the first function, X is first constrained to be between XMIN and XMAX
! inclusively.  Then, if XSTEP is present and non-zero, X is adjusted up or
! down slightly so that (X-XMIN) will be an integral multiple of XSTEP.
!
! The second function allows only integer types for X, XMIN, and XMAX.
! This function first constrains X to be between XMIN and XMAX inclusively.
! Then, if X is greater than zero and even, it is incremented by one.
! For example, if you require X to be >= 1 and odd, XMIN should be 1, but
! if you require X to be either 0 or >= 1 and odd, XMIN should be 0.
!
!-------------------------------------------------------------------------------
! To adjust an increment to be valid:
!
!                                    i    b    b     i       i
!           CALL MTH_FIX_INCREMENT (XMIN,XMAX,XINC,XINCDEF,XINCMAX)
!
! real xmin    = minimum value.
! real xmax    = maximum value (might be adjusted slightly).
! real xinc    = increment (might be adjusted).
! real xincdef = default increment (>0).
! real xincmax = maximum allowed value of increment (>XINCDEF).
!
! If XINC is <= zero, it is reset to XINCDEF.
! If XINC is >  zero, it is reset if necessary so as not to exceed XINCMAX.
! If XMAX is <  XMIN, it is reset to XMIN.
!
!-------------------------------------------------------------------------------
! To adjust a pattern to be valid:
!
!                              i    b    b    b       i         i
!       call mth_fix_pattern (xmin,xinc,xmax,ntot,xmax_keep,ntot_keep,
!                               xincmin,xincmax,xincdef,ntotmin)
!                                  i       i       i       i
!                                 opt     opt     opt     opt
!
! real    xmin      = minimum value.
! real    xinc      = increment (positive or negative) (might be adjusted).
! real    xmax      = maximum value                    (might be adjusted).
! integer ntot      = total number of values           (might be adjusted).
! real    xmax_keep = previous value of XMAX.
! integer ntot_keep = previous value of NTOT.
! real    xincmin   = minimum value for XINC -- default value is 0.0.
! real    xincmax   = maximum value for XINC -- default value is 999999.0.
! real    xincdef   = default value for XINC -- default value is 1.0.
! integer ntotmin   = minimum allowed value of NTOT -- default value is 1.
!
! The algorithm takes the following steps in order:
!  (1) XINC is constrained to lie between XINCMIN and XINCMAX.
!  (2) If XINC == 0, it is reset to XINCDEF (or to 1 if XINCDEF is 0).
!  (3) If NTOT == NTOT_KEEP and XMAX /= XMAX_KEEP, NTOT is recalculated
!       from XMIN, XINC, and XMAX.
!  (4) If NTOT < NTOTMIN, NTOT is reset to NTOTMIN (or 0 if NTOTMIN < 0).
!  (5) XMAX is recalculated from XMIN, XINC, and NTOT.
!
!  Note that XINCMIN and XINCMAX can be positive or negative.
!  Note that XINCMIN and XINCMAX are interchangeable.
!  If XINC is to be constrained to only negative or only positive numbers,
!  one of the values of XINCMIN and XINCMAX must be zero.
!
! The above algorithm has the following effects:
!  (1) It makes sure than XMIN, XINC, XMAX, and NTOT are consistent.
!  (2) It makes sure that NTOT >= maximum of NTOTMIN and 0.
!  (3) It makes sure that XINC is not 0.0.
!  (4) It makes sure that XMAX >= XMIN if XINC > 0.0.
!  (5) It makes sure that XMAX <= XMIN if XINC < 0.0.
!  (6) If XMAX changes but NTOT does not change, NTOT is recalculated from XMAX.
!  (7) If NTOT changes but XMAX does not change, XMAX is recalculated from NTOT.
!  (8) If both XMAX and NTOT change, XMAX is recalculated from NTOT.
!
! To use this subroutine, XMAX_KEEP and NTOT_KEEP should be set to the values
! of XMAX and NTOT before the values of XMAX and NTOT are changed by calls
! to PC_GET.  Then this subroutine should be called after the calls to PC_GET.
!
!-------------------------------------------------------------------------------
! To divide two reals with clipped result:
!
!               o                        i      i     i
!               r  =  mth_divide_clip (numer, denom, clip)
!
! real  numer      = numerator.
! real  denom      = denominator.
! real  clip       = clip value (must be non-zero).
!
! real  r (result) = numer/denom  if  abs(numer/denom) < abs(clip)  or
!                    sign(numer/denom)*abs(clip)  otherwise (but see below).
!
! Returns  r = sign(numer)*abs(clip)  if  denom = 0  and  numer /= 0.
! Returns  r = 0  if numer and denom are both 0 and also in an anomalous
! case where numer and denom are both very small.
!
! Above function reference should replace the subroutine calls
!            CALL xxxxDIVM (R, NUMER, DENOM, CLIP)
! in any of Bill Harlan's processes, where xxxxDIVM may be (for example)
! RMULDIVM, MDIPDIVM, etc.
!
!-------------------------------------------------------------------------------
!</calling_doc>
                                                                          

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
! 18. 2004-03-15  Stoeckley  Fix bug in mth_constrain whereby the returned
!                             value might occasionally be slightly greater than
!                             the maximum requested value when a step increment
!                             is  provided.
! 17. 2002-08-12  Stoeckley  Make the binning functions more robust to keep
!                             from dividing by zero.
! 16. 2002-07-18  C C Burch  Added a version of Park-Miller-Schrage Minimum
!                            Standard random number generator.
! 15. 2002-07-16  Stoeckley  Add function mth_included.
! 14. 2001-12-10  Stoeckley  Add optional argument XSTEP to MTH_CONSTRAIN.
! 13. 2001-05-14  Stoeckley  Make all routines default public.
! 12. 2001-05-03  Stoeckley  Add overloaded double precision binning functions.
! 11. 2001-04-26  Stoeckley  Added binning functions for standardization.
! 10. 2000-09-27  Stoeckley  Added mth_get_constrained_indices.
!  9. 2000-05-08  Stoeckley  Added mth_amplitude, mth_power_normalize,
!                             mth_amplitude_normalize, and mth_constrain_odd.
!  8. 2000-04-07  Stoeckley  Added new options to mth_ranset and mth_ranf
!                             (from RNSYN); added new routines mth_gauss_ranf
!                             (from RNSYN and ADNS), mth_dexp_ranf (from ADNS),
!                             and mth_fix_pattern; improve flexibility of
!                             mth_constrain; add mth_ameq for double precision.
!  7. 2000-03-17  Baumel     Correct documentation for mth_divide_clip (is a
!                            real function, not integer function) - no change
!                            to code.
!  6. 2000-01-24  Baumel     Added mth_divide_clip (from Harlan's DIVM).
!  5. 1999-11-23  Stoeckley  Added mth_search_equal and mth_search_unequal
!                             (from Cray routines isrcheq and isrchne plus
!                             additional variable types); add mth_add,
!                             mth_power, mth_get_indices, mth_split_index,
!                             mth_constrain, and mth_fix_increment (from the
!                             old TOOLS primitive).
!  4. 1999-09-13  Selzler    Added ismax, ismin, icamin.
!  3. 1999-09-02  Selzler    Added comparison routines (ameq and compare)
!  2. 1999-08-27  Selzler    Added random number generators (ranf and ranset)
!  1. 1999-08-20  Dorman     Initial version. New implementation of        
!                             CrayLibs 2.0 functions in Fortran 90.        
!                                                                          
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
                                                                             
  module mth_module                                                         
    use named_constants_module
    implicit none                                                          
    public                                                              
 
    character(len=100),public,save :: MTH_IDENT = &
'$Id: mth.f90,v 1.18 2004/03/15 12:36:13 Stoeckley prod sps $'

    interface mth_ameq
      module procedure mth_ameq_single
      module procedure mth_ameq_double
    end interface

    interface mth_compare
      module procedure mth_compare_single
      module procedure mth_compare_double
    end interface

    interface mth_search_equal
      module procedure mth_search_equal_integer
      module procedure mth_search_equal_real
      module procedure mth_search_equal_double
      module procedure mth_search_equal_char
    end interface

    interface mth_search_unequal
      module procedure mth_search_unequal_integer
      module procedure mth_search_unequal_real
      module procedure mth_search_unequal_double
      module procedure mth_search_unequal_char
    end interface

    interface mth_constrain
      module procedure mth_constrain_integer
      module procedure mth_constrain_real
      module procedure mth_constrain_double
    end interface

    interface mth_bin_increment
      module procedure mth_bin_increment_real
      module procedure mth_bin_increment_double
    end interface

    interface mth_bin_number
      module procedure mth_bin_number_real
      module procedure mth_bin_number_double
    end interface

    interface mth_bin_center
      module procedure mth_bin_center_real
      module procedure mth_bin_center_double
    end interface

    interface mth_included
      module procedure mth_included_real
      module procedure mth_included_double
    end interface


    interface mth_random_number
      module procedure mth_random_number_i
      module procedure mth_random_number_f
    end interface

    interface 
      function mth_random_number_c(seed, max) result(ran)
        integer, intent(inout) :: seed
        integer, intent(in)    :: max
        integer                :: ran
      end function mth_random_number_c

      function mth_random_numberf_c(seed) result(ran)
        integer, intent(inout) :: seed
        real                   :: ran
      end function mth_random_numberf_c
    end interface

    contains                                                         
                                                                    
!!----------------------------- mth_ismax -------------------------------!! 
!!----------------------------- mth_ismax -------------------------------!!
!!----------------------------- mth_ismax -------------------------------!!
                                                                          
    integer function mth_ismax (n, x, incx)                              
      implicit none                                                     
      integer, intent(in) :: n, incx                                   
      real, dimension(1:n), intent(in) :: x                           
      integer :: istart, istop, index, i                             
      if (n > 0 .and. incx /= 0)then                                
        if (incx < 0) then            ! if increment is negative,  
          istart = n                  ! then set up descending loop,    
          istop  = 1                  ! i.e., do i = n, 1, incx        
        else                          ! if increment is positive,     
          istart = 1                  ! then set up ascending loop,  
          istop  = n                  ! i.e., do i = 1, n, incx     
        endif                                                      
          index = istart                                          
        do i = istart, istop, incx    ! locate largest absolute value   
          if (i == istart) cycle                                       
          if (x(i) > x(index)) index = i                              
        end do                                                       
        mth_ismax = index                ! return loc of largest abs value 
      else                                                                
        mth_ismax = 0                    ! if n ,= 0 or incx = 0, return 0 
      endif                                                               
      return                                                             
    end function mth_ismax                                              
                                                                       
!!----------------------------- mth_ismin -------------------------------!!  
!!----------------------------- mth_ismin -------------------------------!! 
!!----------------------------- mth_ismin -------------------------------!! 

    integer function mth_ismin (n, x, incx)                                
      implicit none                                                       
      integer, intent(in) :: n, incx                                     
      real, dimension(1:n), intent(in) :: x                             
      integer :: istart, istop, index, i                               
      if (n > 0 .and. incx /= 0)then                                  
        if (incx < 0) then            ! if increment is negative,    
          istart = n                  ! then set up descending loop,   
          istop  = 1                  ! i.e., do i = n, 1, incx       
        else                          ! if increment is positive,    
          istart = 1                  ! then set up ascending loop, 
          istop  = n                  ! i.e., do i = 1, n, incx     
        endif                                                      
          index = istart                                          
        do i = istart, istop, incx    ! locate minimum absolute value       
          if (i == istart) cycle                                           
          if (x(i) < x(index)) index = i                              
        end do                                                       
        mth_ismin = index                ! return loc of minimum abs value   
      else                                                                  
        mth_ismin = 0                    ! if n ,= 0 or incx = 0, return 0  
      endif                                                                
      return                                                              
    end function mth_ismin                                               
                                                                        
!!----------------------------- mth_isamax -------------------------------!! 
!!----------------------------- mth_isamax -------------------------------!!
!!----------------------------- mth_isamax -------------------------------!! 
                                                                            
    integer function mth_isamax (n, x, incx)                               
      implicit none                                                       
      integer, intent(in) :: n, incx                                     
      real, dimension(1:n), intent(in) :: x                             
      integer :: istart, istop, index, i                               
      if (n > 0 .and. incx /= 0)then                                  
        if (incx < 0) then            ! if increment is negative,    
          istart = n                  ! then set up descending loop,  
          istop  = 1                  ! i.e., do i = n, 1, incx      
        else                          ! if increment is positive,   
          istart = 1                  ! then set up ascending loop,     
          istop  = n                  ! i.e., do i = 1, n, incx        
        endif                                                         
          index = istart                                             
        do i = istart, istop, incx    ! locate largest absolute value   
          if (i == istart) cycle                                       
          if (abs(x(i)) > abs(x(index))) index = i                    
        end do                                                       
        mth_isamax = index                ! return loc of largest abs value  
      else                                                                  
        mth_isamax = 0                    ! if n ,= 0 or incx = 0, return 0 
      endif                                                                
      return                                                              
    end function mth_isamax                                              
                                                                        
!!----------------------------- mth_isamin -------------------------------!!  
!!----------------------------- mth_isamin -------------------------------!! 
!!----------------------------- mth_isamin -------------------------------!! 

    integer function mth_isamin (n, x, incx)                                
      implicit none                                                        
      integer, intent(in) :: n, incx                                      
      real, dimension(1:n), intent(in) :: x                              
      integer :: istart, istop, index, i                                
      if (n > 0 .and. incx /= 0)then                                   
        if (incx < 0) then            ! if increment is negative,     
          istart = n                  ! then set up descending loop, 
          istop  = 1                  ! i.e., do i = n, 1, incx     
        else                          ! if increment is positive,  
          istart = 1                  ! then set up ascending loop,         
          istop  = n                  ! i.e., do i = 1, n, incx            
        endif                                                             
          index = istart                                                 
        do i = istart, istop, incx    ! locate minimum absolute value   
          if (i == istart) cycle                                       
          if (abs(x(i)) < abs(x(index))) index = i                    
        end do                                                       
        mth_isamin = index                ! return loc of minimum abs value  
      else                                                                  
        mth_isamin = 0                    ! if n ,= 0 or incx = 0, return 0
      endif                                                               
      return                                                             
    end function mth_isamin                                             
                                                                       
!!----------------------------- mth_icamax -------------------------------!! 
!!----------------------------- mth_icamax -------------------------------!!
!!----------------------------- mth_icamax -------------------------------!!

    integer function mth_icamax (n, c, incc)                               
      implicit none                                                       
      integer, intent(in) :: n, incc                                     
      complex, dimension(1:n), intent(in) :: c                          
      integer :: istart, istop, index, i                               
      if (n > 0 .and. incc /= 0)then                                  
        if (incc < 0) then            ! if increment is negative,    
          istart = n                  ! then set up descending loop,
          istop  = 1                  ! i.e., do i = n, 1, incc    
        else                          ! if increment is positive, 
          istart = 1                  ! then set up ascending loop,       
          istop  = n                  ! i.e., do i = 1, n, incc          
        endif                                                           
          index = istart                                               
        do i = istart, istop, incc    ! locate largest abs value      
          if (i == istart) cycle                                     
          if (abs(c(i)) > abs(c(index))) index = i                  
        end do                                                     
        mth_icamax = index                ! return loc of largest abs value
      else                                                                
        mth_icamax = 0                    ! if n <= 0 or incc = 0, return 0  
      endif                                                                 
      return                                                               
    end function mth_icamax                                               

!!----------------------------- mth_icamin -------------------------------!!  
!!----------------------------- mth_icamin -------------------------------!! 
!!----------------------------- mth_icamin -------------------------------!!

    integer function mth_icamin (n, c, incc)                               
      implicit none                                                       
      integer, intent(in) :: n, incc                                     
      complex, dimension(1:n), intent(in) :: c                          
      integer :: istart, istop, index, i                               
      if (n > 0 .and. incc /= 0)then                                  
        if (incc < 0) then            ! if increment is negative,    
          istart = n                  ! then set up descending loop, 
          istop  = 1                  ! i.e., do i = n, 1, incc     
        else                          ! if increment is positive,  
          istart = 1                  ! then set up ascending loop,       
          istop  = n                  ! i.e., do i = 1, n, incc          
        endif                                                           
          index = istart                                               
        do i = istart, istop, incc    ! locate largest abs value      
          if (i == istart) cycle                                     
          if (abs(c(i)) < abs(c(index))) index = i                  
        end do                                                     
        mth_icamin = index                ! return loc of largest abs value  
      else                                                                  
        mth_icamin = 0                    ! if n <= 0 or incc = 0, return 0 
      endif                                                                
      return                                                              
    end function mth_icamin                                              


!!-------------------------- binning functions ---------------------------!!
!!-------------------------- binning functions ---------------------------!!
!!-------------------------- binning functions ---------------------------!!


    function mth_bin_increment_real (vinit,vlast,ntot) result (vinc)
      implicit none                                                       
      real            ,intent(in) :: vinit,vlast           ! arguments
      integer         ,intent(in) :: ntot                  ! arguments
      real                        :: vinc                  ! result

      if (ntot <= 1) then
           vinc = 1.0            ! otherwise vinc would be undefined.
      else
           vinc = (vlast - vinit) / (ntot - 1)
      end if
      return                                                              
    end function mth_bin_increment_real



    function mth_bin_increment_double (vinit,vlast,ntot) result (vinc)
      implicit none                                                       
      double precision,intent(in) :: vinit,vlast           ! arguments
      integer         ,intent(in) :: ntot                  ! arguments
      double precision            :: vinc                  ! result

      if (ntot <= 1) then
           vinc = 1.0            ! otherwise vinc would be undefined.
      else
           vinc = (vlast - vinit) / (ntot - 1)
      end if
      return                                                              
    end function mth_bin_increment_double


                            !!!!!!!!!!!!!!


    function mth_bin_number_real (vinit,vinc,val) result (ibin)
      implicit none                                                       
      real            ,intent(in) :: vinit,vinc,val        ! arguments
      integer                     :: ibin                  ! result

      if (vinc == 0.0) then
           ibin = nint(val - vinit) + 1        ! behave as if vinc == 1.
      else
           ibin = nint((val - vinit) / vinc) + 1
      end if
      return                                                              
    end function mth_bin_number_real



    function mth_bin_number_double (vinit,vinc,val) result (ibin)
      implicit none                                                       
      double precision,intent(in) :: vinit,vinc,val        ! arguments
      integer                     :: ibin                  ! result

      if (vinc == 0.0) then
           ibin = nint(val - vinit) + 1        ! behave as if vinc == 1.
      else
           ibin = nint((val - vinit) / vinc) + 1
      end if
      return                                                              
    end function mth_bin_number_double


                            !!!!!!!!!!!!!!


    function mth_bin_center_real (vinit,vinc,ibin) result (vcenter)
      implicit none                                                       
      real            ,intent(in) :: vinit,vinc            ! arguments
      integer         ,intent(in) :: ibin                  ! arguments
      real                        :: vcenter               ! result

      vcenter = vinit + (ibin - 1) * vinc
      return                                                              
    end function mth_bin_center_real



    function mth_bin_center_double (vinit,vinc,ibin) result (vcenter)
      implicit none                                                       
      double precision,intent(in) :: vinit,vinc            ! arguments
      integer         ,intent(in) :: ibin                  ! arguments
      double precision            :: vcenter               ! result

      vcenter = vinit + (ibin - 1) * vinc
      return                                                              
    end function mth_bin_center_double


!!----------------------------- mth_ranset -------------------------------!!  
!!----------------------------- mth_ranset -------------------------------!! 
!!----------------------------- mth_ranset -------------------------------!!


    subroutine mth_ranset (seed,homegrown)
      implicit none
      integer,intent(in)                   :: seed               ! argument
      logical,intent(in),optional          :: homegrown          ! argument
      integer,parameter                    :: MAX_ELEMENTS = 8   ! local
      integer,dimension(MAX_ELEMENTS),save :: f90_seeds = 0      ! local
      integer                              :: elements           ! local
      real                                 :: dummy              ! local

      if (present(homegrown)) then
           if (homegrown) then
                dummy = mth_private_homegrown(-(abs(seed)))
                return
           end if
      end if

      call random_seed(size = elements)
      f90_seeds(1) = seed
      call random_seed(put = f90_seeds(1:elements))
      return
    end subroutine mth_ranset


!!----------------------------- mth_ranf -----------------------------!!    
!!----------------------------- mth_ranf -----------------------------!!    
!!----------------------------- mth_ranf -----------------------------!!    


    function mth_ranf (homegrown)
      implicit none
      logical,intent(in),optional :: homegrown          ! argument
      real                        :: mth_ranf           ! result
 
      if (present(homegrown)) then
           if (homegrown) then
                mth_ranf = mth_private_homegrown(1)
                return
           end if
      end if

      call random_number(mth_ranf)
      return
    end function mth_ranf


!!------------------------- mth_gauss_ranf ---------------------------------!!
!!------------------------- mth_gauss_ranf ---------------------------------!!
!!------------------------- mth_gauss_ranf ---------------------------------!!

! moved from an algorithm in RNSYN_RAND on 2000-04-03.


      subroutine mth_gauss_ranf (sdev,ranx,rany,homegrown)
      implicit none
      real   ,intent(in)          :: sdev                   ! argument
      real   ,intent(out)         :: ranx,rany              ! argument
      logical,intent(in),optional :: homegrown              ! argument
      real                        :: rho,theta,value        ! local
      real,parameter              :: TWOPI = 2.0 * PI       ! local

      do
          value = mth_ranf(homegrown)
          if (value > 0.0) exit
      end do
      rho   = sdev * sqrt( 2.0 * abs(log(value)) )
      theta = TWOPI * mth_ranf(homegrown)
      ranx  = rho * cos(theta)
      rany  = rho * sin(theta)
      return
      end subroutine mth_gauss_ranf


!!------------------------- mth_dexp_ranf ---------------------------------!!
!!------------------------- mth_dexp_ranf ---------------------------------!!
!!------------------------- mth_dexp_ranf ---------------------------------!!

! moved from an algorithm in ADNS_RAND on 2000-04-03.


      function mth_dexp_ranf (sdev,homegrown)
      implicit none
      real   ,intent(in)          :: sdev                   ! argument
      logical,intent(in),optional :: homegrown              ! argument
      real                        :: mth_dexp_ranf          ! result
      real                        :: udif,fact              ! local

      udif = mth_ranf(homegrown) - 0.5
      if (abs(udif) < 0.5) then
           fact = sdev / sqrt (2.0)
           mth_dexp_ranf = sign(fact,udif) * log(1.0 - 2.0*abs(udif))
      else
           mth_dexp_ranf = 0.0
      end if
      return
      end function mth_dexp_ranf


!!--------------------------- mth_private_homegrown ------------------------!!
!!--------------------------- mth_private_homegrown ------------------------!!
!!--------------------------- mth_private_homegrown ------------------------!!

! moved without change from RNSYN_RAN3 on 2000-04-03.

      function mth_private_homegrown (idum) result (ran3)

      integer,intent(in):: idum                       ! argument
      real              :: ran3                       ! result

      integer,parameter :: mbig=1000000000            ! local
      integer,parameter :: mseed=1618033              ! local
      integer,parameter :: mz=0                       ! local
      real,parameter    :: fac = 1.0 / mbig           ! local

      integer           :: mj                         ! local
      integer           :: mk                         ! local
      integer           :: i                          ! local
      integer           :: ii                         ! local
      integer           :: k                          ! local

      integer,save      :: ma(55)                     ! local
      integer,save      :: inext                      ! local
      integer,save      :: inextp                     ! local
      integer,save      :: iff                        ! local

      data iff / 0 /

      if (idum < 0 .or. iff == 0) then
         iff = 1
         mj = mseed - iabs(idum)
         mj = mod(mj,mbig)
         ma(55) = mj
         mk = 1
         do i=1,54
            ii = mod(21*i,55)
            ma(ii) = mk
            mk = mj - mk
            if (mk < mz) mk = mk + mbig
            mj = ma(ii)
         enddo
         do k=1,4
            do i=1,55
               ma(i) =ma(i) - ma(1+mod(i+30,55))
               if (ma(i) < mz) ma(i) = ma(i) + mbig
            enddo
         enddo
         inext = 0
         inextp = 31
      endif
      inext = inext + 1
      if (inext == 56) inext = 1
      inextp = inextp + 1
      if (inextp == 56) inextp = 1
      mj = ma(inext) - ma(inextp)
      if (mj < mz) mj = mj + mbig
      ma(inext) = mj
      ran3 = mj * fac
      return
      end function mth_private_homegrown

!!-------------------------- mth_random_number_i  ----------------------------!!
!!-------------------------- mth_random_number_i  ----------------------------!!
!!-------------------------- mth_random_number_i  ----------------------------!!

      function mth_random_number_i(seed, max) result(ran)
        integer, intent(inout)   :: seed
        integer, intent(in)      :: max
        integer                  :: ran

        ran=mth_random_number_c(seed,max)
        return
      end function mth_random_number_i

!!-------------------------- mth_random_number_f  ----------------------------!!
!!-------------------------- mth_random_number_f  ----------------------------!!
!!-------------------------- mth_random_number_f  ----------------------------!!

      function mth_random_number_f(seed) result(ran)
        integer, intent(inout)   :: seed
        real                     :: ran

        ran=mth_random_numberf_c(seed)
        return
      end function mth_random_number_f
      

!!----------------------------- mth_ameq -----------------------------!!    
!!----------------------------- mth_ameq -----------------------------!!    
!!----------------------------- mth_ameq -----------------------------!!    


      function mth_ameq_single (VALUE1, VALUE2, EPSILON)
      implicit none
      real, intent(in) :: VALUE1, VALUE2, EPSILON
      logical :: mth_ameq_single

      if((VALUE1 > VALUE2 + EPSILON) .OR. &
         (VALUE1 < VALUE2 - EPSILON)) then
        mth_ameq_single = .FALSE.
      else
        mth_ameq_single = .TRUE.
      endif

      end function mth_ameq_single


      function mth_ameq_double (VALUE1, VALUE2, EPSILON)
      implicit none
      double precision, intent(in) :: VALUE1, VALUE2, EPSILON
      logical :: mth_ameq_double

      if((VALUE1 > VALUE2 + EPSILON) .OR. &
         (VALUE1 < VALUE2 - EPSILON)) then
        mth_ameq_double = .FALSE.
      else
        mth_ameq_double = .TRUE.
      endif

      end function mth_ameq_double


!!----------------------------- mth_compare -----------------------------!!    
!!----------------------------- mth_compare -----------------------------!!    
!!----------------------------- mth_compare -----------------------------!!    

! compare two float values (flt1, flt2) for approximate equality.
! return -1, flt1 is much less than flt2
!            Example: (1.99 << 2.00), (0.0 << 1.0e-23), (-1.0e-23 << 0.0)
! return  0, flt1 is approximately equal to flt2
!            Example: (1.99999999 == 2.00000000), (0.0 == 0.0)
! return +1, flt1 is much greater than flt2
!            Example: (2.00 >> 1.99), (1.0e-23 >> 0.0), (0.0 >> -1.0e-23)
!
! mth_compare is the generic name for mth_compare_single and mth_compare_double.
! See also "AMEQ" ('AlMost EQual').

      function mth_compare_single (flt1, flt2)
      implicit none
      integer          :: mth_compare_single   ! return value
      real, intent(in) :: flt1           ! argument
      real, intent(in) :: flt2           ! argument

      real :: delta ! local

      delta = 8 * radix(flt1) * abs(min(spacing(flt1), spacing(flt2)))

      if(flt1 < flt2 - delta) then
        mth_compare_single = -1  ! flt1 << flt2
      else if(flt1 > flt2 + delta) then
        mth_compare_single = +1  ! flt1 >> flt2
      else
        mth_compare_single = 0   ! flt1 == flt2 approximately
      end if

      return

      end function mth_compare_single

      function mth_compare_double (flt1, flt2)
      implicit none
      integer                      :: mth_compare_double  ! return value
      double precision, intent(in) :: flt1         ! argument
      double precision, intent(in) :: flt2         ! argument

      double precision :: delta ! local

      delta = 8 * radix(flt1) * abs(min(spacing(flt1), spacing(flt2)))

      if(flt1 < flt2 - delta) then
        mth_compare_double = -1  ! flt1 << flt2
      else if(flt1 > flt2 + delta) then
        mth_compare_double = +1  ! flt1 >> flt2
      else
        mth_compare_double = 0   ! flt1 == flt2 approximately
      end if

      return

      end function mth_compare_double


!!----------------------- mth search equal ---------------------------------!!
!!----------------------- mth search equal ---------------------------------!!
!!----------------------- mth search equal ---------------------------------!!


      function mth_search_equal_integer (n,array,inc,target) result (indx)
      implicit none
      integer,intent(in) :: n,inc                           ! argument
      integer,intent(in) :: array(n),target                 ! argument
      integer            :: indx                            ! result
      integer            :: istart,istop                    ! local

      if      (n <= 0)  then;  indx   = 0;  return
      else if (inc > 0) then;  istart = 1;  istop = n
      else if (inc < 0) then;  istart = n;  istop = 1
      else                  ;  indx   = 0;  return
      end if
      do indx = istart,istop,inc
           if (array(indx) == target) return
      end do
      indx = 0
      return
      end function mth_search_equal_integer


      function mth_search_equal_real (n,array,inc,target) result (indx)
      implicit none
      integer,intent(in) :: n,inc                           ! argument
      real   ,intent(in) :: array(n),target                 ! argument
      integer            :: indx                            ! result
      integer            :: istart,istop                    ! local

      if      (n <= 0)  then;  indx   = 0;  return
      else if (inc > 0) then;  istart = 1;  istop = n
      else if (inc < 0) then;  istart = n;  istop = 1
      else                  ;  indx   = 0;  return
      end if
      do indx = istart,istop,inc
           if (array(indx) == target) return
      end do
      indx = 0
      return
      end function mth_search_equal_real


      function mth_search_equal_double (n,array,inc,target) result (indx)
      implicit none
      integer         ,intent(in) :: n,inc                  ! argument
      double precision,intent(in) :: array(n),target        ! argument
      integer                     :: indx                   ! result
      integer                     :: istart,istop           ! local

      if      (n <= 0)  then;  indx   = 0;  return
      else if (inc > 0) then;  istart = 1;  istop = n
      else if (inc < 0) then;  istart = n;  istop = 1
      else                  ;  indx   = 0;  return
      end if
      do indx = istart,istop,inc
           if (array(indx) == target) return
      end do
      indx = 0
      return
      end function mth_search_equal_double


      function mth_search_equal_char (n,array,inc,target) result (indx)
      implicit none
      integer         ,intent(in) :: n,inc                  ! argument
      character(len=*),intent(in) :: array(n),target        ! argument
      integer                     :: indx                   ! result
      integer                     :: istart,istop           ! local

      if      (n <= 0)  then;  indx   = 0;  return
      else if (inc > 0) then;  istart = 1;  istop = n
      else if (inc < 0) then;  istart = n;  istop = 1
      else                  ;  indx   = 0;  return
      end if
      do indx = istart,istop,inc
           if (array(indx) == target) return
      end do
      indx = 0
      return
      end function mth_search_equal_char


!!----------------------- mth search unequal ------------------------------!!
!!----------------------- mth search unequal ------------------------------!!
!!----------------------- mth search unequal ------------------------------!!


      function mth_search_unequal_integer (n,array,inc,target) result (indx)
      implicit none
      integer,intent(in) :: n,inc                           ! argument
      integer,intent(in) :: array(n),target                 ! argument
      integer            :: indx                            ! result
      integer            :: istart,istop                    ! local

      if      (n <= 0)  then;  indx   = 0;  return
      else if (inc > 0) then;  istart = 1;  istop = n
      else if (inc < 0) then;  istart = n;  istop = 1
      else                  ;  indx   = 0;  return
      end if
      do indx = istart,istop,inc
           if (array(indx) /= target) return
      end do
      indx = 0
      return
      end function mth_search_unequal_integer


      function mth_search_unequal_real (n,array,inc,target) result (indx)
      implicit none
      integer,intent(in) :: n,inc                           ! argument
      real   ,intent(in) :: array(n),target                 ! argument
      integer            :: indx                            ! result
      integer            :: istart,istop                    ! local

      if      (n <= 0)  then;  indx   = 0;  return
      else if (inc > 0) then;  istart = 1;  istop = n
      else if (inc < 0) then;  istart = n;  istop = 1
      else                  ;  indx   = 0;  return
      end if
      do indx = istart,istop,inc
           if (array(indx) /= target) return
      end do
      indx = 0
      return
      end function mth_search_unequal_real


      function mth_search_unequal_double (n,array,inc,target) result (indx)
      implicit none
      integer         ,intent(in) :: n,inc                  ! argument
      double precision,intent(in) :: array(n),target        ! argument
      integer                     :: indx                   ! result
      integer                     :: istart,istop           ! local

      if      (n <= 0)  then;  indx   = 0;  return
      else if (inc > 0) then;  istart = 1;  istop = n
      else if (inc < 0) then;  istart = n;  istop = 1
      else                  ;  indx   = 0;  return
      end if
      do indx = istart,istop,inc
           if (array(indx) /= target) return
      end do
      indx = 0
      return
      end function mth_search_unequal_double


      function mth_search_unequal_char (n,array,inc,target) result (indx)
      implicit none
      integer         ,intent(in) :: n,inc                  ! argument
      character(len=*),intent(in) :: array(n),target        ! argument
      integer                     :: indx                   ! result
      integer                     :: istart,istop           ! local

      if      (n <= 0)  then;  indx   = 0;  return
      else if (inc > 0) then;  istart = 1;  istop = n
      else if (inc < 0) then;  istart = n;  istop = 1
      else                  ;  indx   = 0;  return
      end if
      do indx = istart,istop,inc
           if (array(indx) /= target) return
      end do
      indx = 0
      return
      end function mth_search_unequal_char


!!----------------------------- mth add --------------------------------!!
!!----------------------------- mth add --------------------------------!!
!!----------------------------- mth add --------------------------------!!


      SUBROUTINE MTH_ADD (X,N,   XSUM)
      implicit none
      integer,intent(in)    :: n                       ! arguments
      real   ,intent(in)    :: x(n)                    ! arguments
      real   ,intent(inout) :: xsum(n)                 ! arguments

      xsum(1:n) = xsum(1:n) + x(1:n)
      RETURN
      END SUBROUTINE MTH_ADD


!!----------------------------- mth power ------------------------------!!
!!----------------------------- mth power ------------------------------!!
!!----------------------------- mth power ------------------------------!!


      SUBROUTINE MTH_POWER (X,N,   POWER)
      implicit none
      integer,intent(in)    :: n                       ! arguments
      real   ,intent(in)    :: x(n)                    ! arguments
      real   ,intent(inout) :: power                   ! arguments
      integer               :: i                       ! local

      DO I = 1,N
         POWER = POWER + X(I)**2
      END DO
      RETURN
      END SUBROUTINE MTH_POWER


!!----------------------------- mth amplitude ------------------------------!!
!!----------------------------- mth amplitude ------------------------------!!
!!----------------------------- mth amplitude ------------------------------!!


      SUBROUTINE MTH_AMPLITUDE (X,N,   AMP)
      implicit none
      integer,intent(in)    :: n                       ! arguments
      real   ,intent(in)    :: x(n)                    ! arguments
      real   ,intent(inout) :: amp                     ! arguments
      integer               :: i                       ! local

      DO I = 1,N
         amp = amp + abs(x(I))
      END DO
      RETURN
      END SUBROUTINE MTH_AMPLITUDE


!!------------------------- mth power normalize ----------------------------!!
!!------------------------- mth power normalize ----------------------------!!
!!------------------------- mth power normalize ----------------------------!!


      subroutine mth_power_normalize (a,b,n)
      implicit none
      real   ,intent(in)  :: a(:)             ! arguments
      real   ,intent(out) :: b(:)             ! arguments
      integer,intent(in)  :: n                ! arguments
      real                :: power            ! local
      integer             :: i                ! local

      power = 0.0
      call mth_power (a,n,power)
      if (power == 0.0) then
           b(1:n) = 0.0
      else
           power = n/power
           do i = 1,n
                b(i) = power * a(i)
           end do
      end if
      return
      end subroutine mth_power_normalize


!!----------------------- mth amplitude normalize --------------------------!!
!!----------------------- mth amplitude normalize --------------------------!!
!!----------------------- mth amplitude normalize --------------------------!!


      subroutine mth_amplitude_normalize (a,b,n)
      implicit none
      real   ,intent(in)  :: a(:)             ! arguments
      real   ,intent(out) :: b(:)             ! arguments
      integer,intent(in)  :: n                ! arguments
      real                :: amp              ! local
      integer             :: i                ! local

      amp = 0.0
      call mth_amplitude (a,n,amp)
      if (amp == 0.0) then
           b(1:n) = 0.0
      else
           amp = n/amp
           do i = 1,n
                b(i) = amp * a(i)
           end do
      end if
      return
      end subroutine mth_amplitude_normalize


!!------------------- mth get constrained indices ------------------------!!
!!------------------- mth get constrained indices ------------------------!!
!!------------------- mth get constrained indices ------------------------!!


      SUBROUTINE MTH_GET_CONSTRAINED_INDICES (X,XMIN,XINC,NX,               &
                                              Y,YMIN,YINC,NY,  IX,IY,INDEX)
      implicit none
      real   ,intent(in)    :: x,xmin,xinc             ! arguments
      real   ,intent(in)    :: y,ymin,yinc             ! arguments
      integer,intent(in)    :: nx,ny                   ! arguments
      integer,intent(out)   :: ix,iy,index             ! arguments

      IX = NINT((X-XMIN)/XINC) + 1
      IY = NINT((Y-YMIN)/YINC) + 1
      call mth_constrain (ix,1,nx)
      call mth_constrain (iy,1,ny)
      INDEX = (IY-1)*NX + IX
      RETURN
      END SUBROUTINE MTH_GET_CONSTRAINED_INDICES


!!------------------------ mth get indices -----------------------------!!
!!------------------------ mth get indices -----------------------------!!
!!------------------------ mth get indices -----------------------------!!


      SUBROUTINE MTH_GET_INDICES (X,XMIN,XINC,NX,                  &
                                  Y,YMIN,YINC,NY,  IX,IY,INDEX)
      implicit none
      real   ,intent(in)    :: x,xmin,xinc             ! arguments
      real   ,intent(in)    :: y,ymin,yinc             ! arguments
      integer,intent(in)    :: nx,ny                   ! arguments
      integer,intent(out)   :: ix,iy,index             ! arguments

      IX = NINT((X-XMIN)/XINC) + 1
      IY = NINT((Y-YMIN)/YINC) + 1
      INDEX = 0
      IF (IX.LT.1.OR.IX.GT.NX) RETURN
      IF (IY.LT.1.OR.IY.GT.NY) RETURN
      INDEX = (IY-1)*NX + IX
      RETURN
      END SUBROUTINE MTH_GET_INDICES


!!------------------------- mth split index ----------------------------!!
!!------------------------- mth split index ----------------------------!!
!!------------------------- mth split index ----------------------------!!


      SUBROUTINE MTH_SPLIT_INDEX (INDEX,NX,   IX,IY)
      implicit none
      integer,intent(in)    :: index,nx                ! arguments
      integer,intent(out)   :: ix,iy                   ! arguments

      IY = (INDEX + NX - 1)/NX
      IX = INDEX - (IY-1)*NX
      RETURN
      END SUBROUTINE MTH_SPLIT_INDEX


!!------------------------- mth constrain ------------------------------!!
!!------------------------- mth constrain ------------------------------!!
!!------------------------- mth constrain ------------------------------!!


      SUBROUTINE MTH_CONSTRAIN_INTEGER (X,XMIN,XMAX,xstep)
      implicit none
      integer,intent(inout)       :: x                       ! arguments
      integer,intent(in)          :: xmin,xmax               ! arguments
      integer,intent(in),optional :: xstep                   ! arguments
      integer                     :: num                     ! arguments

      if (xmin < xmax) then
           X = MAX(X,XMIN)
           X = MIN(X,XMAX)
      else
           X = MAX(X,XMAX)
           X = MIN(X,XMIN)
      end if
      if (present(xstep)) then
      if (xstep /= 0) then
           num = nint(float(x-xmin)/xstep)
           x   = xmin + num * xstep
           if (x > max(xmin,xmax) .and. num >= 1) x = x - xstep
      end if
      end if
      RETURN
      END SUBROUTINE MTH_CONSTRAIN_INTEGER



      SUBROUTINE MTH_CONSTRAIN_REAL (X,XMIN,XMAX,xstep)
      implicit none
      real   ,intent(inout)       :: x                       ! arguments
      real   ,intent(in)          :: xmin,xmax               ! arguments
      real   ,intent(in),optional :: xstep                   ! arguments
      integer                     :: num                     ! arguments

      if (xmin < xmax) then
           X = MAX(X,XMIN)
           X = MIN(X,XMAX)
      else
           X = MAX(X,XMAX)
           X = MIN(X,XMIN)
      end if
      if (present(xstep)) then
      if (xstep /= 0.0) then
           num = nint((x-xmin)/xstep)
           x   = xmin + num * xstep
           if (x > max(xmin,xmax) .and. num >= 1) x = x - xstep
      end if
      end if
      RETURN
      END SUBROUTINE MTH_CONSTRAIN_REAL



      SUBROUTINE MTH_CONSTRAIN_DOUBLE (X,XMIN,XMAX,xstep)
      implicit none
      double precision,intent(inout)       :: x                  ! arguments
      double precision,intent(in)          :: xmin,xmax          ! arguments
      double precision,intent(in),optional :: xstep              ! arguments
      integer                              :: num                ! arguments

      if (xmin < xmax) then
           X = MAX(X,XMIN)
           X = MIN(X,XMAX)
      else
           X = MAX(X,XMAX)
           X = MIN(X,XMIN)
      end if
      if (present(xstep)) then
      if (xstep /= 0.0) then
           num = nint((x-xmin)/xstep)
           x   = xmin + num * xstep
           if (x > max(xmin,xmax) .and. num >= 1) x = x - xstep
      end if
      end if
      RETURN
      END SUBROUTINE MTH_CONSTRAIN_DOUBLE


!!-------------------------- mth constrain odd ---------------------------!!
!!-------------------------- mth constrain odd ---------------------------!!
!!-------------------------- mth constrain odd ---------------------------!!


      subroutine mth_constrain_odd (x, xmin, xmax)
      implicit none
      integer,intent(inout) :: x                  ! arguments
      integer,intent(in)    :: xmin,xmax          ! arguments

      call mth_constrain (x,xmin,xmax)
      if (x > 0 .and. x == 2*(x/2)) x = x + 1
      return
      end subroutine mth_constrain_odd


!!------------------------- mth fix increment ---------------------------!!
!!------------------------- mth fix increment ---------------------------!!
!!------------------------- mth fix increment ---------------------------!!


      SUBROUTINE MTH_FIX_INCREMENT (XMIN,XMAX,XINC,XINCDEF,XINCMAX)
      implicit none
      real   ,intent(in)    :: xmin,xincdef,xincmax    ! arguments
      real   ,intent(inout) :: xmax,xinc               ! arguments

      XMAX = MAX(XMAX,XMIN)
      XINC = MIN(XINC,XINCMAX)
      IF (XINC.LE.0.0) XINC = XINCDEF
      RETURN
      END SUBROUTINE MTH_FIX_INCREMENT


!!---------------------------- mth fix pattern ---------------------------!!
!!---------------------------- mth fix pattern ---------------------------!!
!!---------------------------- mth fix pattern ---------------------------!!


      subroutine mth_fix_pattern (xmin,xinc,xmax,ntot,xmax_keep,ntot_keep, &
                                  xincmin,xincmax,xincdef,ntotmin)
      implicit none
      real   ,intent(in)          :: xmin                ! arguments
      real   ,intent(inout)       :: xinc                ! arguments
      real   ,intent(inout)       :: xmax                ! arguments
      integer,intent(inout)       :: ntot                ! arguments
      real   ,intent(in)          :: xmax_keep           ! arguments
      integer,intent(in)          :: ntot_keep           ! arguments
      real   ,intent(in),optional :: xincmin             ! arguments
      real   ,intent(in),optional :: xincmax             ! arguments
      real   ,intent(in),optional :: xincdef             ! arguments
      integer,intent(in),optional :: ntotmin             ! arguments
      real                        :: xincmin2            ! local
      real                        :: xincmax2            ! local
      real                        :: xincdef2            ! local
      integer                     :: ntotmin2            ! local

      xincmin2  = 0.0
      xincmax2  = 999999.0
      xincdef2  = 1.0
      ntotmin2  = 1

      if (present(xincmin)) xincmin2 = xincmin
      if (present(xincmax)) xincmax2 = xincmax
      if (present(xincdef)) xincdef2 = xincdef
      if (present(ntotmin)) ntotmin2 = max(ntotmin,0)

      call mth_constrain (xinc,xincmin2,xincmax2)
      if (xinc == 0.0) xinc = xincdef2
      if (xinc == 0.0) xinc = 1.0

      if (ntot == ntot_keep .and. xmax /= xmax_keep) then
             ntot = nint((xmax - xmin)/xinc) + 1
      end if

      if (ntot < ntotmin2) ntot = ntotmin2
      xmax = xmin + xinc * (ntot - 1)
      return
      end subroutine mth_fix_pattern


!!-------------------------- mth divide clip ----------------------------!!
!!-------------------------- mth divide clip ----------------------------!!
!!-------------------------- mth divide clip ----------------------------!!


      function mth_divide_clip (numer, denom, clip)  result (r)
!
!     This was originally subroutine DIVM by Bill Harlan
!     -- called by many of Harlan's processes
!
      implicit none
      real            :: r
      real,intent(in) :: numer, denom, clip
!
      real            :: a, b, rmax      ! local variables
!
      a    = numer
      b    = denom
      rmax = abs(clip)
!
      do while (abs(a)>1.0e10 .and. abs(b)>1.0e10)
        a = a / 1.0e10
        b = b / 1.0e10
      end do
!
      if (a == 0.) then
        r = 0.
      else if (b == 0.) then
        if (a < 0.) then
          r = -rmax
        else
          r = rmax
        end if
      else if (abs(a) < rmax*abs(b)) then
        r = a / b
! ----- (following test will be true only if a and b are very small)
        if (abs(r) > rmax) r = 0.
      else
        if ((a>0. .and. b<0.) .or. (a<0. .and. b>0.)) then
          r = -rmax
        else
          r = rmax
        end if
      end if
!
      return
      end function mth_divide_clip


!!---------------------------- mth included ---------------------------------!!
!!---------------------------- mth included ---------------------------------!!
!!---------------------------- mth included ---------------------------------!!


      function mth_included_real (val,init,width,inc,num)  result (included)
      real   ,intent(in) :: val,init,width,inc               ! arguments
      integer,intent(in) :: num                              ! arguments
      logical            :: included                         ! result
      real               :: center,last                      ! local
      integer            :: i1,i2                            ! local

      included = .false.

      last = init + (num - 1) * inc
      i1 = nint((val - init) / inc)
      i2 = nint((val - last) / inc)
      if (i1 < 0 .or. i2 > 0) return

      center = init + i1 * inc
      if (nint((val - center) / width) /= 0) return

      included = .true.
      end function mth_included_real



      function mth_included_double (val,init,width,inc,num)  result (included)
      double precision,intent(in) :: val,init,width,inc         ! arguments
      integer         ,intent(in) :: num                        ! arguments
      logical                     :: included                   ! result
      double precision            :: center,last                ! local
      integer                     :: i1,i2                      ! local

      included = .false.

      last = init + (num - 1) * inc
      i1 = nint((val - init) / inc)
      i2 = nint((val - last) / inc)
      if (i1 < 0 .or. i2 > 0) return

      center = init + i1 * inc
      if (nint((val - center) / width) /= 0) return

      included = .true.
      end function mth_included_double


!!------------------------ mth included alternate ---------------------------!!
!!------------------------ mth included alternate ---------------------------!!
!!------------------------ mth included alternate ---------------------------!!


! This version of mth_included contains an algorithm written by Randy Selzler
! and used in the SELECT process.  Although this routine should be faster, it
! sometimes gives a different result when a value falls at the edge of a bin.
! Also, this routine does not work for negative increments as the above does.
! For these reasons this version is deliberately not documented and should not
! be used; it is here for test purposes only.  SELECT has been changed to call
! the above routine.

! Details: This routine returns true (and the above routine returns false)
! when the above routine evaluates nint(-0.5) to be -1.  This routine behaves
! as if nint(-0.5) is 0.


      function mth_included_alternate &
                        (val,init,width,inc,num)  result (included)
      real   ,intent(in) :: val,init,width,inc            ! arguments
      integer,intent(in) :: num                           ! arguments
      logical            :: included                      ! result
      real               :: p_start,p_scale,p_coord       ! local

      p_start = init - width / 2.0
      p_scale = (val - p_start) / inc
      p_coord = val - p_start - aint(p_scale) * inc

      if (p_scale < 0.0 .or. p_scale >= num  .or. &
          p_coord < 0.0 .or. p_coord >  width) then
        included = .false.    ! header and mask pattern do not intersect.
      else
        included = .true.     ! header and mask pattern intersect.
      end if
      end function mth_included_alternate


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

  end module mth_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
