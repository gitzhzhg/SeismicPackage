function imp=rcs2impbl(rcs,t,fmin,fmax,delflow,delfhigh)
% imp=rcs2impbl(rcs,t,fmin,fmax,delflow,delfhigh)
%
% RCS2IMPBL takes a reflectivity vector specified in time and computes
% a corresponding impedence by integrating and exponentiating it.
% The integration is bandlimited (done by BLINT) so as not to generate
% significant power outside the band [fmin fmax]. The resultant impedance
% estimate is returned with mean removed.
%
% rcs... column vector of reflection coeficients
% t  ... column vector of times (note that rcs must be regularly sampled)
% znot ... initial impedance (at t=t(1)) estimate
% fmin ... minimum frequency to pass
% fmax ... maximum frequency to pass
% delflow ... gaussian rolloff width on low end
%         ******* default min([5 flow/5]) *******
% delfhigh ... gaussian rolloff width on high end
%         ******* default min([(fnyquist-fhigh)/10 10]) *******
%
% G.F. Margrave May 1995
%
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE
 %integrate the rcs
 if(nargin==6)
 	ircs=blint(rcs,t,fmin,fmax,delflow,delfhigh);
 elseif(nargin==5)
 	ircs=blint(rcs,t,fmin,fmax,delflow);
 else
 	ircs=blint(rcs,t,fmin,fmax);
 end
 %exponentiate
 imp= exp(2*ircs );
 imp=imp-mean(imp);