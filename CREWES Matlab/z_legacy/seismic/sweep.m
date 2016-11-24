function [s,t]=sweep(fmin,fmax,dt,tmax,taper)
% [s,t]= sweep(fmin,fmax,dt,tmax,taper)
% [s,t]=sweep(fmin,fmax,dt,tmax)
%
% SWEEP generates a linear synthetic Vibroseis sweep for the 
% specified passband. (After Waters, Reflection Seismology, 1981, page 90)
%
% fmin= minimum swept frequency in Hz
% fmax= maximum swept frequency in Hz
% dt= time sample rate in seconds
% tmax= sweep length in seconds
% taper= length of cosine sweep taper in seconds
% ********** default =.5 if tmax>4.0
%            otherwise =.25 *************
%
% s= output linear sweep
% t= output time coordinate vector 
%
% by G.F. Margrave, June 1991
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
 if nargin<5
   if tmax>4.0
    taper=.5;
   else
    taper=.25;
   end
 end
%
 t=0.:dt:tmax;
 b=(fmax-fmin)/tmax;
% 
 f=fmin+.5*b*t;
 theta=2.*pi*f.*t;
 s=sin(theta);
% apply taper
 percent=100.*taper/tmax;
 s=s.*mwindow(s,percent);
 