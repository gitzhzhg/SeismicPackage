function w = gwindow(n,percent)
% GWINDOW: creates a gwindow (Gaussian window with specified width)
%
% w = gwindow(n,percent)
% w = gwindow(n)
% 
% GWINDOW returns the n-point Gaussian window in a column vector. This
% window is a truncated Gaussian with a default width such that it is
% 2standard deviations down ast the edges If n is a vector, it is the same
% as gwindow(length(n)). The window maximum is 1.0 but this will only occur
% on a sample if n is odd.
%
% n= input length of the mwindow. If a vector, length(n) is
%    used
% percent= half-width (standard deviation) expressed as a percentage of the
%    window width. The default width means the Gaussian is two standard
%    deviations from the mean (center) at the truncation point. Should be
%    a number between 0 and 50.
%   ************* default=25 ************
%
% by G.F. Margrave, 2016
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
  
% set defaults
 if nargin<2
  percent=10;
 end
 if length(n)>1
   n=length(n);
 end
% compute the Hanning function 
 if(percent>50)||(percent<0)
   error(' invalid percent for gwindow')
 end
 m=2.*percent*n/100.;
 m=2*floor(m/2);
 %if n is odd then the Gaussian center is at point floor(n/2)+1. If even,
 %it is at n/2+.5 (between samples)
 if(iseven(n))
     n0=n/2+.5;
 else
     n0=floor(n/2)+1;
 end
 nn=(1:n)';
 w=exp(-(nn-n0).^2/m^2);
 
 