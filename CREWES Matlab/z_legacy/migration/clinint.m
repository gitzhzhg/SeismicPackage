function vint = clinint(f,v,fint)
% vint = clinint(f,v,fint)
%
% CLININT does complex linear interpolation of a function assumed to the
% the t->f transform of a time domain function defined from 0 to some tmax.
% In order to avoid spectral artifacts, the interpolation weights are 
% complex (i.e. have phase terms). These weights may be derived by 
% considering sinc function interpolation, which in the time domain is
% boxcar multiplication, and shifting the boxcar into the time range
% of 0->tmax.
%
% f    ... vector of frequency coordinates. Must be regularly sampled and
%	   in increasing order.
% v    ... vector of complex spectral values
% fint ... vector of interpolation frequencies. All fint must lie between
%	   f(1) and f(length(f).
% vint ... vector of complex spectral values interpolated linearly at the
% 	   frequencies fint.
%
% G.F. Margrave and J. Bancroft, CREWES Project, U of Calgary, 1996
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
df=f(2)-f(1);
i1 = floor( fint/df) +1;
int = fint/df+1;
del = (int-i1);
v1 = v(i1).*exp(-i*pi*(del));
v2=v(i1+1).*exp(i*pi*(1-del));
vint = v1 + del.*(v2-v1);