function w = mwhalf(n,percent)
% MWHALF: half an mwindow (boxcar with raised-cosine taper on one end)
%
% mwhalf(n,percent)
% mwhalf(n)
% 
% MWHALF returns the N-point half-Margrave window in a 
% row vector. This window is a boxcar over the first samples,
% (100-percent)*n/100 in number, while it has a raised cosine
% (hanning style) taper on the end. If n is a vector, it is
% the same as mwindow(length(n)
%
% n= input length of the mwindow. If a vector, length(n) is
%    used
% percent= percent taper on the ends of the window
%   ************* default=10 ************
%
% by G.F. Margrave, May 1991
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
 if(percent>100)||(percent<0)
   error(' invalid percent for mwhalf')
 end
 m=floor(percent*n/100);
 h=han(2*m);
 h=h(:);
 w = [ones(n-m,1);h(m:-1:1)];
 
function w=han(n)

xint=2*pi/(n+1);
x=xint*(1:n)-pi;

w=.5*(1+cos(x))';