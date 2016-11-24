function w = mwindow2(n,percent,ic)
% MWINDOW2: creates an mwindow (boxcar with raised-cosine tapers) with causality awareness
%
% w = mwindow2(n,percent,ic)
% 
% MWINDOW2 returns the N-point Margrave window in a column vector. This window is a boxcar over
% the central samples round((100-2*percent)*n/100) in number, while it has a raised cosine
% (hanning style) taper on each end. If n is a vector, it is the same as mwindow(length(n))
%
% n= input length of the mwindow. If a vector, length(n) is
%    used
% percent= percent taper on the ends of the window
%   ************* default=10 ************
% ic= causality factor. 0 for a symmetric window, tapers on both ends, 1 for a causal window
%       with a taper only at the trailing end. A value between 1 and 50 gives an increasingly
%       large taper on the front with the same taper on the trainling end. ic=50 is the same as
%       ic=0.
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
 if(ic==0)
     ic=50;
 end
 if(ic<1 || ic>50)
     error('invalid value for ic');
 end
% compute the Hanning function 
 if(percent>50)||(percent<0)
   error(' invalid percent for mwindow')
 end
 m=2.*percent*n/100.;
 m=2*floor(m/2);
 h=han(m);
 h1=h(1:m/2);
 h2=h(end-m/2+1:end);
 m2=n-m/2;
 m1=ceil(ic*m/100);
 if(m1>1)
    w = [h1(end-m1+1:end);ones(m2-m1,1);h2];
 else
    w = [ones(m2,1);h2]; 
 end
 
function w=han(n)

xint=2*pi/(n+1);
x=xint*(0:n+1)-pi;

w=.5*(1+cos(x))';