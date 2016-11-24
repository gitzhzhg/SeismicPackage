function [r,t]=reflec(tmax,dt,amp,m,n)
% REFLEC: synthetic pseudo random reflectivity
%
% [r,t]=reflec(tmax,dt,amp,m,n)
% [r,t]=reflec(tmax,dt,amp,m)
% [r,t]=reflec(tmax,dt,amp)
% [r,t]=reflec(tmax,dt)
%
% REFLEC creates a psuedo random reflectivity by first 
% generating a Gaussian random noise sequence and then raising
% each element in the sequence to a small integral power. This 
% has the effect of making the sequence more spiky.
%
% tmax = record length
% dt= sample rate
% amp= maximum rc;
% ************* default=.2 *******************
% m= exponentiation power to which gaussian distribution 
%    is raised; Should be an odd power to preserve the sign 
%    of the sample.
% ***************** default= 3 ***********************
% n= random number seed; 
% ************* defaults to a random number based on the
%                system clock  ***********
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

if(nargin<3)
  amp=.2;
 end
if(nargin<4)
 m=3;
end
if(nargin<5)
  c=clock;
  n=c(6);
end
m=round(m);
t=(0.:dt:tmax)';
% matlab random number generator screws up with a negative seed
%randn('seed',abs(n))
rng('default');
rng(abs(n));
if(floor(m/2)*2==m)
    tmp=randn(size(t));
    r=(tmp.^m).*sign(tmp);
else
    r=randn(size(t)).^m;
end
r=amp*r/max(abs(r));