function drdt=drayveclin(t,r)
% DRAYVECLIN: compute the derivative of ray vector for v0=a*x+b*z
%
% drdt=drayveclin(t,r)
%
% This is the fundamental driver for v(x,z) raytracing.
% The velocity model is defined by first calling rayvelmod.
% t ... scalar giveg the current time
% r ... 4x1 column vector giving (x,z,p,q)
% drdt ... 4x1 column vector giving the time-derivative of r
%
%
% by G.F. Margrave, CREWES, June 2000
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

x=r(1);z=r(2);p=r(3);q=r(4);

drdt=zeros(size(r));


a0=1800;g=0.75;h=0;

alpha=a0+g*z+h*x;
drdt(1)=(alpha^2)*p;
drdt(2)=(alpha^2)*q;
drdt(3)=-h/alpha;
drdt(4)=-g/alpha;