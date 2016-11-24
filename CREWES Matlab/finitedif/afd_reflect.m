function r=afd_reflect(velocity,clipn)
% AFD_REFLECT ... calculate the reflectivity from a velocity model
%
% r=afd_reflect(velocity,clipn)
% 
% AFD_REFLECT calculates the normal indicence reflectivity of an input
% velocity model for use with AFD_EXPLOAD.  
%
% velocity = velocity matrix
% clipn = the number of "bin layers" with which to remove from the
%       edge of velocity matrix in order to prevent artifacts from
%       the absorbing boundary conditions
%     = the suggested number is 5, but zero may also be chosen
% r = refelctivity calculated as .5*abs(gradient(log(velocity)))
%
% Carrie Youzwishen, April 1999
% completely rewritten by G.F. Margrave August 2000
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

% recalculate velocity for exploding reflector model (1 way travel time)

[nz,nx]=size(velocity);

[rx,rz]=gradient(log(velocity));

tmp=.5*sign(rz).*sqrt(rx.^2+rz.^2);
clear rx rz

r=zeros(size(tmp));
r(clipn+1:nz-clipn,clipn+1:nx-clipn)=tmp(clipn+1:nz-clipn,clipn+1:nx-clipn);





%create reflectivity matrix
%vnew=zeros(nz+2,nx);
%r=zeros(nz,nx);

%vnew(2:nz+1,:) = 1/2*log(velocity);
%vnew(1,:)=vnew(2,:);
%vnew(nz+2,:)=vnew(nz+1,:);

%temp(:,:)=(vnew(3:nz+2,:) - vnew(1:nz,:))/2;
%r(clipn+1:nz-clipn,clipn+1:nx-clipn)=temp(clipn+1:nz-clipn,clipn+1:nx-clipn);