function [vrms,t]=vzmod2vrmsmod(vel,z,dt,tmax,flag)
%  VZMOD2VRMSMOD: Compute Vrms(x,t) from Vint(x,z)
%
%  [vrms,t]=vzmod2vrmsmod(vel,z,dt,tmax)
%
%  VZMOD2VRMSMOD converts an interval velocity model in depth (such as the
%  models required for finite difference modelling) into an rms velocity
%  model in time (such as is required for Kirchhoff time migration)
%
%  vel..........is the input velocity model in depth. Each row is a
%               constant depth.
%  z........depth coordinate for vel, length(z) = size(vel,1) 
%  dt.... desired time sample rate
%  tmax ... maximum two-way time desired
%  flag ... 1 ... extend the rms velocity to tmax by constant extrapolation
%           2 ... extend the interval velocity model to tmax by constant
%           extrapolation
%  ********** default flag = 1 ************
% NOTE: flag only matters if the maximum 2-way traveltime in the model is
% less than tmax. In that case, flag=1 extends the final vrms to tmax while
% flag=2 extends the final interval velocity to tmax and then computes
% vrms. The second option may seem more physical but can result in a very
% fast bottom layer.
%
%  vrms....is the output RMS velocity matrix in time
%  t....... output two-way time coordinate
%
%
%  Zoron Rodriguez, November 2006
%  G.F. Margrave, March 2013
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

if(nargin<4)
    error('not enough input arguments')
end
if(nargin<5)
    flag=1;
end
if(length(z)~=size(vel,1))
    error('vel and z sizes are not compatible')
end
nx=size(vel,2);
t=(0:dt:tmax);%two way time for output
vrms=zeros(length(t),nx);
for k=1:nx
   tv=2*vint2t(vel(:,k),z);%two way time at kth location
   if(tv(end)<tmax && flag==2)%extend the last interval velocity if needed
       tv(end)=tmax;
   end
   vrms(:,k)=vint2vrms(vel(:,k),tv,t);
   if(rem(k,100)==0)
       disp(['finished location ' int2str(k) ' of ' int2str(nx)])
   end
end