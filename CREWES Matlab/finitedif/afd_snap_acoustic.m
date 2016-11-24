function [snapshot,z,x]=afd_snap_acoustic(delx,delt,velocity,density,snap1,snap2,laplacian,boundary)
% AFD_SNAP ... take one finite difference time step
%
% [snapshot,z,x]=afd_snap_acoustic(delx,delt,velocity,density,snap1,snap2,laplacian,boundary)
%
% AFD_SNAP_ACOUSTIC propogates a wavefield forward in depth by 
% one time step.  Two input matrices of the wavefield, one at 
% time=0-delt and one at time=0, are used in a finite 
% difference algorithm to propogate the wavefield.  The 
% finite difference algorithm can be calculated with a 
% five approximation to the Laplacian operator.  The snapshot
% of this propagated wavefield is returned. Note that the velocity 
% and grid spacing must fulfill the equation max(velocity)*delt/delx
% > 0.7 for the model to be stable. This condition usually results in
% snap1 and snap2 being identical. Current implementation of absorbing
% boundary conditions assumes no density contrast at boundary.
%
% delx = the horizontal AND vertical bin spacing in consistent units
% delt = time interval in seconds
% velocity = the input velocity matrix in consisnent units
%          = has a size of floor(zmax/delx)+1 by floor(xmax/delx)+1
% density = the input density matrix in consisnent units
%          = has a size of floor(zmax/delx)+1 by floor(xmax/delx)+1
% snap1 = the wavefield at time=0 - delt (same size as velocity matrix)
%        = will be based on the source array desired i.e. the position
%          of the sources will be one, and the rest of the positions
%          will be zero
% snap2 = the wavefield at time = 0 (same size as velocity matrix)
% laplacian = an option between two approximations to the laplacian operator
%           = 1 is a 5 point approximation
%           = 2 is a nine point approximation
% boundary = indicate whether all sides of the matrix are absorbing
%          = 0 indicates that no absorbing boundaries are desired
%          = 1 indicates all four sides are absorbing
%          = 2 choses three sides to be absorbing, and the top one not to be
%             this enables sources to be put on the surface
%
% snapshot = the wavefield propagated forward one time interval
%            where the time interval = delt
% 
% by Carrie Youzwishen, February 1999
% extended to full acoustic wave equation by Hugh Geiger, September 2003 
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

[nz,nx]=size(snap1);
if(prod(double(size(snap1)~=size(snap2))))
	error('snap1 and snap2 must be the same size');
end
xmax=(nx-1)*delx;
zmax=(nz-1)*delx;

x=0:delx:xmax;
z=(0:delx:zmax)';

snapshot=velocity.^2.*delt^2.*ders_5pt(snap2,density,delx) + 2*snap2 - snap1;
	
%prepare for absorbing bc's by zeroing outer 1 row and column
if boundary == 1
   snapshot(1,:)=zeros(1,nx);
   snapshot(nz,:)=zeros(1,nx);
   snapshot(:,1)=zeros(nz,1);
   snapshot(:,nx)=zeros(nz,1);
else
   %zero the first row of the wavefield for free surface
   % valid with boundary = 0 or 2
%   snapshot(1,:)=zeros(1,nx);
   snapshot(nz,:)=zeros(1,nx);
   snapshot(:,1)=zeros(nz,1);
   snapshot(:,nx)=zeros(nz,1);
end

if(boundary)
   [snapshot]=afd_bc_outer(delx,delt,velocity,snap1,snap2,snapshot,boundary);
end  