function [snapshotn,snapshotm,z,x]=afd_snapnm(delx,delt,velocity,snap1,snap2,toutput,laplacian,boundary,wavelet)
% AFD_SNAPNM ... time steps a wavefield "n" steps
%
% [snapshotn,snapshotm,z,x]=afd_snapn(delx,delt,velocity,snap1,snap2,toutput,laplacian,boundary,wavelet)
%
% AFD_SNAP propogates a wavefield forward in depth to
% a desired output time.  Two input matrices of the wavefield, 
% one at time=0-delt and one at time=0, are used in a finite 
% difference algorithm to propogate the wavefield.  The 
% finite difference algorithm can be calculated with a 
% five or nine point approximation to the Laplacian operator.  
% The five point approximation is faster, but the nine 
% point results in a broader bandwidth.The snapshot of this 
% propagated wavefield is returned. Note that the velocity 
% and grid spacing must fulfill the equation
% max(velocity)*delt/delx > 0.7 for the model to be stable.  
% This condition usually results in snap1 and snap2 
% being identical. 
%
% delx = the horizontal AND vertical bin spacing in consistent units
% delt = time interval in seconds
% velocity = the input velocity matrix in consisnent units
%          = has a size of floor(zmax/delx)+1 by floor(xmax/delx)+1
% snap1 = the wavefield at time=0 - delt (same size as velocity matrix)
% snap2 = the wavefield at time = 0 (same size as velocity matrix
% toutput = the time in seconds at which the propagated wavefield will be returned
% laplacian - an option between two approximation to the laplacian operator
%           - 1 is a 5 point approximation
%           - 2 is a nine point approximation
% boundary = indicate whether all sides of the matrix are absorbing
%          = '1' indicates all four sides are absorbing
%          = '2' choses three sides to be absorbing, and the top one not to be
%             this enables sources to be put on the surface
% wavelet = wavelet specified as a time series. Must be sampled at delt
% ***** default = [1 0 0 0 0 ...] (an impulse) *******
%
% snapshotn = the image of the wavefield at the specified time
% snapshotm = the wavefield one timestep befor snapshotn.
% x ... x coordinate for the wavefields
% z ... z coordinate for the wavefields
% NOTE: Two snap shots are returned so that a wavefield can be
% incrementally propagated by calling afd_snapn repeatedly.
%
% by Carrie Youzwishen and Gary Margrave, February 1999
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
tic
[nx,nz]=size(snap1);
if(prod(double(size(snap1)~=size(snap2))))
	error('snap1 and snap2 must be the same size');
end
if(prod(double(size(snap1)~=size(velocity))))
	error('snap1 and velocity must be the same size');
end

if(nargin<9) wavelet=1; end

xmax=(nx-1)*delx;
zmax=(nz-1)*delx;

x=0:delx:xmax;
z=(0:delx:zmax)';

if laplacian ==1 

    if max(max(velocity))*delt/delx > 1/sqrt(2)
    disp('Model is unstable:  max(velocity)*delt/delx MUST BE < 1/sqrt(2)');
    return;
    end
else

    if max(max(velocity))*delt/delx > sqrt(3/8)
    disp('Model is unstable:  max(velocity)*delt/delx MUST BE < sqrt(3/8)');
    return;
    end
end

disp(['There are ' int2str(toutput/delt-1) ' steps to complete']);

sources=snap2;
snap2=wavelet(1)*sources;

for k=1:round(toutput/delt)

   [snapshotn]=afd_snap(delx,delt,velocity,snap1,snap2,laplacian,boundary);
   snap1=snap2;
   if(k+1<=length(wavelet))
       snap2=snapshotn+wavelet(k+1)*sources;
   else
       snap2=snapshotn;
   end
   
   if rem(k,10) == 0
           disp(['The wavefield has been propagated to ' num2str(k*delt) ' seconds']);
   end 

end
snapshotm=snap1;
toc