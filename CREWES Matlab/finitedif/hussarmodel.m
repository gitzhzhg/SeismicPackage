function [vel,rho,x,z]=hussarmodel(dx)
% MARMOUSIMODEL ... return the 2D p-wave Marmousi model
%
% [vel,x,z]=hussarmodel(dx)
%
% dx ... must be either 2.5, 5 or 10. This specifies the grid size in both x
%       and z for the output model. 
% vel ... matrix of p-wave velocities
% rho ... matrix of densities
% x ... x (column) coordinate for vel
% z ... z (row) coordinate for vel
% 
% You can plot the result with
% figure;
% imagesc(x,z,vel);colorbar
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

%
if(nargin==0)
    [vel,rho,x,z]=hussarmodel(2.5);
    
    figure
    imagesc(x,z,vel);colorbar
    title('Hussar model, colors indicate velocity')
    xlabel('distance (m)')
    zlabel('depth (m)')
    
    clear
    return;
end



if(dx~=2.5 && dx~= 5 && dx~=10)
    error('only dx of 5, 10, and 25 are available')
end
vel=[];
rho=[];
x=[];
z=[];
s=which('afd_snap');
if(isempty(s))
    error('2.5 m Hussar model not found, you need to load and install the CREWES toolbox')
end
ind = strfind(s,'afd_snap');
sm=[s(1:ind-1) 'hussar_model_2p5'];
disp(['Hussar model loaded from ' sm])
load(sm)
if(dx==2.5)
  return;
elseif(dx==5)
  [nrows,ncols]=size(vel);
  vmin=min(vel(:));
  rhomin=min(rho(:));
  vele=extend(vel,1);
  vel2=conv2(vele,ones(3),'same')/9;
  vel=vel2(2:2:nrows+1,2:2:ncols+1);
  ind=find(vel<vmin);
  vel(ind)=vmin;
  rho2=conv2(rho,ones(3),'same')/9;
  rho=rho2(1:2:end,1:2:end);
  ind=find(rho<rhomin);
  rho(ind)=rhomin;
  x=x(1:2:end);
  z=z(1:2:end);
  disp('resampled to 5 m grid');
elseif(dx==10)
  [nrows,ncols]=size(vel);
  vmin=min(vel(:));
  rhomin=min(rho(:));
  vele=extend(vel,3);
  vel2=conv2(vele,ones(7),'same')/49;
  vel=vel2(4:4:nrows+3,4:4:ncols+3);
  ind=find(vel<vmin);
  vel(ind)=vmin;
  rho2=conv2(rho,ones(7),'same')/49;
  rho=rho2(1:4:end,1:4:end);
  ind=find(rho<rhomin);
  rho(ind)=rhomin;
  x=x(1:4:end);
  z=z(1:4:end);
  disp('resampled to 10 m grid');
end

function vele=extend(vel,n)
[nrows,ncols]=size(vel);
veltmp=[vel(:,ones(1,n)) vel vel(:,ncols*ones(1,n))];
vele=[veltmp(ones(n,1),:); veltmp; veltmp(nrows*ones(n,1),:)];