function velsmo=velsmooth(vel,x,z,hw)
% VELSMOOTH: smooth a velocity model by conv2 with a Gaussian
%
% velsmo=velsmooth(vel,x,z,hw)
%
% Function smooths a velocity model by convolving it with a 2D Gaussian.
% The smoothing is actually done by convolving the Gaussian with slowness
% (1./vel) and then inverting the result. This has the effect of roughly
% conserving traveltime.
% The Gaussian is specified with a certain halfwidth and is truncated at
% two halfwidths. The velocity model is extended for two halfwidth's in
% every direction (constant extrapolation) to avoid edge effects. After
% convolution it is truncated to the original size
% vel ... 2D velocity matrix
% x ... vector of x coordinates for the velocity model. Length must be the
%       same as the number of columns of vel. Alternativly, this may be
%       specified as a single scalar in which case it is the horizontal
%       grid size
% z ... similar to x but for depth coordinates
% hw ... half-width of the Gaussian smoother in consistent spatial units
% velsmo ... smoothed velocity matrix the same size as vel
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

[nz,nx]=size(vel);
if(length(x)==1)
    dx=x;
    x=(0:nx-1)*dx;
else
    if(length(x)~=nx)
        error('x is the wrong size')
    end
    x=x(:)';
    dx=x(2)-x(1);
end
if(length(z)==1)
    dz=z;
    z=(0:nz-1)*dz;
    z=z';
else
    if(length(z)~=nz)
        error('z is the wrong size')
    end
    z=z(:);
    dz=z(2)-z(1);
end

%make a 2D Gaussian the hw wide
ngx2=round(3*hw/dx);
xg=(-ngx2:ngx2)*dx;
ngz2=round(3*hw/dz);
zg=((-ngx2:ngx2)*dx)';
sq_dist_from_cntr=(ones(size(zg))*xg).^2 + (zg*ones(size(xg))).^2;
gauss=exp(-sq_dist_from_cntr/(hw*hw));
% extend the velocity model
velextend=[vel(:,1)*ones(1,ngx2) vel vel(:,end)*ones(1,ngx2)];
velextend=[ones(ngz2,1)*velextend(1,:);velextend;ones(ngz2,1)*velextend(end,:)];
%
% now smooth
temp=conv2(1./velextend,gauss,'same')/sum(gauss(:));
velsmo=1./temp(ngz2+1:ngz2+length(z),ngx2+1:ngx2+length(x));