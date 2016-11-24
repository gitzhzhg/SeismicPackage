function g=greenseed(a,x,y,xshot,yshot,f,fmax,v,dz)
% GREENSEED ... generate an unaliased 3D greens function
%
% g=greenseed(a,x,y,xshot,yshot,f,v,dz)
%
% GREENSEED evaluates a*exp(-i*k*r*sign(dz)))/r (k=2*pi*f/v) such that the propagating
% energy is not aliased.
%
% a ... scalar strength of the source
% x ... vector of x coordinates (columns) for the computation grid
% y ... vector of y coordinates (rows) for the computation grid
% **** Both x and y must have the same grid spacing and must be a power of 2 in length ****
% **** The origin of coordinates should be row 1 column 1 and coordinates
%       should increase not decrease. The origin need not be zero.
% xshot, yshot ... x and y coordinates of the source
% f ... frequency in Hertz at which calculation is done.
% fmax ... maximum frequency, used for antialiasing calculations, at which
%     this function will be called in the simulation.
% v ... must either be a scalar or a matrix of length(y) rows and length(x) 
%       columns. If the latter, then the GPSPI approximation is used.
% dz ... distance below (or above) the source at which the function is
%       evaluated. Positive for below, negative for above.
% g ... matrix of size ny by nx containing the complex-valued 3D Green's function
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

small=1000*eps;
dx=abs(x(2)-x(1));
dy=abs(y(2)-y(1));
nx=length(x);
ny=length(y);
if(abs(dx-dy)>small)
    error('x and y grid spacings must be identical')
end
nv=length(v);
if(nv~=1)
    if(size(v,1)~=length(y)) || (size(v,2)~=length(x))
        error('v must have the size length(y) X length(x)')
    end
% else
%     v=v*ones(ny,nx);
end
vmin=min(real(v(:)));

%determine the grid spacing for the unaliased calculation
%we assume the input grid is a power of two long. Find the next power of
%two grid such that the nyquist is greater than kcrit.
kcrit=fmax/vmin;
kxnyq=1/(2*dx);
pow2=nextpow2(nx);
dx2=dx;
nx2=nx;
while(kxnyq<kcrit)
    pow2=pow2+1;
    nx2=2^pow2;
    dx2=dx2/2;
    kxnyq=1/(2*dx2);
end
x2=(0:nx2-1)*dx2;
%determine number of y samples needed to span the aperture
kynyq=1/(2*dy);
pow2=nextpow2(ny);
dy2=dy;
ny2=ny;
while(kynyq<kcrit)
    pow2=pow2+1;
    ny2=2^pow2;
    dy2=dy2/2;
    kynyq=1/(2*dy2);
end
y2=(0:ny2-1)*dx2;

%Check for trivial case
trivial=0;
v2=v;
if(dx<=dx2)
    r=sqrt((y(:)*ones(1,nx)-yshot).^2+(ones(ny,1)*x(:)'-xshot).^2+dz^2);
    trivial=1;
else
    r=sqrt((y2(:)*ones(1,nx2)-yshot).^2+(ones(ny2,1)*x2(:)'-xshot).^2+dz^2);
    %upsample v is needed
    if(nv~=1)
        v2=interp2(v,x,y,x2,y2);
    end                     
end

%ok, calculate greens function
w=2*pi*f;
g=a*exp(-i*w*r./v2)./r;

%now downsample with antialias filtering
if(~trivial)
    %compute wavenumber vectors
    knyq = 1/(2*dx2);
    dkx = 1/(nx2*dx2); % or 2.*kxnyq/nx2;
    kx=[0:dkx:knyq-dkx -knyq:dkx:-dkx]';%column vector

    dky = 1/(ny2*dx2);
    ky=[0:dky:knyq-dky -knyq:dky:-dky];%row vector
    ghat=fft2(g);
    
    %new nyquist
    knyqnew=1/(2*dx);
    %
    ind=find(abs(abs(kx)-knyqnew)<small);
    if(length(ind)~=2)
        error('Logic failure in greenseed')
    end
    kx(ind(1))=knyqnew;ky(ind(1))=knyqnew;
    kx(ind(2))=-knyqnew;ky(ind(2))=-knyqnew;
    indx=find(kx>knyqnew | kx<= -knyqnew);
    indy=find(ky>knyqnew | ky<= -knyqnew);
    
    ghat(indy,:)=[];
    ghat(:,indx)=[];
    g=ifft2(ghat);
end



    