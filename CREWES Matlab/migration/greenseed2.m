function g=greenseed2(a,x,xshot,f,fmax,v,dz,ifilt)
% GREENSEED2 ... generate an unaliased 2D Greens function for the Helmholtz equation.
%
% g=greenseed(a,x,xshot,f,fmax,v,dz,ifilt)
%
% GREENSEED evaluates (a*i/4)H_0^1(-2*pi*f*r/v) such that the propagating
% energy is not aliased. H_0^1 is the zero order Hankel function of the
% first kind and r is the radial distance from (xshot,0) to (x,dz). This is
% the 2D free-space Green's function for the Helmholtz equation (Zauderer,
% Partial Differential Equations of Applied Mathematics, 1989, p383). This
% Green's function can be condsidered to be the response of a monochromatic
% source at (xshot,0) as viewed at (x,dz). If v is variable, then the GPSPI
% (or locally homogeneous) approximation is used. For a time domain source,
% run greenseed2 for each frequency and inverse Fourier transform (i.e. ifftrl).
%
% a ... scalar strength of the source
% x ... vector of x coordinates for the computation grid
% **** The origin of coordinates should be x(1) and coordinates
%       should increase not decrease. The origin need not be zero. ****
% xshot ... x coordinate of the source
% f ... frequency in Hertz at which calculation is done.
% fmax ... maximum frequency, used for antialiasing calculations, at which
%     this function will be called in the simulation.
% v ... must either be a scalar or a vector of the same size as x 
%     If the latter, then the GPSPI approximation is used.
% dz ... distance below (or above) the source at which the function is
%       evaluated. Positive for below, negative for above.
% ifilt ... flag, if 1 the apply antialias filter before downsampling, if
%       0, don't bother.
%   *********** default = 1 **********
% g ... complex-valued green's function the same size as x
% 
% G. F. Margrave, CREWES, October 2008
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

if(nargin<8)
    ifilt=1;
end

small=1000*eps;
dx=abs(x(2)-x(1));
nx=length(x);
nv=length(v);

if(nv~=1)
    if(length(v)~=length(x))
        error('v must have the same length as x')
    end
else
    v=v*ones(size(x));
end
vmin=min(real(v(:)));

%pad inputs to power of 2
n2=nextpow2(nx);
nxp=2^n2;
if(nxp~=nx)
    xp=(0:nxp-1)*dx+x(1);
    vp=zeros(size(xp));
    vp(1:nx)=v;
    vp(nx+1:nxp)=v(end);
else
    xp=x;vp=v; 
end
if(ifilt)
    %determine the grid spacing for the unaliased calculation
    %Find the next power of
    %two grid such that the nyquist is greater than kcrit.
    kcrit=fmax/vmin;
    kxnyq=1/(2*dx);
    pow2=nextpow2(nxp);
    dx2=dx;
    nx2=nxp;
    while(kxnyq<kcrit)
        pow2=pow2+1;
        nx2=2^pow2;
        dx2=dx2/2;
        kxnyq=1/(2*dx2);
    end
    x2=(0:nx2-1)*dx2+x(1);
    %Check for trivial case
    trivial=0;
    v2=vp;
    if(dx<=dx2)
        r=sqrt((xp-xshot).^2+dz^2);
        trivial=1;
    else
        r=sqrt((x2-xshot).^2+dz^2);
        %upsample v is needed
        if(nv~=1)
            v2=interp1(xp,vp,x2);
            ind=find(isnan(v2));
            if(~isempty(ind))
                v2(ind)=mean(vp);
            end
        end                     
    end
    %ok, calculate greens function
    w=2*pi*f;
    g=(1i*a/4)*besselh(0,1,-w*r./v2); 
    %now downsample with antialias filtering
    if(~trivial)
        %compute wavenumber vectors
        knyq = 1/(2*dx2);
        dkx = 1/(nx2*dx2); % or 2.*kxnyq/nx2;
        kx=[0:dkx:knyq-dkx -knyq:dkx:-dkx]';%column vector

        ghat=fft(g);

        %new nyquist
        knyqnew=1/(2*dx);
        %
        ind=find(abs(abs(kx)-knyqnew)<small);
        if(length(ind)~=2)
            error('Logic failure in greenseed2')
        end
        kx(ind(1))=knyqnew;
        kx(ind(2))=-knyqnew;
        indx= kx>knyqnew | kx<= -knyqnew;

        ghat(indx)=[];
        g=ifft(ghat);
        g=g(1:nx);
    else
        g=g(1:length(x));
    end
else
%    ok, calculate greens function without antialias filter
    r=sqrt((x-xshot).^2+dz^2);
    w=2*pi*f;
    g=(i*a/4)*besselh(0,1,-w*r./v);
end



    