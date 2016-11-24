function seisex=ps_step(seis,t,x,v,dz)
%PS_STEP: take one dz step by phase shift at constant velocity
% 
% seisex=ps_step(seis,t,x,v,dz)
% 
% PS_STEP is just a friendly interface to IPS. The former has input and
% output in (x,t) while the latter has both in (kx,f). PS_STEP is mostly
% useful for learning about extrapolation and viewing the result.
%
% seis ... input seismic matrix (each column is one trace)
% t ... time coordinate vector for seis or dt
% x ... x coordinate vector for seis or dx
% NOTE: length(t) must equal the number of rows of seis unless t is a
% scalar in which case it is assumed to be the time sample interval. Same
% thing for x.
% v ... velocity, must be a scalar
% dz ... depth step in consistent units (must be a scalar)
%
% G.F. Margrave 2011
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

[nt,nx]=size(seis);
if(length(t)~=nt)
    if(length(t)==1)
        t=(0:nt-1)*t;
    else
        error('t vector is the wrong size');
    end
end
if(length(x)~=nx)
    if(length(x)==1)
        x=(0:nx-1)*x;
    else
        error('x vector is the wrong size');
    end
end
if(length(v)~=1)
    error('v must be scalar')
end
if(length(dz)~=1)
    error('dz must be scalar')
end

[spec,f,k]=fktran(seis,t,x);
dx=x(2)-x(1);
specex=ips(fftshift(spec,2),f,dx,v,dz);
seisex=ifktran(fftshift(specex,2),f,k);
seisex=seisex(1:nt,1:nx);