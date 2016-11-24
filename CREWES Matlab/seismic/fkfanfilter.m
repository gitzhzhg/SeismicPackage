function [seisf,mask,f,kx]=fkfanfilter(seis,t,x,va1,va2,dv,flag,xpad,tpad)
%FKFANFILTER ... apply an fk fan filter to a seismic section or gather
%
% [seisf,mask,f,kx]=fkfanfilter(seis,t,x,va1,va2,dv,flag,xpad,tpad)
%
% FKFANFILTER designs and applies an f-k (frequency-wavenumber) fan reject
% filter. The reject region is fan-shaped and defined by two bounding
% apparent velocities, va1 and va2. These are expressed as positive numbers
% and the option is provided to reject both positive and negative apparent
% velocities or either one separately. A raised cosine taper is applied to
% the filter edge. The filter is constructed as a mask which is a
% multiplicative operator, the same size as the fk spectrum of the data,
% whose values lie between 0 and 1. This mask can be examined by returning
% all four values and then plotting it.
%
% seis ... seismic section or gather to be filtered. One trace per column
% t ... time coordinate ve3ctor for seis
% x ... space coordinate vector for seis
% REQUIREMENT: size(seis,1) must equal length(t) and size(seis,2) must
% equal length(x)
% va1 ... minumum apparent velocity defining the rejection fan. Enter 0 to
%       reject everything slower than va2.
% va2 ... maximum apparent velocity defining the rejection fan.
% Requirement: va2>va1. neither value van be negative.
% dv  ... with of the taper on the edge of the rejection fan in velocity
%         units
% REQUIREMENT: 0<dv<va1<=va2.  Setting va1=va2 gives a very narrow reject
% region. Better rejection of a specific velocity, vn, follows from makeing
% va1 slightly less than vn and va2 slightly greater.
%
% flag ... -1 ... reject only negative apparent velocities
%           0 ... reject both postive and negative apparent velocities
%           1 ... reject only positive apparent velocities
% ********* default flag=0 *********
% xpad ... size (in x units) of spatial zero pad to be afixed to seis
% ********* default = 0.1*(max(x)-min(x))***********
% tpad ... size (in t units) of temporal zero pad to be afixed to seis
% ********* default = 0.1*(max(t)-min(t)) **********
%
% NOTE: The values supplied for xpad and tpad are minimal pads because,
% after afixing these pads, the maxtrix is further extended to the next
% power of 2 in both dimensions.
%
% seisf ... the f-k filtered result with all pads removed. It will be the
%       same size as seis.
% mask ... the f-k filter multiplier. If seisfk is the fk transform of the
%       padded input, then the fk filter is applied as seisfk.*mask.
% f ... frequency coordinate for mask
% kx ... wavenumber coordinate for mask
% 
% The mask can be displayed by: plotimage(mask,f,kx)
% 
% G.F. Margrave, CREWES Project, U of Calgary, 2015
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
if(nargin<9)
    tpad=0.1*(max(t)-min(t));
end
if(nargin<8)
    xpad=0.1*(max(x)-min(x));
end
if(nargin<7)
    flag=0;
end
[nt,nx]=size(seis);
if(length(t)~=nt)
    error('seis and t have incompatible sizes')
end
if(length(x)~=nx)
    error('seis and x have incompatible sizes')
end
if(dv<=0 || va1<0 || va2<=0)
    error('dv, va1,va2 must not be negative')
end
if(va1>va2)
    error('va1 must be less than va2')
end
% if(dv>va1)
%     error('dv must be less than va1')
% end
small=1000*eps;
if(sum(abs(diff(diff(x))))>small)
    error('x coordinates must be regularly spaced')
end
if(sum(abs(diff(diff(t))))>small)
    error('t coordinates must be regularly spaced')
end
dx=x(2)-x(1);
dt=t(2)-t(1);
%attach x pad 
nx2=nx;
if(xpad>0)
    nxpad=round(xpad/dx);
    seis=[seis zeros(nt,nxpad)];
    nx2=nx+nxpad;
    x=(0:nx2-1)*dx;
end
%attach t pad
nt2=nt;
if(tpad>0)
    ntpad=round(tpad/dt);
    nt2=nt+ntpad;
    t=(0:nt2-1)*dt;
    seis=[seis;zeros(ntpad,nx2)];
end

%fk transform
[seisfk,f,kx]=fktran(seis,t,x);

%design filter mask
mask=ones(size(seisfk));
kmin=3;%minimum width of reject region
va1dv=va1-dv;%beginning of taper on low end
va2dv=va2+dv;%end of taper on high end
dk=kx(2)-kx(1);
k0=kx(1);%the first wavenumber sample
nk2=length(kx);
for k=1:length(f)
% for k=62
    %reject region, full rejection from kr1 to kr2
    kr1=f(k)/va1;
    kr2=f(k)/va2;
    %tapers
    kr0=f(k)/va1dv; %taper from kr0 to kr1
    kr3=f(k)/va2dv; %taper from kr2 to kr3
    %note: kr0<kr1<kr2<kr3
    if(flag==0 || flag==-1)
        %negative apparent velocity
        ik0=(-kr0-k0)/dk+1;%start of taper
        ik1=(-kr1-k0)/dk+1;%start of full reject
        if(ik1-ik0<kmin)
            ik0=ik1-kmin;%ensures at least kmin samples at first edge
        end
        ik2=(-kr2-k0)/dk+1;%end of full reject
        ik3=(-kr3-k0)/dk+1;%end of taper
        if(ik3-ik2<kmin)
            ik3=ik2+kmin;%ensures at least kmin samples at second edge
        end
        jk1=max([round(ik1) 1]);
        jk2=round(ik2);
        if(round(ik3)>0)
            mask(k,jk1:jk2)=0;%full reject region
            jk0=max([round(ik0) 1]);
            %first taper
            if(ik1>0)
                mask(k,jk0:jk1)=(.5+.5*cos(pi*((jk0:jk1)-ik0)/(ik1-ik0))).*mask(k,jk0:jk1);
            end
            jk3=round(ik3);
            %second taper
            jk2=max([jk2 1]);
            mask(k,jk2:jk3)=(.5+.5*cos(-pi*((jk2:jk3)-ik3)/(ik2-ik3))).*mask(k,jk2:jk3);
        end
    end
    if(flag==0 || flag==1)
        %positive apparent velocity
        ik0=(kr0-k0)/dk+1;%end of taper
        ik1=(kr1-k0)/dk+1;%end of full reject
        if(ik0-ik1<kmin)
            ik0=ik1+kmin;%ensure at least kmin samples in final taper
        end
        ik2=(kr2-k0)/dk+1;%start of full reject
        ik3=(kr3-k0)/dk+1;%start of taper
        if(ik2-ik3<kmin)
            ik3=ik2-kmin;
        end
        jk1=min([round(ik1) nk2]);
        jk2=round(ik2);
        if(round(ik3)<=nk2)
            mask(k,jk2:jk1)=0;
            jk0=min([round(ik0) nk2]);
            if(ik1<nk2)
                mask(k,jk1:jk0)=(.5+.5*cos(pi*((jk1:jk0)-ik0)/(ik1-ik0))).*mask(k,jk1:jk0);
            end
            jk3=round(ik3);
            jk2=min([jk2 nk2]);
            mask(k,jk3:jk2)=(.5+.5*cos(-pi*((jk3:jk2)-ik3)/(ik2-ik3))).*mask(k,jk3:jk2);
        end
    end
end
%apply the mask
seisfk=seisfk.*mask;

%inverse transform
seisf=ifktran(seisfk,f,kx);
%truncate pads if needed
if(size(seisf,1)>nt)
    seisf=seisf(1:nt,:);
end
if(size(seisf,2)>nx)
    seisf=seisf(:,1:nx);
end
    