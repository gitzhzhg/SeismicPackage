function [vel,x,z,zlay,vlay]=channelmodel(dx,xmax,zmax,vhigh,vlow,zchannel,wchannel,hchannel,vchannel,nlayers,flag,zlay,vlay)
% CHANNELMODEL : build a model representing a channel in a stratigraphic sequence
%
% [vel,x,z,zlay,vlay]=channelmodel(dx,xmax,zmax,vhigh,vlow,zchannel,wchannel,hchannel,vchannel,nlayers,flag,zlay,vlay)
%
% This function builds a velocity matrix representing a channel buried in
% the middle of a stratigraphic sequence. The number of layers and the
% total model width and depth are specified. Layer thicknesses are chosen
% to fluctuate randomly about the nominal thickness of zmax/nlayers. Layer 
% velocities are chosen to grade uniformly from vmin to vmax. The
% model is created to use with the acoustic finite difference code.
%
% dx ... grid interval (distance between grid points in x and z)
% xmax ... maximum x coordinate (minimum is zero)
%  *********** default 2500 **********
% zmax ... maximum z coordinate (minimum is zero)
%  *********** default 1000 ************
% vhigh ... highest velocity used in the model
%  *********** default 4000 ************
% vlow ... lowest velocity used in the model
%  *********** default 2000 ************
% zchannel ... depth to the channel
%  *********** default zmax/2 *******
% wchannel ... width of the channel
%  *********** default 5*dx ********
% hchannel ... height (thickness) of the channel
%  *********** default  5*dx  **********
% vchannel ... velocity of the channel
%  *********** default  vlow+(vhigh-vlow)/nlayers **********
% nlayers ... number of sedimentary layers
%  *********** default 4 *************
% flag ... if 0 distribute layers randomly, if 1 put layers mostly above
%  *********** default 1 *************
% zlay ... vector of layer depths
%  If provided, this overrides nlayers and flag
%  Must have at least 2 entries, first entry should be 0.
% ************ if not provided, then chosen randomly ********
% vlay ... vector of layer velocities
%  If provided, this must be the same length as zlay and specifies the
%  velocities for the layer above each entry in zlay. If not provided then
%  velocities are chosen randomly between vlow and vhigh.
%
% vel ... velocity model matrix
% x ... x coordinate vector for vel
% z ... z coordinate vector for vel
% zlay ... vector of layer depths
% vlay ... vector of layer velocities
%
% NOTE: the simplest way to plot vel is: plotimage(vel-mean(vel(:)),z,x);
% A better plot is figure;imagesc(x,z,vel);colorbar;
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
    [vel,x,z,zlay,vlay]=channelmodel(5,1000,1000,4000,1000,500,100,50,2000,5);
    figure
    imagesc(x,z,vel);colorbar
    title('channel mode, colors indicate velocity')
    xlabel('distance (m)')
    ylabel('depth (m)')
    clear vel x z zlay vlay
    return;
end

if(nargin<5)
    vlow=2000;
end
if(nargin<4)
    vhigh=4000;
end
if(nargin<3)
    zmax=1000;
end
if(nargin<2)
    xmax=2500;
end
if(nargin<6)
    zchannel=zmax/2;
end
if(nargin<7)
    wchannel=5*dx;
end
if(nargin<8)
    hchannel=5*dx;
end
if(nargin<10)
    nlayers=4;
end
if(nargin<9)
    vchannel=vlow+(vhigh-vlow)/nlayers;
end
if(nargin<11)
    flag=1;
end
if(nargin<12)
    zlay=[];
    vlay=[];
else
    if(length(zlay)<2)
        error('must be at least two layers')
    end
    if(nargin<13)
        error('vlay must be prescribed if zlay is prescribed');
    end
    if(length(zlay)~=length(vlay))
        error('zlay and vlay must be the same size');
    end
    vhigh=max(vlay);
    vlow=min(vlay);
    if(zlay(1)~=0)
        zlay=[0;zlay(:)];
        vlay=[vlay(:);vhigh];
    end
    test=diff(zlay);
    if(any(test<0))
        error('layers must be in order of increasing depth')
    end
    nlayers=length(zlay); %override nlayers
end
%initialize
if(flag)
    thicknom=zchannel/nlayers;
else
    thicknom=zmax/nlayers;%nominal thickness
end

x=0:dx:xmax;z=0:dx:zmax; % x and z coordinate vector
vrange=vhigh-vlow; % high and low velocities
vel=vhigh*ones(length(z),length(x));%initialize velocity matrix
xpoly=[-dx xmax+dx xmax+dx -dx];
if(isempty(vlay))
    vlay=zeros(nlayers,1);
    vlay(end)=vhigh;
end
if(isempty(zlay))
    zlay=zeros(nlayers,1);   
    for k=2:nlayers
        tmp=thicknom*(rand(1)+.5);
        tmp=round(tmp/dx)*dx;
        if(flag && k==nlayers)
            zlay(k) = zchannel+dx;
        else
            zlay(k) = zlay(k-1)+tmp;
        end    
        zpoly=[zlay(k-1)-dx zlay(k-1)-dx zlay(k) zlay(k)];
        vlay(k-1)=vlow+(k-1)*vrange/(nlayers);
        vel=afd_vmodel(dx,vel,vlay(k-1),xpoly,zpoly);%install layer
    end
else
    for k=2:nlayers
        zpoly=[zlay(k-1)-dx zlay(k-1)-dx zlay(k) zlay(k)];
        %vlay(k-1)=vlow+(k-1)*vrange/(nlayers);
        vel=afd_vmodel(dx,vel,vlay(k-1),xpoly,zpoly);%install layer
    end
end

%install channel
xm=mean(x);
x1=round((xm-wchannel/2)/dx)*dx;
x2=round((xm+wchannel/2)/dx)*dx;
z1=round(zchannel/dx)*dx;
z2=round((zchannel+hchannel)/dx)*dx;
xpoly=[x1 x2 x2 x1];
zpoly=[z1 z1 z2 z2];
vel=afd_vmodel(dx,vel,vchannel,xpoly,zpoly);%install channel