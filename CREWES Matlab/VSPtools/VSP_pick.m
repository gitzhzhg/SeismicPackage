function tp=VSP_pick(vsp,t,z,tfb,zfb,twin,option)
% VSP_pick: picks the first arrival times on a vsp
%
% tp=VSP_pick(vsp,t,z,tfb,zfb,twin,option)
%
% vsp ... vsp data stored as a matrix. one trace per depth, traces in
%       columns.
% t ... time coordinate vector for vsp 
% z ... receiver coordinate (depth) vector for vsp 
% tfb ... vector of at least two times giving the approximate
%       first break time at the first and last receiver depths.
% zfb ... vector of the same length as tfb giving depths of the times in
%       tfb
% twin ... picking window. Picks will be found within +/-twin of the
%       trajectory defined by tfb and zfb
% ****** default = .1 *****
% option ... 1 use sta/lta picking algorithm. sta and lta stand for short
%               term average and long term average.
%            2 use super-duper mystery picking
% ****** default = 2 *******
%
% tp ... vector of first break times, one for each depth
%
% Example:
% >> tp=VSP_pick(vsp,t,z,[0 .4],[0 1000]);
% >> plotimage(vsp,t,z); hold on
% >> plot(z,tp,'r*')
%
%
%
% G.F. Margrave, 2014, CREWES
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

if(nargin<7)
    option=1;
end
if(nargin<6)
    twin=.1;
end

if(option==1)
    pickflag=8;
else
    pickflag=9;
end

if(length(tfb)<2)
    error('tfb must contain at least two values');
end
if(length(tfb)~=length(zfb))
    error('tfb and zfb must be the same length');
end

if(zfb(1)<z(1) || zfb(end)>z(end))
    error('all elements of zfb must lie within z')
end

[ap,ae,tp,zp]=picker(vsp,t,z,tfb,zfb,twin,pickflag);
%convert tp to a row vector
tp=tp(:)';
%look for nans
ind=find(isnan(tp));
if(~isempty(ind))
    tp(ind)=[];
    zp(ind)=[];
    tmp=interp1(zp,tp,z,'linear','extrap');
    tp=tmp;
    zp=z;
end
%look for outliers
zpp=zp/mean(zp);
p=polyfit(zpp,tp,4);
tlinear=polyval(p,zpp);
tresidual=tp-tlinear;
sigmat=std(tresidual);
ind=find(abs(tresidual)>3*sigmat);
if(~isempty(ind))
    tp(ind)=nan;
end
%look for nans
ind=find(isnan(tp));
if(~isempty(ind))
    tp(ind)=[];
    zp(ind)=[];
    tmp=interp1(zp,tp,z,'linear','extrap');
    tp=tmp;
end