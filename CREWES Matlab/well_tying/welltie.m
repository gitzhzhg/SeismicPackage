function [stie,sreftie,tstretch,tphase]=welltie(s,t,sref,tref,twin,tinc,flag)
% WELLTIE: perform well tying by time-variant stretch and phase rotation
%
% [stie,sreftie,tstretch,tphase]=welltie(s,t,sref,tref,twin,tinc,flag)
%
% Given a fully processed seismic trace and a reference trace that is either
% a synthetic seismogram or a reflectivity produced from well logs, the
% seismic trace is "tied" to the reference by measuring time-variant delays
% and time-variant phase rotations. It is important to do a rough
% time-alignment of the reference trace prior to running this. This usually
% amounts to simply changing the start time of the reference trace by a
% constant time shift that represents the unlogged overburden. The well
% tying process here consists of mesuring time-variant time-shifts and
% time-variant phase rotations. Once measured, the default action is to
% apply the time-shifts to the reference trace (since they are time
% variant, this is a stretch) and the phase rotations to the seismic trace.
% Optionally, both can be applied to the seismic trace.
% Method: 
%   (1) Transfer the seismic bandwidth to the reference trace (see bandwidth_xfer)
%   (2) Check for polarity and flip s if necessary
%   (3) Measure time-variant time shift by tvmaxcorr using trace envelopes.
%       Using trace envelopes here helps to avoid the usual coupling
%       between time-shifts and phase rotations.
%   (4) Apply the measured time shifts to either the reference trace (the
%       default) or the seismic trace.
%   (5) Measure time-variant phase rotations using tvconstphase.
%   (6) Apply phase rotations to the seismic trace.
% It is expected that the reference trace will be shorter than the seismic
% trace. However, the longer the reference trace the better. If the
% reference trace is less than half the length of the seismic, consider
% creating a composite log using several wells with different depth ranges.
% Also be aware that shifts and rotations for times outside the range of
% the reference trace are simply constant extrapolations.
%
% s ... input seismic trace to be "tied"
% t ... time coordinate for s (same size as s)
% sref ... reference trace (seismogram at well with zero phase wavelet)
% tref ... time coordinate for sref (same size as sref)
% twin ... half width of Gaussian window used for time variant
%       localization. The maximum expected time shift should be less than
%       twin. A common value would be 0.1 or 0.2.
% tinc ... increment between windows. Typically tinc would be about twin/4.
%       A smaller tinc can detect more rapid time variance.
% flag ... determines how phase rotations and times shifts are applied
%           0: Don't apply phase rotations. Time shifts still applied to sref in
%               order to measure phase.
%           1: Apply phase rotations to s and time shifts to sref
%           2: Apply both phase rotations and time shifts to s
% ******** default flag=1 ********
% stie ... adjusted seismic trace
% sreftie ... adjusted reference trace
% tstretch ... measured and applied time shifts (same size as t)
% tphase ... measured and applied phase rotations (same size as t)
%
% To apply the time shifts to another trace, say s2, use
%   s2s=stretcht(s2,t,tstretch);
% To apply the phase rotations use
%   s2sr=tvphsrot(s2,t,-tphase,t);
% Note the minus sign in the phase rotations and that the rotations are applied after shifting.
% This is the case if flag=2 was used. For flag=1, then the reference trace has been shifted
% and the phase rotations should be applied directly to s2.
%
%
% by: G.F. Margrave, Devon Canada, 2016
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
    flag=1;
end

extrapbeg=0;
extrapend=0;
if(tref(1)>t(1))
    extrapbeg=1;
end
if(tref(end)<t(end));
    extrapend=1;
end
ind=near(t,tref(1),tref(end));

%transfer the seismic bandwidth to the reference
df=1/(t(end)-t(1));
n=round(5/df);
sref=bandwidth_xfer(s(ind),sref,n);

%check for an overall polarity flip
cc=maxcorr(sref,s(ind));
if(cc(1)<0)
    s=-s;
end

%measure time variant cc of envelopes
%note the use of env here. it fails with traces themselves
[cc,tcc]=tvmaxcorr(env(sref),env(s(ind)),t(ind),twin,tinc);
if(extrapbeg==1)
    cc=[cc(1,:);cc];
    tcc=[t(1);tcc];
end
if(extrapend==1)
    cc=[cc;cc(end,:)];
    tcc=[tcc;t(end)];
end
dt=t(2)-t(1);
delt2=cc(:,2)*dt;
tstretch=interp1(tcc,delt2,t);
% remove estimated delay
if(flag==2)
    s2=stretcht(s,t,tstretch);
    sreftie=sref;
elseif(flag==1 || flag==0)
    sreftie=stretcht(sref,tref,-tstretch(ind));
    s2=s;
else
    error('invalid value for flag');
end

%estimate the phase
[phs,tphs]=tvconstphase(sreftie,s2(ind),t(ind),twin,tinc);
if(extrapbeg==1)
    phs=[phs(1);phs];
    tphs=[t(1);tphs];
end
if(extrapend==1)
    phs=[phs;phs(end)];
    tphs=[tphs;t(end)];
end
tphase=interp1(tphs,phs,t);
%remove estimated phase
if(flag~=0)
    stie=tvphsrot(s2,t,-tphase,t);
else
    stie=s2;
end