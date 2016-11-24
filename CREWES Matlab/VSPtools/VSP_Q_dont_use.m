function [Qint,z0,zq,CA]=VSP_Q(vspdown,t,z,tp,zq,z0,twin,f1,f2,method,spectype,pflags,wintype,p)
% VSP_Q: Estimate Q values from a VSP downgoing wave
%
% [Qint,z0,zq,CA]=VSP_Q(vspdown,t,z,tp,zq,z0,twin,f1,f2,method,spectype,pflags,wintype,p)
%
% vspdown ... downgoing wave estimated from a VSP (see VSP_separation)
% t ... time coordinate for vspdown
% z ... depth coordinate for vspdown
% NOTE: These conditions must be true: length(t)==size(vspdown,1) and
%       length(z)==size(vspdown,2)
% tp ... first break picks for the downgoing field
% NOTE: length(z) must equal length(tp)
% zq ... vector of receiver depths for which Q estimates are desired
% z0 ... depth(s) to which each zq is compared. If a scalar, then all zq
%           are compared to the same depth. Alternatively, it may be a
%           vector the same length as zq giving each Q estimate a different
%           starting depth.
% NOTE: the zq and z0 must all lie within z and each z0 must be less than
%       the corresponding zq.
% twin ... temporal window legth for spectral estimation
% f1 ... lowest trustworthy signal frequency (no effect for method 'domfreq'
% f2 ... highest trustworthy signal frequency
% method ... choose 'specrat' (spectral ratio), 'specmatch' (spectral
%       matching), 'domfreq' (dominant frequency matching), or 'all'.
%       See qestimator.m for more information.
% spectype ... choose 'fourier','burg',or 'multi'. See qestimator.m for more
%       information.
% pflags ... vector the same length as zq. All entries must be either zero
%       or 1. A value of 1 causes a plot to be produced showing the result
%       of the Q estimation. A value of 0 makes no plot.
% ********* default = zeros(size(zq)) ***************
% wintype ... 'boxcar' 
%             'cosine' (raised cosine)
%             'gaus' Gaussian truncated at two standard deviations
%             'mwin'  see mwindow.m (20% taper)
%             'mwhalf' see mwhalf.m (20% taper). Intended for VSPs
% NOTE: wintype only matters if spectype='fourier'
% ************ default = 'mwhalf' **************
% p ... exponent used in dominant frequency calculation (see q_centroid)
% ************ default p=2 ***********
%
%
% Qint ... vector of interval Q's corresponding to the depths zq and
%           relative to z0. There are length(zq) of these and each is
%           actually an average Q for the depth interval z0(k) to zq(k).  You
%           could call this an interval Q.
% z0 ... vector of start depths for the interval Q calculations. These may
%           be different from the input z0's if the latter did not fall
%           exactly on a sample.
% zq ... vector of end depths for the interval Q calculations. These may
%           be different from the input z0's if the latter did not fall
%           exactly on a sample.
% CA ... cumulative attenuation measures corresponding to the estimated
% Q's. CA=pi*(t2-t1)/Qint(t1,t2). See Hauge, 1981, 
%       'Measurements of attenuation from vertical seismic profiles',Geophysics.
%
%
% Note: To convert either z0 or zq to a time, use the correspondence
% between tp and z and linear interpolation. For example:
% tq=interp1(z,tp,zq);
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

if(nargin<12)
    pflags=zeros(size(zq));
end
if(nargin<13)
    wintype='mwhalf';
end
if(nargin<14)
    p=2;
end

%check for cell array input of zq and z0
if(iscell(zq))
    if(length(zq)~=2)
        error('zq must be a length 2 cell array');
    end
    dzq=zq{2};
    zq=zq{1};
    if(length(dzq)==1)
        dzq=dzq*ones(size(zq));
    end
    if(length(zq)~=length(dzq))
        error('the two cell entries of zq must be equal length vectors');
    end
else
    dzq=zeros(size(zq));
end

if(iscell(z0))
    if(length(z0)~=2)
        error('z0 must be a length 2 cell array');
    end
    dz0=z0{2};
    z0=z0{1};
    if(length(dz0)==1)
        dz0=dz0*ones(size(z0));
    end
    if(length(z0)~=length(dz0))
        error('the two cell entries of z0 must be equal length vectors');
    end
else
    dz0=zeros(size(z0));
end


if(any(zq<z(1)) || any(zq>z(end)))
    error('zq must lie within z')
end
if(length(z0)==1)
    z0=z0*ones(size(zq));
end
if(any(z0<z(1)) || any(z0>z(end)))
    error('z0 must lie within z')
end
if(any(zq<z0))
    error('each z0 must be less than the corresponding zq')
end

if(length(t)~=size(vspdown,1))
    error('t vector and vspdown don''t match')
end

if(length(z)~=size(vspdown,2))
    error('z vector and vspdown don''t match')
end

if(length(z)~=length(tp))
    error('z and tp must be the same length')
end

% if(any(diff(tp)<=0))
%     error('tp must be monotonically increasing')
% end

%determine indices for the analysis depths
iq=zeros(size(zq));
i0=iq;
diq=cell(size(iq));
di0=diq;
for k=1:length(zq)
    iq(k)=near(z,zq(k));
    i0(k)=near(z,z0(k));
    diq{k}=near(z,zq(k)-dzq(k),zq(k)+dzq(k));
    di0{k}=near(z,z0(k)-dz0(k),z0(k)+dz0(k)); 
end
%recompute the zq(k) in case they are not exactly on samples
zq=z(iq);
tq=tp(iq);
%starting level
z0=z(i0);%might change if input z0 is not eqactly in z
t0=tp(i0);%start time at the reference depths
% iwin0=near(t,t0,t0+twin);%samples for reference wavelet
% w0=vspdown(iwin0,i0);%the reference wavelet

% Qave=nan*ones(size(zq));
%Qave=zeros(size(zq));

Qint=zeros(size(zq));
CA=Qint;

tbackoff=.02;%the time gate will start this much earlier than the first breaks


for k=1:length(iq)
    i0k=i0(k);%starting level for kth computation
    w0=vspdown(:,i0k);%the reference waveform
    tk=tq(k);%start time for the kth analysis
    wk=vspdown(:,iq(k));
%     Qave(k)=qestimator(w0,wk,t,t0,tk,twin,f1,f2,wintype,spectype,pflag,method);
%     Qint(k)=Qave(k);
    t1=max(t(1),t0(k)-tbackoff);
    t2=min(t(end),tk-tbackoff);
    if(t2<=t1)
        Qint(k)=inf;
        CA(k)=0;
    else
        [Qint(k),CA(k)]=qestimator(w0,wk,t,t1,t2,twin,f1,f2,wintype,spectype,pflags(k),method,nan,p);
    end
end
% zqave=(z0+zq)/2;
% zqint=(zq(1:end-1)+zq(2:end))/2;
    
    