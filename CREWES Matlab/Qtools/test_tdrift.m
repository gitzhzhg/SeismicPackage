%%% demo reading well logs from an las file and making normal incidence seismograms
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

%this file is written for the Hussar 12-27 well

%read the las file
filename='1409.las'; %enclose the fully qualified filename here in single quotes

[logmat,mnem,desc,name,id,loc,null,units,kb,tops,ztops,lash]=readlas(filename);
% by examining the returned variable mnem, we determine the columns of the
% log matrix to be
% column ... Identity
%    1   ... depth
%    2   ... p-wave sonic
%    3   ... density
%    4   ... gamma ray
%    5   ... s-wave sonic
%    6   ... poisson's ratio
%
%unpack interesting logs from logmat
z=logmat(:,1);%depth
sp=logmat(:,3);%p-wave sonic
rho=logmat(:,2);%density

%convert sonics to velocity
vp=10^6 ./sp;

%make a plot
figure
hh=plot(vp,z,'b',rho,z,'g');flipy
set(hh(2),'color',[0 .5 0]);
legend('p-wave velocity','density');
grid
title(['Well ' filename])
prepfig
% hs=plottops(ztops,tops,'k');
%%
%doubledepth, set to 2 to double the depths and get more Q effect
doubledepth=2;
z=z*doubledepth;
%%
%seismogram at well velocity and at seismic velocity
%
Q=75;%used throughout this simulation
f0=12500;%logging frequency
fdom=15;%seismic frequency
vseis=vp.*(1+(1./(pi*Q))*log(fdom/f0));
spseis=10^6 ./vseis;
%wavelet
dt=.002;
[wm,twm]=wavemin(dt,fdom,.2);
%we make a p-p normal incidence seismogram
fmult=1;%flag for multiple inclusion. 1 for multiples, 0 for no multiples
fpress=1;%flag for pressure (hydrophone) or displacement (geophone)
[sw,tw,rcw,pmw,pw]=seismo(sp,rho,z,fmult,fpress,wm,twm);%at well velocities
[ss,ts,rcs,pms,ps]=seismo(spseis,rho,z,fmult,fpress,wm,twm);%at seismic velocities
%make primaries only seismograms
swp=convm(rcw,wm);
ssp=convm(rcs,wm);


figure
amp=max(abs(sw));
h1=wtva(sw,tw,'b',0,1,-1);
h2=wtva(ss+amp,ts,'r',amp,1,-1);
% flipy
grid
legend([h1(1) h2(1)],'At v_{well}','At v_{seis}');
title(['Synthetic seismograms with multiples showing drift effect for Q=' int2str(Q)])
prepfig
figure
amp=max(abs(swp));
h3=wtva(swp,tw,'b',0,1,-1);
h4=wtva(ssp+amp,ts,'r',amp,1,-1);
grid
legend([h3(1) h4(1)],'At v_{well}','At v_{seis}');
title(['Synthetic seismograms without multiples showing drift effect for Q=' int2str(Q)])
prepfig
%% drift corrections
%drift correct the seismogram computed at vwell
[swdc,spd,tdr]=drift_corr(sw,tw,sp,z,Q,fdom,f0);
%make a new seismogram with drift corrected sonic
swdc2=seismo(spd,rho,z,fmult,fpress,wm,twm);%at well velocities
twdc2=dt*(0:length(swdc2)-1);

% hh=plot(ss,ts,'r',swdc+amp,tw,'b',swdc2+2*amp,twdc2,'b');
% flipy
figure
amp=max(abs(ss));
h1=wtva(ss,ts,'b',0,1,-1);
h2=wtva(swdc+amp,tw,'r',amp,1,-1);
h3=wtva(swdc2+2*amp,twdc2,[0 .5 0],2*amp,1,-1);
grid
legend([h1(1) h2(1) h3(1)],'Comp. at v_{seis}','Comp. at v_{well} and drift corr.',...
    'Comp. with drift corr. sonic');
title(['Synthetic seismograms showing drift correction for Q=' int2str(Q)])
prepfig
%%
%apply qmatrix with full drift to a synthetic seismogram
[wm,twm]=wavemin(dt,2*fdom,.2);%double fdom to start with higher f's
qmat1=qmatrix(Q,tw,wm,twm,1);%Nyquist drift
qmat3=qmatrix(Q,tw,wm,twm,3);%full drift
%so=convz(r,wo);
sn_fulldr=qmat3*rcw;%rcw is the rc's in time at vwell
sn_nyqdr=qmat1*rcw;%rcw is the rc's in time at vwell
figure
amp=max(abs(rcw));
h1=plot(tw,rcw);
h2=wtva(sn_nyqdr+1.5*amp,tw,'r',1.5*amp,1,-1);
h3=wtva(sn_fulldr+3*amp,tw,[0 .5 0],3*amp,1,-1);
grid
legend([h1 h2(1) h3(1)],'rcs at v_{well} time',['Q=' int2str(Q) ' w/ Nyq drift'],...
    ['Q=' int2str(Q) ' w/ Full drift']);
prepfig
%%
%Attempt a stationary decon
twin=.2;tinc=.05;
snb=tvbalans(sn_fulldr,tw,rcw,tw,twin,tinc);
id=near(tw,.3,.7);%decon design window
snbd=deconw(snb,snb(id),80,.0001);
snbdb=tvbalans(snbd,t,sref,t,twin,tinc);
twin=.1;tinc=.02;
[phs,tphs]=tvconstphase(sdb,sref,t,twin,tinc);
srot=tvphsrot(sdb,t,phs,tphs,twin,tinc);
figure
inc=.2;
plot(t,sn,t,snb+inc,t,sdb+2*inc,t,srot+3*inc,t,sref+4*inc);
title('Analysis without drift correction')
prepfig
legend('Nonstationary','Balanced','Wiener decon','phase rot','stationary')
figure
subplot(3,1,1)
plot(t,sdb)
%a=max(abs(sdb));
ylim([-.25 .25])
title('Before phase rotations')
subplot(3,1,2)
plot(t,srot)
ylim([-.25 .25])
title('After phase rotations')
subplot(3,1,3)
plot(tphs,phs)
title('Derived phase rotations')