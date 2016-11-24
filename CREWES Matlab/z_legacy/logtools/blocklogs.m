function [vpblk,vsblk,rhoblk,zout,ztop]=blocklogs(lasfilename,dzblk,dzout,vp0,vs0,rho0,mud1,mud2,gardner1,gardner2)
% BLOCKLOGS: reads an las file for sonic and density logs and blocks them
% 
% [vpblk,vsblk,rhoblk,zblk,ztop]=blocklogs(lasfilename,dzblk,dzout,vp0,vs0,rho0,mud1,mud2,gardner1,gardner2)
%
% This function reads an LAS well file and returns vectors of vp, vs, and
% rho (density) which have been averaged over the interval dzout. A gradient
% overburden is attached that extends from specified surface (z=0) values
% of vp, vs, and rho, to the local averages of values found at the tops of
% the logs. This function always averages slowness and then inverts this to
% get velocity.
%
% Note: LAS file must contain at least a P-wave sonic. If S-sonics and/or
% density logs are missing they will be synthesized from the P-sonic using
% Castagna and Gardner. If there are multiple logs of the same type, the
% first one found will be used.
%
% lasfilename ... string containing the las file name (including the .las)
%                   and the path relative to the working directory. For
%                   example, '../mywell.las' refers to the file mywell.las
%                   in the parent directory to Matlab's current working
%                   directory. The current working directory may be
%                   displayed by the command: >>pwd
% dzblk ... depth step. Logs will be blocked (averaged) over this depth
%                   interval.
% ******** if dzblk<0, then the tops will be used as block boundaries with
% an implicit top at the top and bottom of the well. Between first top and
% the implcit top, layers will be inserted of thickness abs(dzblk). For
% example, dzblk=-100 will use the tops as boundaries except in the upper
% portion of the log where things will be blocked at 100 *******
%
% dzout ... output depth sample rate of the logs.
% ********** if dzout=-1, then the input sample rate will be used *********
% vp0 ... p-wave velocity at the depth z=0. This is used to fill in a
%                   linear gradient overburden from the first logged depth
%                   to depth 0.
%  ***** Default ... 1800 for metric logs and 5900 for Imperial logs
% vs0 ... s-wave velocity at depth z=0
%  ***** Default ... 600 for metric and 1970 for Imperial
% rho0 ... density at depth z=0
%  ***** Default ... 1.8 or 1800
% mud1,mud2 ... mudrock line coefficients. These are only used if there is
%                   no shear sonic. mud1 and mud2 are the coefficients in
%                   the desired Castagna relation vp=mud1*vs+mud2.
%  ****** Defaults are mud1=1.160 and mud2=1360 for metric and 1.160 and 4460
%                   for Imperial
% gardner1,gardner2 ... gardner constants. These are only used if there is
%                   no density log. In that case, a density log will be
%                   synthesized by rho=gardner1*vp.^gardner2
%   ***** Defaults: gardner1=.31 or 3100, gardner2=.25 *******
%
% vpblk ... vector of p-wave velocities sampled at dzblk
% vsblk ... vector of s-wave velocities sampled at dzblk
% rhoblk ... vector of p-wave velocities sampled at dzblk
% zblk ... vector of depths sampled at dzblk
% ztop ... first logged depth
%
% You can diplay the blocked model with something like
% figure
% plot(vpblk,zblk,'b',vsblk,zblk,'r',rhoblk,zblk,'g');flipy
% xlabel('Velocity or density');ylabel('Depth')
% title('Blocked logs')
% legend('V_p','V_s','\rho')
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

% read the las file
[logmat,mnem,desc,name,id,loc,null,units,kb,tops,ztops,lash]=readlas(lasfilename);
%search mnem for density, p-sonic, and s-sonic
ipson=0;
iden=0;
isson=0;
for k=2:size(mnem,1)
    thislog=las2logtype(mnem(k,:));
    switch thislog
        case 0
            ipson=k;
            disp(['P-wave sonic chosen is ' mnem(k,:)]);
        case 1
            iden=k;
            disp(['Density log chosen is ' mnem(k,:)]);
        case 2
            iden=k;
            disp(['Density log chosen is ' mnem(k,:)]);
        case 3
            iden=k;
            disp(['Density log chosen is ' mnem(k,:)]);
        case 7
            isson=k;
            disp(['S-wave sonic chosen is ' mnem(k,:)]);
    end
end
if(ipson==0)
    error('File has no p-sonic, cannot proceed')
end
metric=0;
if(strcmp(units,'M'))
    metric=1;
end
%establish defaults
if(nargin<4)
    if(metric)
        vp0=1800;
    else
        vp0=5900;
    end
end
if(nargin<5)
    if(metric)
        vs0=600;
    else
        vs0=1970;
    end
end
if(nargin<6)
    if(iden>0)
        den1=logmat(1,iden);
    else
        den1=1.8;
    end
    if(den1<10)
        rho0=1.8;
    else
        rho0=1800;
    end
end
if(nargin<7)
    mud1=1.160;
end
if(nargin<8)
    if(metric)
        mud2=1360;
    else
        mud2=4460;
    end
end
if(nargin<9)
    if(rho0>10)
        gardner1=310;
    else
        gardner1=.31;
    end
end
if(nargin<10)
    gardner2=.25;
end

 z=logmat(:,1);
 ztop=z(1);
 if(dzout==-1)
     dzout=z(2)-z(1);
 end
 sp=logmat(:,ipson);%p-wave sonic
 if(iden>0)
     rho=logmat(:,iden);
 else
     %synthesize a density log
     vp=10^6./ sp;
     rho=gardner1.*vp.^gardner2;
 end
 if(isson>0)
     ss=logmat(:,isson);
 else
     %synthesize an s-wave sonic
    vp=10^6 ./sp;
    vs=(vp-mud2)/mud1;
    ss=10^6 ./vs;
 end
 
 ind=find(isnan([sp ss rho]), 1);
 if(~isempty(ind))
     error('Input logs contain NAN''s. You must resolve this first using LOGEDIT')
 end
 
 %surface velocities and density

 sp0=10^6/vp0;%surface slowness

 ss0=10^6/vs0;%surface slowness

 %define number of samples at top of log to average to determine linear
 %gradient in overburden
 nave=20;
 %compute average values at logtop
 sptop=mean(sp(1:nave));
 sstop=mean(ss(1:nave));
 rhotop=mean(rho(1:nave));
 %extend depth axis to zero
 dz=z(2)-z(1);
 zmax=max(z);
 nzmax=round(zmax/dz)+1;
 z2=(0:nzmax-1)*dz;
 %define extended logs
 nz=length(z);
 if(z(1)~=0)
     sp2=zeros(size(z2));
     sp2(nzmax-nz+1:nzmax)=sp;
     sp2(1:nzmax-nz)=sp0+(sptop-sp0)*(z2(1:nzmax-nz)/z2(nzmax-nz));
     vp2=10^6./sp2;
     ss2=zeros(size(z2));
     ss2(nzmax-nz+1:nzmax)=ss;
     ss2(1:nzmax-nz)=ss0+(sstop-ss0)*(z2(1:nzmax-nz)/z2(nzmax-nz));
     vs2=10^6./ss2;
     rho2=zeros(size(z2));
     rho2(nzmax-nz+1:nzmax)=rho;
     rho2(1:nzmax-nz)=rho0+(rhotop-rho0)*(z2(1:nzmax-nz)/z2(nzmax-nz));
 else
     sp2=sp;
     z2=z;
     vp2=10^6./sp2;
     ss2=ss;
     vs2=20^6./ss;
     rho2=rho;
 end
%  figure
%  title('Logs with gradient overburden attached');
%  plot(vp2,z2,vs2,z2,rho2,z2);flipy
%  legend('vp','vs','\rho');

 %ok, now blocking
 %define block boundaries
 if(dzblk<0)
     zblk=sort(ztops);
     if(zblk(1)>z2(1))
         zupper=(z2(1):abs(dzblk):zblk(1)-abs(dzblk))';
         zblk=[zupper;zblk];
     end
     if(zblk(end)<z2(end))
         zblk=[zblk;z2(end)];
     end
     nzblk=length(zblk);
 else
     %these boundaries are defined to start at the base of the overburden
     %so that we do not block the overburden
     nzblk=round((zmax-ztop)/dzblk)+1;
     zblk=((0:nzblk-1)*dzblk+ztop)';%the first boundary is at the top of the log
 end
 zout=(z2(1):dzout:z2(end))';
 vpblk=zeros(size(zout));
 vsblk=zeros(size(zout));
 rhoblk=zeros(size(zout));
%adjust the last boundary
if(zblk(end)~=zout(end))
 zblk(end)=zout(end);
end
 z0=z2(1);
 %calculate the block averages and sample the blocks at sample rate dzout
 for k=1:nzblk-1
     zup=zblk(k);
     zdown=min(zblk(k+1),zmax);
     indin=round((zup-z0)/dz)+1:round((zdown-z0)/dz)+1;
     indout=round((zup-z0)/dzout)+1:round((zdown-z0)/dzout)+1;
     vpblk(indout)=10^6/mean(sp2(indin));
     vsblk(indout)=10^6/mean(ss2(indin));
     rhoblk(indout)=mean(rho2(indin));
 end
 %fill in the overburden but do not block it
 iover=near(zout,0,ztop);
 for k=iover
     ind=near(z2,zout(k));
     vpblk(iover(k))=10^6./sp2(ind(1));
     vsblk(iover(k))=10^6./ss2(ind(1));
     rhoblk(iover(k))=rho2(ind(1));
 end
 if(length(zout)<length(vpblk))
     vpblk=vpblk(1:length(zout));
     vsblk=vsblk(1:length(zout));
     rhoblk=rhoblk(1:length(zout));
 end
  if(length(zout)>length(vpblk))
     zout=zout(1:length(vpblk));
  end
 if(vpblk(end)==0)
     vpblk(end)=[];
     vsblk(end)=[];
     rhoblk(end)=[];
     zout(end)=[];
 end
%  vpblk(1)=vpblk(2);
%  vsblk(1)=vsblk(2);
%  rhoblk(1)=rhoblk(2);
%  figure
%  plot(vp2,z2,'b',vs2,z2,'r',rho2,z2,'g');flipy;hold
%  legend('vp','vs','\rho');
%  plot(vpblk,zout,'k',vsblk,zout,'k',rhoblk,zout,'k');
%  title('Blocked logs in black, gradient overburden attached')
%  xlabel('Velocity or density');
%  ylabel('Depth')
%  h1=stairs(vpblk,zblk);
%  set(h1,'color','r');
%  h2=stairs(vsblk,zblk);
%  set(h2,'color','b');
%  h3=stairs(rhoblk,zblk);
%  set(h3,'color','g');
%  filename = ['12_27_blocked_' int2str(dzblk) 'm'];
%  save(filename,'zblk','vpblk','vsblk','rhoblk')
 