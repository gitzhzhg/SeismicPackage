function [seisw,seis,t]=afd_shotrec(dx,dtstep,dt,tmax, ...
         model,snap1,snap2,xrec,zrec,wlet,tw,laplacian)
% AFD_SHOTREC ... makes finite difference shot records using initial snapsots
%
% [seisw,seis,t]=afd_shotrec(dx,dtstep,dt,tmax,model,snap1,snap2,xrec,zrec,wlet,tw,laplacian)
%
%
% AFD_SHOTREC will create a shot record or VSP given a velocity model and source 
% and receiver configutations using second-order finite-difference time stepping.
% The wave equation being simulated is constant density but variable velocity.
% See also AFD_SHOTREC_ALT for an alternative method of source injection. 
% AFD_SHOTREC is the more intuitive source specification but AFD_SHOTREC_ALT 
% may be preferred for movie making or other applications. In AFD_SHOTREC the
% sources are propagated as impulses and the resulting seismogram is filtered 
% after creation by the desired source wavelet.
%
% The sources are assumed to initiate at time=0 although a noncausal wavelet
% may be applied after-the-fact. Source locations (and implicitly the initial 
% conditions) are specifed by providing two input snapshots of the wavefield, 
% one at time=0-dtstep (i.e. one time step before source initiation) called 
% snap1 and one at time=0 called snap2. The simplest scheme makes 
% snap1=zeros(size(velocity)) meaning that the wavefield is zero before source 
% initiation. Then snap2 is similarly initialized to zero followed by the 
% placement of unit impulse(s) at the source location(s). For example:
% snap2=zeros(size(velocity));snap2(1,round(size(velocity,2)/2));
% places a unit source at the surface midway along the model. Source arrays
% are simulated by simply placing more impulses. Source location and
% strength are arbitrary.
%
% The finite difference algorithm can run with a five or nine point 
% approximation to the Laplacian operator.  The five point approximation 
% (second order in space) is faster, but the nine point (4th order) results 
% in a broader bandwidth. 
% 
% The final seismogram has one column per receiver and the number of rows is
% round(tmax/dt)+1. Receiver arrays are not explicitly provided although
% they can be simulated after-the-fact by appropriate spatial convolutions.
% Receivers may be placed anywhere in the model at locations specified by
% the xrec and zrec arrays. For example, xrec=dx*(0:size(velocity,2)-1);
% zrec=zeros(size(xrec)) places receivers at the surface at each grid
% location as is suitable for a typical shot record. On the other hand, 
% a VSP can be simulated by zrec=dx*(0:size(velocity,1)-1);
% xrec=ones(size(zrec))*500; This places receivers in a vertical borehole
% (column) from the top to the bottom of the velocity model at x coordinate
% of 1000.
%
% The user must ensure that the finite-difference stability conditions are met
% or the function will refuse to proceed. This involves choosing dtstep
% such that
%        dtstep <= a*dx/vmax
% where vmax is the maximum velocity found anywhere in the velocity model
% and a=1/sqrt(2) for the second order Laplacian (5 pt) and a=sqrt(3/8) for
% the fourth order Laplacian (9 pt). Note that reducing the spatial grid
% size (dx) generally requires that dtstep also decrease.
%
% dx = the bin spacing for both horizontal and vertical (in consistent units)
% dtstep = size of time step for modelling (in seconds). this parameter
%       must be chosen to ensure stability. See the discussion below
%       following the parameter "laplacian".
% dt = size of time sample rate for output seismogram. dt<dtstep causes resampling.
%		 dt>dtstep is not allowed. This allows the model to be oversampled for
%		 propagation but then conveniently resampled. Often dt is much
%		 greater than dtstep. This meanst that grid dispersion will be
%		 minimal in the frequency band defined by dt.
%		 The sign of dt controls the phase of the antialias resampling filter.
%		 dt>0 gives minimum phase, dt<0 is zero phase. Resampling is of course
%		 done at abs(dt). 
% tmax = the maximum time of the seismograms in seconds
% model = This is either a matrix containing the input velocity model or a cell 
%        array of two matrices, the first being the velocity model and the
%        second the density model. If the former case, then density is
%        assumed constant. If the latter case, then the velocity and
%        density matrices must be exactly the same size. The two grids must
%        have values in consistent units. If vel is the velocity matrix,
%        and den the density matrux, then putting these together in a cell
%        array is done by: model={vel;den}; .  Note that the velocity matrix
%        must be first and density second.
% snap1 = the wavefield at time=0 - dtstep.  (same size as velocity matrix)
%       This matrix determines the x and z sizes of the simulation. It should
%		be mostly zeros except for islotated ones corresponding to sources.
% snap2 = the wavefield at time = 0 (same size as velocity matrix)
%       This matrix represents the sources after one dtstep time step. Its often
%		acceptable to make this equal snap1.
% xrec = a vector of the x-positions of receivers (in consisent units)
%		The left hand column of snap1 is x=0.
% zrec = a vector of the z-positions of receivers (in consistent units)
%       The top row of snap1 is z=0. 
% wlet or filter = a 4 component ' Ormsby ' specification = [f1 f2 f3 f4] in Hz
%        or a wavelet. If it is longer than four elements, it is assumed to be
%      a wavelet. The wavelet should be sampled at the output sample rate
%      (dt)
% tw or phase ... If a the previous input was a four point filter, then this must
%     a scalar where 0 indicates a zero phase filter and 1 is a minimum phase
%     filter. If the previous input was a wavelet, then this is the time 
%     coordinate vector for the wavelet. The time sample rate of the wavelet MUST
%		equal dt.
% laplacian - an option between two approximation to the laplacian operator
%           - 1 is a 5 point approximation
%           STABILITY CONDITION: max(velocity)*dtstep/dx MUST BE < 1/sqrt(2)~ 0.7071
%           - 2 is a nine point approximation
%           STABILITY CONDITION: max(velocity)*dtstep/dx MUST BE < sqrt(3/8)~ 0.6124
%  ************** default = 1***********
%
% seisw = the filtered seismogram (wavelet applied)
% seis = the output seismogram including all frequencies
% t = the time vector (from 0 to tmax)
%
% by Carrie Youzwishen, February 1999
%    G. F. Margrave July 2000
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

tic;
if(iscell(model))
    if(length(model)~=2)
        error('model must be a cell array of length 2 for density option')
    end
    velocity=model{1};
    density=log(model{2});
    denflag=1;
    if(size(velocity)~=size(density))
        error('Velocity model and density model must be matrices of the same size')
    end
else
    velocity=model;
    density=0;
    denflag=0;
end
boundary=2;

[nz,nx]=size(snap1);
if(prod(double(size(snap1)~=size(snap2))))
	error('snap1 and snap2 must be the same size');
end
if(prod(double(size(snap1)~=size(velocity))))
	error('snap1 and velocity must be the same size');
end

xmax=(nx-1)*dx;
zmax=(nz-1)*dx;

% x=0:dx:xmax;
% z=(0:dx:zmax)';

nrec=length(xrec);
if(nrec~=length(zrec))
	error('xrec and zrec are inconsistent')
end

test=between(0,xmax,xrec,2);
if(length(test)~=length(xrec))
	error('xrec not within grid')
end

test=between(0,zmax,zrec,2);
if(length(test)~=length(zrec))
	error('zrec not within grid')
end
	

if laplacian ==1 
    if max(max(velocity))*dtstep/dx > 1/sqrt(2)
    	error('Model is unstable:  max(velocity)*dtstep/dx MUST BE < 1/sqrt(2)');
    end
elseif laplacian ==2
    if max(max(velocity))*dtstep/dx > sqrt(3/8)
    	error('Model is unstable:  max(velocity)*dtstep/dx MUST BE < sqrt(3/8)');
    end
else
   error('invalid Laplacian flag')
end


if(abs(dt)<dtstep)
	error('abs(dt) cannot be less than dtstep')
end

if(length(wlet)==4)
   %switch from ormsby to filtf style
   wlet=[wlet(2) wlet(2)-wlet(1) wlet(3) wlet(4)-wlet(3)];
   if(tw~=0 && tw ~=1)
      error('invalid phase flag');
   end
else
   if(length(wlet)~=length(tw))
      error('invalid wavelet specification')
   end
   w=wlet;
   wlet=0;
   if abs( tw(2)-tw(1)-abs(dt)) >= 0.000000001
      error('the temporal sampling rate of the wavelet and dt MUST be the same');
   end
end

%temporal information chosen by user
%t=(0:dtstep:tmax)';

%set up matrix for output seismogram
seis=zeros(floor(tmax/dtstep),nrec);

%transform receiver locations to bin locations
ixrec = floor(xrec./dx)+1;
izrec = floor(zrec./dx)+1;

%determine linear addresses for receivers
irec=(ixrec-1)*nz + izrec;
%or irec=sub2ind(size(snap1),izrec,ixrec);

%grab time zero from snap2
seis(1,:)=snap2(irec);

maxstep=round(tmax/dtstep)-1;
disp(['There are ' int2str(maxstep) ' steps to complete']);
time0=clock;

% each loop does two time steps
nwrite=2*round(maxstep/50)+1;
for k=1:2:maxstep
	
	%time step
    if(denflag)
        snap1=afd_snap_acoustic2(dx,dtstep,velocity,density,snap1,snap2,laplacian,boundary);
        seis(k+1,:)=snap1(irec);
        snap2=afd_snap_acoustic2(dx,dtstep,velocity,density,snap2,snap1,laplacian,boundary);
        seis(k+2,:)=snap2(irec);
    else
        snap1=afd_snap(dx,dtstep,velocity,snap1,snap2,laplacian,boundary);
        seis(k+1,:)=snap1(irec);
        snap2=afd_snap(dx,dtstep,velocity,snap2,snap1,laplacian,boundary);
        seis(k+2,:)=snap2(irec);
    end
    
    
    if rem(k,nwrite) == 0
        timenow=clock;
        tottime=etime(timenow,time0);
        timeperstep=tottime/k;
        timeleft=timeperstep*(maxstep-k);
        
        disp(['wavefield propagated to ' num2str(k*dtstep) ...
            ' s; computation time remaining ' ...
            num2str(timeleft) ' s']);
    end

end

%compute a time axis
t=((0:size(seis,1)-1)*dtstep)';

disp('modelling completed')

%resample if desired
if(abs(dt)~=dtstep)
	disp('resampling')
	phs=(sign(dt)+1)/2;
	dt=abs(dt);
	for k=1:nrec
		cs=polyfit(t,seis(:,k),4);
		[tmp,t2]=resamp(seis(:,k)-polyval(cs,t),t,dt,[min(t) max(t)],phs);
		seis(1:length(tmp),k)=tmp+polyval(cs,t2);
	end
	seis(length(t2)+1:length(t),:)=[];
	t=t2;
end
if(iscomplex(seis))
        yyy=1;
end

%filter

seisw=zeros(size(seis));
if(~wlet)
   nzero=near(tw,0);
   disp('applying wavelet');
	ifit=near(t,.9*max(t),max(t));
	tpad=(max(t):dt:1.1*max(t))';
   for k=1:nrec
		tmp=seis(:,k);
		cs=polyfit(t(ifit),tmp(ifit),1);
		tmp=[tmp;polyval(cs,tpad)];
      tmp2=convz(tmp,w,nzero);
		seisw(:,k)=tmp2(1:length(t));
   end
else
   disp('filtering...')
	ifit=near(t,.9*max(t),max(t));
%   HDG changed from 	tpad=(max(t):dt:1.1*max(t))';
	tpad=(max(t)+dt:dt:1.1*max(t))';
   for k=1:nrec
		tmp=seis(:,k);
		cs=polyfit(t(ifit),tmp(ifit),1);
		tmp=[tmp;polyval(cs,tpad)];
%       HDG changed from tmp2=filtf(tmp,t,[filt(1) filt(2)],[filt(3) filt(4)],phase);
        tmp2=filtf(tmp,[t;tpad],[wlet(1) wlet(2)],[wlet(3) wlet(4)],tw);
		seisw(:,k)=tmp2(1:length(t));
   end
end

if(iscomplex(seisw))
        %disp('Really really bad! Complex amplitudes generated...')
        seisw=real(seisw);
end

toc;