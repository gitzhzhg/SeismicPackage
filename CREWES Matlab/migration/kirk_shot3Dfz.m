function [shotmig,zmig,xmig,ymig,xgrid,ygrid]=kirk_shot3Dfz(shotrec,t,x,y,xshot,yshot,vrms,tv,params,zmig)
% KIRK_SHOT3Dfz: Kirchhoff prestack shot-record time migration in 3D, depth output   
% 
% [shotmig,zmig,xmig,ymig,xgrid,ygrid]=kirk_shot3Dfz(shotrec,t,x,y,xshot,yshot,velmod,tv,params,zmig)
%
% KIRK_SHOT3Dfz migrates a single 3D shot record using prestack time
% migration, and outputs in depth. This is especially useful if only select
% depth planes are desired. The 3D shot record is stored in a 2D 
% matrix and it is not necessary that the traces fall on a regular spatial 
% grid. The algorithm is simple traveltime path summation with a few
% options as described in the parameters vector. Traveltime is described by
% a single, laterally invariant, vrms function. Time shifts are
% accomplished by frequency domain phase shift where the frequency domain 
% operations are done on narrow fairways defined around the traveltime for
% each event. The choice of the fairway width is a key parameter and, for
% 2ms data, it has been found that 10ms fairway (params(9)) seems to work
% well. There is no topographic compensation at present. The
% source and all receivers are assumed to be on the same horizontal plane.
% The Kirchhoff integral is done as described in Bleistein, Cohen, and
% Stockwell p247 eqn 5.2.22. Although this derivation is for constant
% velocity, the adjustments for time migration are reasonably obvious.
%
% shotrec ... 2D matrix containing the shot record. One trace per column.
% t ... time coordinate vector for shotrec. 
%           Requirement: length(t)=size(shotrec,1);
% x ... row vector of the first space coordinate for shotrec. Let
%   [nt,ntr]=size(shotrec), then we must have ntr=length(x);
%           Requirement: length(x)=size(shotrec,2);
% y ... row vector of the first space coordinate for shotrec. Let
%   [nt,ntr]=size(shotrec), then we must have ntr=length(y);
%           Requirement: length(y)=size(shotrec,2)
% vrms ... vector of rms velocities as a function of time
% tv ... time coordinate vector for vrmsd
% xshot ... x coordinate of the shot. It must be in the same coordinate
%       system as vector x. 
% yshot ... y coordinate of the shot. It must be in the same coordinate
%       system as vector y. 
%
% params ... vector of migration parameters. An entry of nan gets the
% default.
%
%    params(1--3) : migration aperture and its taper
%    params(1) ... physical aperture in meters. This is the largest lateral
%               distance a trace may have from the output location and still be
%               allowed to contribute.
%	          default is the largest diagonal of the velocity model
%    params(2) ... width of the aperture taper
%                 default is 0.05*params(1)
%    params(3) ... = 0, linear taper
%                     = 1, cosine taper   
%                 default is 1 (cosine taper)
%    params(4-5) : scattering angle limit (degrees)
%    params(4) ... maximum scattering angle limit (degrees)
%                 default = 60
%    params(5) ... width of angle limit taper (linear in cosine)
%                 default = min([0.15*params(4), 90-params(4)])
% NOTE: The angle limit described by params(4-5) is applied to both the
% source angle (angle w.r.t vertical of the vector from imagepoint to source)
% and the receiver angle (similarly defined). If either angle exceeds this
% limit, then the response is either damped or attenuated altogether.
%    paqrams(6) ... fmin, minimum frequency to migrate
%           default = 10 Hz
%    params(7) ... fmax, maximum frequency to migrate
%            default = 70% of Nyquist
%    params(8) ... incf, frequency increment (an integer). Every incf'th
%           frequency will be imaged.
%            default = 1
% NOTE: frequencies below fmin and above fmax are ignored
%    params(9) : time slice width (seconds)
%           default = 5 dominant periods = 10/(fmin+fmax);
%                  (If already a power of 2, it will not be extended).
%    params(10--12) : defines migration target window
%	 params(10) ... zmin of migration target window
%	          default = 0 
%	 params(11) ... tmax of migration target window
%	          default = something appropriate
%    params(12) ... xmin of target window
%	          default = min(x)
%    params(13) ... xmax of migration target window
%	          default = max(x) 
%    params(14) ... ymin of target window
%	          default = min(y)
%    params(15) ... ymax of migration target window
%	          default = max(y) 
%    params(16) ... dx = spatial grid size of migrated volume. Same in x
%               and y.
%             default = min([mean(diff(x)) mean(diff(y))])
%    params(17) ... ievery: write a message after computing this many
%               migrated traces
%             default = 100
%    params(18) ... ttflag: 0 means calculated traveltimes for each point,
%               1 means build a traveltime table and do table lookup.
%             default = 0
%
% zmig ... if specified on input, then it overrides params(9,10). This is a
%       vector of explicit depths to image at. That is, if specified, then
%       only these depths will be imaged. (if params(6)=0, then these are
%       interpreted as times.
%
% OUTPUT arguments
%
%    shotmig ... 2D matrix of the output migrated shot record
%    tmig/zmig ... t/z coordinates of migrated data (depends on params(6)
%    xmig ... x coordinates of migrated data (length(xmig)=size(shotmig,2))
%    ymig ... y coordinates of migrated data (length(ymig)=size(shotmig,2))
%    xgrid ... the unique x coordinates in the output grid
%    ygrid ... the unique y coordinates in the output grid
%
% By G.F. Margrave and J.K. Cooper
% CREWES Project, U of Calgary, 2007
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

%tstart=clock;
[nsamp,ntr]=size(shotrec);
nvsamp=length(vrms);

% check the validity input arguments

if(length(t)~=nsamp)
    error('Incorrect time specification')
end
t=t(:);
dt=t(2)-t(1);
nt=length(t);

if(length(x)~=ntr)
    error('Incorrect x specification for input data')
end

if(length(y)~=ntr)
    error('Incorrect y specification for input data')
end



%  ---- test velocity info ----

if(length(tv)~=nvsamp)
    error('Time vector for velocity model is incorrect')
end

if(min(tv)>min(t) || max(tv)<max(t))
    error('Time vector for velocity model must span that for data')
end


%  ---- examine parameters ----
nparams=18; 				% number of defined parameters
					
if(nargin<9) 				% no parameters inputted
	params= nan*ones(1,nparams); 
end 	 	

if(length(params)<nparams) 
	params = [params nan*ones(1,nparams-length(params))];
end

%assign parameter defaults

if( isnan(params(1)) ) 
	aper = sqrt((max(x)-min(x))^2+(max(y)-min(y))^2);
else
    aper = params(1);
end

if( isnan(params(2)) )
    width1 = aper/20;
else
    width1 = params(2);
end

if( isnan(params(3)) )
    itaper1 = 1;
else
    itaper1 = params(3);
end

if( isnan(params(4)) )
	ang_limit = pi/3;
else
	ang_limit = params(4)*pi/180;
end

if( isnan(params(5)) )
	width2 = 0.15*ang_limit;
    if(width2>pi/2-ang_limit)
        width2=pi/2-ang_limit;
    end
else
	width2 = params(5)*pi/180;
end
anglemax = ang_limit + width2;
cosanglemax=cos(anglemax);
cosanglimit=cos(ang_limit);
if( isnan(params(6)) )
	fmin=10;
else
	fmin = params(6);
end
if( isnan(params(7)) )
	fmax=.7/(2*dt);
else
	fmax = params(7);
end
if( isnan(params(8)) )
	incf=1;
else
	incf = params(8);
end
if(incf<1 || round(incf)~=incf)
    error('frequency increment must be a positive integer')
end
if( isnan(params(9)) )
    tslicewidth=10/(fmin+fmax);
else
	tslicewidth=params(9);
end

if( isnan(params(10)) ) 
		zmig1 = 0;
else
		zmig1 = params(10);
end

if( isnan(params(11)) ) 
		zmig2 = -1;%this is a flag to be adjusted later
else
		zmig2 = params(11);
end
if zmig2 < zmig1
    if(zmig2~=-1)
        error(['the target time window start time should be smaller than the end time !'...
                'i.e. paraams(8) < params(9)']);
    end
end

if( isnan(params(12)) ) 
		xmig1 = min(x);
else
		xmig1 = params(12);
end
if( isnan(params(13)) ) 
		xmig2 = max(x);
else
		xmig2 = params(13);
end
if xmig2 < xmig1
 	error(['the start x location of target trace range should be less than the end x location'...
        'i.e. params(11) < params(12)']);
end

if( isnan(params(14)) ) 
		ymig1 = min(y);
else
		ymig1 = params(14);
end
if( isnan(params(15)) ) 
		ymig2 = max(y);
else
		ymig2 = params(15);
end
if ymig2 < ymig1
 	error(['the start y location of target trace range should be less than the end y location'...
        'i.e. params(13) < params(14)']);
end

if(isnan(params(16)))
    dx=max([median(diff(x)) median(diff(y))]);
else
    dx=params(16);
end
if(dx<=0)
    error('output grid size must be greater than zero')
end
if(isnan(params(17)))
    ievery=100;
else
    ievery=params(17);
end

if(isnan(params(18)))
    ttflag=0;
else
    ttflag=params(18);
end

%one way time
dt1=.5*dt;

vmin=min(vrms);
vmax=max(vrms);
%output samples targeted
if(nargin<10)
    %build a zmig vector
    dz=vmin/(4*fmax);
    %estimate a zmax if need be
    if(zmig2==-1)
        zmig2=vmax*max(t)/2; %should use average velocity here but it won't matter much
    end
    zmig=(zmig1:dz:zmig2)';
else
    zmig=zmig(:);%Force column vector
    if(zmig2==-1)
        zmig2=max(zmig);
    end
    zmig1=min(zmig);
end
nz=length(zmig);

%compute maximum time needed. This is the traveltime for a scatterpoint
%when the source and receiver are colocated a distance aper away.
tmax=sqrt(zmig2^2 + 4*aper)/vmin;
%pad input to tmax
npad=ceil(tmax/dt1)-nsamp+5;
if( npad > 0)
	shotrec= [shotrec; zeros(npad,ntr)];
	t = [t;((nsamp+1:nsamp+npad)')*dt1];
end

%need average velocity
vave=vrms2vave(vrms,tv);
zv=tv.*vave/2;%depth axis for velocity function
%t1=pwlint(zv,tv,zmig)/2;%one way times to each output point
t1=interp1(zv,tv,zmig)/2;%one way times to each output point
t2=t1.^2;

%get rms and average velocities at each output depth
vrms_mig=pwlint(zv,vrms,zmig);
vrms_mig=vrms_mig(:);
vave_mig=pwlint(zv,vave,zmig);
vave_mig=vave_mig(:);
if(~ttflag) vrms_mig2=vrms_mig.^2; end
%tmig=2*zmig./vave_mig;


%build output grid
xgrid=min(x):dx:max(x);%row vector
ygrid=(min(y):dx:max(y))';%column vector
%find locations in target window
ixtarget= near(xgrid,xmig1,xmig2);
iytarget= near(ygrid,ymig1,ymig2)';
xmig=ones(size(iytarget))*xgrid(ixtarget);
ymig=ygrid(iytarget)*ones(size(ixtarget));
xmig=xmig(:)';
ymig=ymig(:)';
ntrmig=length(xmig);

if(ttflag)
    %compute traveltime table
    omax=sqrt((max(x)-min(x))^2+(max(y)-min(y))^2);%max offset
    do=dx/10;%sample increment in offset for table
    offs=(0:do:omax)';
    noffs=length(offs);
    offs=linspace(0,omax,noffs);
    do=offs(2);
    timetable=zeros(length(offs),length(zmig));
    for kz=1:length(zmig)
        timetable(:,kz)=sqrt(t2(kz)+offs.^2/vrms_mig(kz)^2);
    end
end

%Differentiate and phase rotate the shotrecord
nt2=2^nextpow2(nt);
[shotftmp,f]=fftrl(shotrec,t,0,nt2);
shotftmp=-2*pi*i*f(:,ones(1,length(x))).*shotftmp;
shotftmp(end,:)=0;%zero the nyquist
shotrec=ifftrl(shotftmp,f);

% ifuse=between(fmin,fmax,f);%indices of frequencies to use
% ifuse=ifuse(1:incf:end);
% omega=2*pi*f(ifuse);%column vector of frequencies
% shotf= omega(:,ones(1,length(x))).*shotftmp(ifuse,:).*exp(-i*pi/2); %multiply by omega
% eio=exp(i*omega);
% clear shotrec shotftmp f;
%initialize output array
shotmig=zeros(nz,ntrmig);

%
%loop over migrated traces
%

disp(' ');
disp([' --- Total number of input traces : ' int2str(ntr) ' ---']);
disp([' --- Total number of migrated traces : ' int2str(ntrmig) ' ---']);
if(ttflag)
    disp('Using traveltime table lookup')
else
    disp('Using traveltime calculations per sample')
end
disp(['Output grid spacing ' num2str(dx) ' length units'])
switch incf
    case {1}
        disp('Imaging every frequency')
    case{2}
        disp('Imaging every other frequency')
    case{3}
        disp('Imaging every third frequency')
    case{4}
        disp('Imaging every fourth frequency')
    case{5}
        disp('Imaging every fifth frequency')
    otherwise
        disp(['Trying something really ridiculous and imaging every '...
            int2str(incf) 'th frequency'])
end
clock1=cputime;
steptimes=nan*ones(size(ntrmig));
ntimes=0;
% loop over traces in aperture
% ntrmig=1;
% xmig=xshot;
% ymig=yshot;

x=x(:);%want coordinate vectors in columns
y=y(:);

for kmig=1:ntrmig 			% ktr--the index of the output trace
%for kmig=21 			% ktr--the index of the output trace
    xtr=xmig(kmig);%x coordinate of target
    ytr=ymig(kmig);%y coordinate of target

	%determine input traces in aperture
    inaperx=between(xtr-aper,xtr+aper,x,2);
    inapery=between(ytr-aper,ytr+aper,y(inaperx),2);
    inaper=inaperx(inapery);
    
    xnow=x(inaper);
    ynow=y(inaper);
    
	%shot offset
    if(ttflag)
        offsetshot=sqrt((xtr-xshot)^2+(ytr-yshot)^2);
    else
        offsetshot2=(xtr-xshot)^2+(ytr-yshot)^2;
    end
        
    %gather=zeros(length(zmig),length(inaper));
	for kz=1:length(zmig)
        %source vector: points from image point to source
        rs_vec=[(xshot-xtr)*ones(size(xnow)) (yshot-ytr)*ones(size(xnow)) -zmig(kz)*ones(size(xnow))];
        rs=sqrt(sum(rs_vec.^2,2));
        %cosine of the source angle
        cossource=zmig(kz)./rs;
        
        %receiver vector: points from image point to receiver
        rg_vec=[(xnow-xtr) (ynow-ytr) -zmig(kz)*ones(size(xnow))];
        rg=sqrt(sum(rg_vec.^2,2));%column vector of mag(receiver vector)
        %cosine of the receiver angle
        cosrec=zmig(kz)./rg;
        
        if(ttflag)
            %receiver offset
            offsetrec=sqrt((xtr-xnow)^2+(ytr-ynow)^2);
            % source-receiver travel time via double square root equation
            % look up times in table via offset
            ioffshot=round(offsetshot/do)+1;
            ioffrec=round(offsetrec/do)+1;
            tsr= timetable(ioffshot,kz)+timetable(ioffrec,kz)+100*eps;%column vector of traveltimes
        else
           %receiver offset
            offsetrec2=(xtr-xnow).^2+(ytr-ynow).^2; 
            tsr= sqrt(offsetshot2/vrms_mig2(kz) + t2(kz))+sqrt(offsetrec2/vrms_mig2(kz) + t2(kz))+100*eps;
        end
        
        %cosine of opening angle
        costwotheta=sum(rg_vec.*rs_vec,2)./(rg.*rs); %just the dot product formula
        costheta=sqrt((costwotheta+1)/2);

        %angle limit and the taper
        cosangle=min([cossource cosrec],[],2);

        ind1 = find( cosangle < cosanglimit);
        ind2 = find( cosangle(ind1) > cosanglemax );
        
        angleweight=ones(size(cosangle));
        angleweight(ind1)=zeros(size(ind1));
        angleweight(ind1(ind2))=(cosangle(ind1(ind2))-cosanglemax)/(cosanglimit-cosanglemax);
        
		% aperture taper
		aperweight = 1.0/length(inaper);%divide by number of traces in aperture
        xtest=aper-sqrt((xtr-xnow).^2+(ytr-ynow).^2);%distance of trace from edge of aper
        if xtest < width1 & xtest > 0
            if(itaper1==1)
                aperweight=(.5+.5*cos(pi*(width1-xtest)/width1))/length(inaper);
            else
                aperweight=(width1-xtest)/(width1*length(inaper));
            end
        elseif xtest < width1 & xtest <= 0
            aperweight=0;
        end
        
        %get a slice from the input data
        tsrrounded=round(tsr/dt)*dt;
        s=slicemat(shotrec(:,inaper),tsrrounded/dt+1,round(tslicewidth/(2*dt)),1);
        %transform the slice, windowed with a gaussian
        ntwid=size(s,1);%slice width in samples
        n2=2^nextpow2(ntwid);
        itrel=1:ntwid;%relative time index
        n0=(ntwid-1)/2+1; %relative index to center of slice
        trel=dt*(itrel-n0);
        gwin=exp(-((itrel-n0)/(n0/2)).^2)';
        [sftmp,f]=fftrl(s.*(gwin(:,ones(size(inaper)))),trel,0,n2);
        %get frequencies to use, etc....
        ifuse=between(fmin,fmax,f);%indices of frequencies to use
        ifuse=ifuse(1:incf:end);
        omega=2*pi*f(ifuse);%column vector of frequencies
        sf= sftmp(ifuse,:);
        %apply phase shift with times adjusted to the slice time.
        %tmp=real(sum(exp(i*omega*(tsr-tsrrounded-trel(1))').*sf(:,inaper)))';
        tmp=real(sum(exp(i*omega*(tsr-tsrrounded-trel(1))').*sf))';
        %see Bleistein, Cohen, and Stockwell p247 eqn 5.2.22
		shotmig(kz,kmig)= shotmig(kz,kmig)+sum(zmig(kz)*aperweight*tmp.*rs.*costheta...
            .*angleweight./(rg*vave_mig(kz)).^2);
        %gather(:,kaper)=tmp.*zmig.*rs.*costheta./(rg.*vave_mig).^2;
		
	end
	
    if(rem(kmig,ievery)==0)
	    disp([' Completed migrated trace no. ' ,int2str(kmig) ,' of ' int2str(ntrmig) ]);
        timenow=cputime-clock1;
        
        ntimes=ntimes+1;
        steptimes(ntimes)=timenow;
        if(ntimes>1)
            timeremaining = (length(x)/(length(inaper)+1))*(timenow-steptimes(ntimes-1))*(ntrmig-kmig)/ievery;
        else
            timeremaining = (length(x)/(length(inaper)+1))*timenow*(ntrmig-kmig)/ievery;
        end
        disp([' time so far ' int2str(timenow) ' S estimated remaining ' int2str(timeremaining) ' S' ]);
    end

end
ind=find(isnan(steptimes));
steptimes(ind)=[];

totaltime=cputime-clock1;
disp(['Total time required ' int2str(totaltime)])