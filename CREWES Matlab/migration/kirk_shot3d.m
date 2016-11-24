function [shotmig,tmig,xmig,ymig,xgrid,ygrid]=kirk_shot3d(shotrec,t,x,y,xshot,yshot,velmod,tv,xv,yv,params)
% KIRK_SHOT3D: Kirchhoff prestack shot-record time migration in 3D   
% 
% [shotmig,tmig,xmig,ymig,xgrid,ygrid]=kirk_shot3d(shotrec,t,x,y,xshot,yshot,velmod,tv,xv,yv,params)
%
% KIRK_SHOT3D migrates a single 3D shot record using prestack time
% migration. The 3D shot record is stored in a 2D matrix and it is not
% necessary that the traces fall on a regular spatial grid.
% The algorithm is simple traveltime path summation with a few
% options as described in the parameters vector. Traveltime from source to
% scatterpoint (i.e the image point) is approximated by a Dix equation
% using the rms velocity from the model at the lateral position halfway
% between source and receiver and at the vertical traveltime of the 
% scatterpoint. Similarly, from the scatterpoint to a receiver, a Dix 
% equation using the rms velocity at halfway between scatterpoint and 
% receiver is used. There is no topographic compensation at present. The
% source and all receivers are assumed to be on the same horizontal plane.
% The Kirchhoff integral weighting is done as described in Bleistein, Cohen,
% and Stockwell p247 eqn 5.2.22. Although this derivation is for constant
% velocity, the adjustments for time migration are reasonably obvious. The
% algorithm works entirely in the time domain including the sample
% interpolation.
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
% velmod ... 3D velocity model. This is a 3D matrix of RMS velocities as a
%           function of lateral position and time. For a constant velocity
%           migration this may be input as a single scalar. For a v(t)
%           migration, this may be input as a column vector (time series). 
%           In these latter two cases, it is internally expanded into a 3D volume.
% tv ... time coordinate vector for velmod
%        ignored if velmod input as scalar.
%           Requirement: length(tv)=size(velmod,1);
% xv ... space coordinate vector for velmod
%        ignored if velmod input as scalar or vector
%           Requirement: length(xv)=size(velmod,2);
% yv ... space coordinate vector for velmod
%        ignored if velmod input as scalar or vector
%           Requirement: length(yv)=size(velmod,3);
% REQUIREMENT: the span of tv, xv and yv must equal or exceed that of t, x and y.
% NOTE: While the data may be spatially irregular, the velocity model must
% be regular.
% NOTE: when a velocity function is required at a location not directly
% given in velmod, nearest-neighbor interpolation is used.
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
%    params(6) ... not used
%    params(7) : determine how seismic amplitudes are interpolated
%    params(7) ... = 1, linear interpolation
%                     = 2, cubic interpolation
%                     = 3, spline interpolation
%                     = 4, sinc interpolation
%                 default = 1
%    params(8--11) : defines migration target window
%	 params(8) ... tmin of migration target window
%	          default = min(tv) 
%	 params(9) ... tmax of migration target window
%	          default = max(tv)
%    params(10) ... xmin of target window
%	          default = min(xv)
%    params(11) ... xmax of migration target window
%	          default = max(xv) 
%    params(12) ... ymin of target window
%	          default = min(yv)
%    params(13) ... ymax of migration target window
%	          default = max(yv) 
%    params(14) ... dx = spatial grid size of migrated volume. Same in x
%               and y.
%             default = min([mean(diff(x)) mean(diff(y))])
%
%    params(15) : box-car anti-aliasing filter
%    params(15) ... = 0, no box-car filter used;
%                      = 1, box-car filter will be used.
%                 default is 0.
%
% OUTPUT arguments
%
%    shotmig ... 2D matrix of the output migrated shot record
%    tmig ... t coordinates of migrated data (depends on params(6)
%    xmig ... x coordinates of migrated data (length(xmig)=size(shotmig,2))
%    ymig ... y coordinates of migrated data (length(ymig)=size(shotmig,2))
%    xgrid ... the unique x coordinates in the output grid
%    ygrid ... the unique y coordinates in the output grid
%
% By G.F. Margrave and J.K. Cooper
% CREWES Project, U of Calgary, 2007-2008
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
[nvsamp,nvx,nvy]=size(velmod);

% check the validity input arguments

if(length(t)~=nsamp)
    error('Incorrect time specification')
end
t=t(:);
dt=t(2)-t(1);

if(length(x)~=ntr)
    error('Incorrect x specification for input data')
end

if(length(y)~=ntr)
    error('Incorrect y specification for input data')
end



%  ---- test velocity info ----
%test for alternate velocity input
if(nvsamp*nvx*nvy==1)
    %scalar velocity input
    velmod=velmod*ones(nsamp,2,2);
    xv=[min(x) max(x)];
    yv=[min(y) max(y)];
    nvx=2;nvy=2;
    tv=t;
end
if(nvsamp~=1 && nvx*nvy==1)
    %v(t) input
    tmp=velmod;
    velmod=zeros(length(tmp),2,2);
    velmod(:,1,1)=tmp;
    velmod(:,1,2)=tmp;
    velmod(:,2,1)=tmp;
    velmod(:,2,2)=tmp;
    xv=[min(x) max(x)];
    yv=[min(y) max(y)];
    nvx=2;nvy=2;
end
        
if(length(tv)~=nvsamp)
    error('Time vector for velocity model is incorrect')
end
if(length(xv)~=nvx)
    error('Space coordinate vector xv for velocity model is incorrect')
end
if(length(yv)~=nvy)
    error('Space coordinate vector yv for velocity model is incorrect')
end
if(min(tv)>min(t) || max(tv)<max(t))
    error('Time vector for velocity model must span that for data')
end
if(min(xv)>min(x) || max(xv)<max(x))
    error('Space vector xv for velocity model must span vector x for data')
end
if(min(yv)>min(y) || max(yv)<max(y))
    error('Space vector yv for velocity model must span vector y for data')
end

%  ---- examine parameters ----
nparams=15; 				% number of defined parameters
					
if(nargin<11) 				% no parameters inputted
	params= nan*ones(1,nparams); 
end 	 	

if(length(params)<nparams) 
	params = [params nan*ones(1,nparams-length(params))];
end

%assign parameter defaults

if( isnan(params(1)) ) 
	aper = sqrt((max(xv)-min(xv))^2+(max(yv)-min(yv))^2);
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


if( isnan(params(7)) )
	interp_type = 1;
else
	interp_type = params(7);
end
if interp_type < 1 || interp_type > 4
	error('the interpolation index params(7) should be 1, 2, 3 and 4 !');
end

if( isnan(params(8)) ) 
		tmig1 = min(tv);
else
		tmig1 = params(8);
end

if( isnan(params(9)) ) 
		tmig2 = max(tv);
else
		tmig2 = params(9);
end
if tmig2 < tmig1
	error(['the target time window start time should be smaller than the end time !'...
            'i.e. paraams(8) < params(9)']);
end

if( isnan(params(10)) ) 
		xmig1 = min(xv);
else
		xmig1 = params(10);
end
if( isnan(params(11)) ) 
		xmig2 = max(xv);
else
		xmig2 = params(11);
end
if xmig2 < xmig1
 	error(['the start x location of target trace range should be less than the end x location'...
        'i.e. params(10) < params(11)']);
end

if( isnan(params(12)) ) 
		ymig1 = min(yv);
else
		ymig1 = params(12);
end
if( isnan(params(13)) ) 
		ymig2 = max(yv);
else
		ymig2 = params(13);
end
if ymig2 < ymig1
 	error(['the start y location of target trace range should be less than the end y location'...
        'i.e. params(12) < params(13)']);
end

if(isnan(params(14)))
    dx=max([median(diff(x)) median(diff(y))]);
else
    dx=params(14);
end
if(dx<=0)
    error('output grid size must be greater than zero')
end
if( isnan(params(15)) )
		ibcfilter = 0;
else
		ibcfilter = params(15);
end

if ibcfilter 
	% get a cumulative array from shotrec
	arycum=cumsum(shotrec);
end

%one way time
dt1=.5*dt;
t1=t/2;
nt=length(t);

%compute maximum time needed. This is the traveltime for a scatterpoint
%when the source and receiver are colocated a distance aper away.
vmin=min(velmod(:));
tmax=sqrt(tmig2^2 + (2*aper/vmin)^2);

%pad input to tmaxin
npad=ceil(tmax/dt1)-nsamp+5;
if( npad > 0)
	shotrec= [shotrec; zeros(npad,ntr)];
	t1 = [t1',(nsamp+1:nsamp+npad)*dt1]';
	if ibcfilter
        arycum=[arycum; ones(npad,1)*arycum(nsamp,:)];
	end
end
t2= t1.^2;

%output samples targeted
samptarget=near(t,tmig1,tmig2);
tmig=t(samptarget);

%build output grid
xgrid=min(xv):dx:max(xv);%row vector
ygrid=(min(yv):dx:max(yv))';%column vector
%find locations in target window
ixtarget= near(xgrid,xmig1,xmig2);
iytarget= near(ygrid,ymig1,ymig2)';
xmig=ones(size(iytarget))*xgrid(ixtarget);
ymig=ygrid(iytarget)*ones(size(ixtarget));
xmig=xmig(:)';
ymig=ymig(:)';
ntrmig=length(xmig);

%Differentiate and phase rotate the shotrecord
nt2=2^nextpow2(nt);
[shotftmp,f]=fftrl(shotrec,t,0,nt2);
shotftmp=-2*pi*i*f(:,ones(1,length(x))).*shotftmp;
shotftmp(end,:)=0;%zero the nyquist
shotrec=ifftrl(shotftmp,f);

%initialize output array
shotmig=zeros(length(samptarget),ntrmig);

%loop over migrated traces
%

disp(' ');
disp([' --- Total number of migrated traces : ' int2str(ntrmig) ' ---']);
disp(' ');

clock1=cputime;
steptimes=nan*ones(size(ntrmig));
ntimes=0;
ievery=20;%print a progress message every this many traces
%save1=zeros(size(ntrmig));
% loop over traces in aperture

for kmig=1:ntrmig 			% kmig--the index of the output trace
    xtr=xmig(kmig);%x coordinate of target
    ytr=ymig(kmig);%y coordinate of target

	%determine input traces in aperture
    inaperx=between(xtr-aper,xtr+aper,x,2);
    inapery=between(ytr-aper,ytr+aper,y(inaperx),2);
    inaper=inaperx(inapery);
    
    %need average velocity at output point
    ivx=near(xv,xtr);
    ivy=near(yv,ytr);
    vrms_mig=velmod(:,ivx(1),ivy(1));
    vave_mig=vrms2vave(vrms_mig,tv);
    zmig=t1(samptarget).*vave_mig(samptarget);%depths at output point
    
	%shot offset and velocity 
    offsetshot2=(xtr-xshot)^2+(ytr-yshot)^2;
    xvshotside=(xshot+xtr)/2;
    yvshotside=(yshot+ytr)/2;
    ivshotx=near(xv,xvshotside);
    ivshoty=near(yv,yvshotside);
	vshot2 = velmod(:,ivshotx(1),ivshoty(1)).^2;
    
    %source vector: points from image point to source
    rs_vec=[(xshot-xtr)*ones(size(zmig)) (yshot-ytr)*ones(size(zmig)) -zmig];
    rs=sqrt(sum(rs_vec.^2,2));
    %cosine of the source angle
    cossource=zmig./(rs+100*eps);
        
    %gather=zeros(length(tmig),length(inaper));
	for kaper=1:length(inaper)
        xnow=x(inaper(kaper));
        ynow=y(inaper(kaper));
        %receiver offset and velocity
        offsetrec2=(xtr-xnow)^2+(ytr-ynow)^2;
        xvrecside=(xnow+xtr)/2;
        yvrecside=(ynow+ytr)/2;
        ivrecx=near(xv,xvrecside);
        ivrecy=near(yv,yvrecside);
        vrec2 = velmod(:,ivrecx(1),ivrecy(1)).^2;
        
        %receiver vector: points from image point to receiver
        rg_vec=[(xnow-xtr)*ones(size(zmig)) (ynow-ytr)*ones(size(zmig)) -zmig];
        rg=sqrt(sum(rg_vec.^2,2));
        %cosine of the receiver angle
        cosrec=zmig./(rg+100*eps);
        
		% source-receiver travel time via double square root equation
        % "t2" is the squared one-way vertical time at the output point
        tsmig=sqrt(offsetshot2./vshot2(samptarget) + t2(samptarget)); %time from shot to mig point
        trmig=sqrt(offsetrec2./vrec2(samptarget) + t2(samptarget));%time from receiver to mig point
        tsr= tsmig+trmig+100*eps;
       
        %cosine of opening angle
        costwotheta=sum(rg_vec.*rs_vec,2)./(rg.*rs+100*eps); %just the dot product formula
        costheta=sqrt((costwotheta+1)/2);
        
        %angle limit and the taper
        cosangle=min([cossource cosrec],[],2);

        ind1 = find( cosangle < cos(ang_limit));
        ind2 = find( cosangle(ind1) > cos(anglemax) );
        
        angleweight=ones(size(cosangle));
        angleweight(ind1)=0;
        angleweight(ind1(ind2))=(cosangle(ind1(ind2))-cos(anglemax))/(cos(ang_limit)-cos(anglemax));
       
	
		% boxcar anti-aliasing filter
		if ibcfilter
			lt0=round((dx*tanalpha./velmod(samptarget,kmig)/dt));
			indt = round((tsr/dt))+1;
			lentr = nsamp+npad;
			lt = ones(lentr,1)*max(lt0);
			lt(indt)=lt0;
			lt(max(indt)+1:lentr) = ones(lentr-max(indt),1)*min(lt0);
			it = (1:lentr)';
			l1=it-lt-1;
			l2=it+lt;
			ind = find(l1 < 1);
			l1(ind) = ones(length(ind),1);
			ind = find(l2> lentr);
			l2(ind)=ones(length(ind),1)*lentr;
			tmp0=t;
			tmp0(1) = arycum(1,inaper(kaper));
			ind = 2:lentr;
			tmp0(ind) = (arycum(l2(ind),inaper(kaper))-arycum(l1(ind),inaper(kaper)))...
				    ./(l2(ind)-l1(ind));
		else
			tmp0 = shotrec(:,inaper(kaper));
		end	

		%interpolation
		% Linear
		if interp_type == 1
			tnumber = tsr/dt;
			it0 = floor( tnumber ) + 1;
			it1 = it0+1; 
			xt0 = tnumber - it0 + 1;
			xt1 = it0-tnumber;
			tmp = xt1.*tmp0(it0)+xt0.*tmp0(it1);
		end
		% Spline
		if interp_type == 2
			tmp = interp1(t,tmp0,tsr,'spline');
		end
		% Cubic
		if interp_type == 3
			tmp = interp1(t,tmp0,tsr,'cubic');
		end
		% Sinc
		if interp_type == 4 %#ok<ALIGN>
			tmp = sinci(tmp0,t,tsr);
        end


		% aperture taper
		aperweight = 1.0/length(inaper);
        xtest=abs(aper-abs(xtr-xnow));%distance of trace from edge of aper
		if xtest < width1 %#ok<ALIGN>
            if(itaper1==1)
                aperweight=(.5+.5*cos(pi*(xtest-width1)/(180*width1)))/length(inaper);
            else
                aperweight=(xtest-width1)/(width1*length(inaper));
            end
        end
		tmp = tmp .* aperweight.*angleweight;
		
        %see Bleistein, Cohen, and Stockwell p247 eqn 5.2.22
		tmp = tmp.*zmig.*rs.*costheta./(rg.*vave_mig(samptarget)+100*eps).^2;
        %save1(kmig)=save1(kmig)+sum(costheta);
        %gather(:,kaper)=tmp;
        %ind=find(isnan(tmp),1);
		shotmig(:,kmig)= shotmig(:,kmig)+tmp;
		
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
        disp([' time so far ' num2str(timenow) ' estimated remaining ' num2str(timeremaining) ]);
    end

end
%ind=find(isnan(steptimes));
%steptimes(ind)=[];

totaltime=cputime-clock1;
disp(['Total time required ' num2str(totaltime)])