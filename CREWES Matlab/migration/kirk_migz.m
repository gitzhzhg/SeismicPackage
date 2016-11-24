function [seismig,zmig,xmig]=kirk_migz(seis,vmodel,dt,dx,dz,params)
% KIRK_MIG2: full-featured post-stack Kirchhoff depth migration
% 
% [seismig,zmig,xmig]=kirk_migz(seis,vmodel,t,x,,dz,params)
%
% KIRK_MIGZ is a post-stack kirchhoff depth migration routine.
%
% seis ... matrix of zero offset data. One trace per column.
% vmodel ... input velocity model. This should me a matrix with the same
%       number of columns as seis (i.e. the same x coordinate)
% t ... if a scalar, this is the time sample rate in SECONDS.
%       If a vector, it gives the time coordinates for the rows of seis.
% x ... if a scalar, this is the spatial sample rate (in units 
%       consistent with the velocity information. If a vector, then
%       it gives the x coordinates of the columns of seis
% dz ... if a scalar, then the output depth sample rate
%         May also be a vector of output depths
%
% params ... vector of migration parameters. An entry of nan gets the
% default.
%
%    params(1--3) : migration aperture and its taper
%    params(1) ... physical length aperture
%	          default is the length of the section
%    params(2) ... width of the aperture taper
%                 default is 0.05*params(1)
%    params(3) ... = 0, linear taper
%                     = 1, cosine taper   
%                 default is 1 (cosine taper)
%    params(4-6) : angle limit in degree
%    params(4) ... maximum dip limte
%                 default = 60
%    params(5) ... width of angle limit taper
%                 default = 0.15*params(4)
%    params(6) ... taper type:
%                     = 0: linear taper;
%                     = 1: cosine taper.
%                 default = 1.
%    params(7) : relative to sample interpolation
%    params(7) ... = 1, linear interpolation
%                     = 2, cubic interpolation
%                     = 3, spline interpolation
%                     = 4, sinc interpolation
%                 default = 1
%    params(8--11) : relative to migration target window
%	 params(8) ... zmin of migration target window
%	          default = 0.0 
%	 params(9) ... zmax of migration target window
%	          default is vave_max*Tmax/2 where Tmax is the input record
%	          length and vave_max is the maximum average velocity in the
%	          input velocity model.
%    params(10) ... xm target window
%	          default is the minimum input coordinate 
%    params(11) ... xmax of migration target window
%	          default is the maximum input coordinate 
%
%    params(12) : box-car anti-aliasing filter
%    params(12) ... = 0, no box-car filter used;
%                      = 1, box-car filter will be used.
%                 default is 0.
%
% OUTPUT argument
%
%    seismig ... the output migrated time section
%    tmig ... t coordinates of migrated data
%    xmig ... x coordinates of migrated data
%
% By Xinxiang Li, modified from kirk.m by Dr. G.F. Margrave
% CREWES Project, U of Calgary, 1996
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
[nsamp,ntr]=size(seis);
[nvsamp,nvtr]=size(vmodel);

% check the validity input arguments

%  ---- check dt  ----
if(length(dt)>1)
	if(length(dt)~=nsamp)
		error('Incorrect time specification')
	end
	t=dt(:);
	[nrow,nvol] = size(t) ;
	if nrow < nvol
		t = t' ;
	end
	dt=t(2)-t(1);
else
 t=((0:nsamp-1)*dt)';
end

%  ---- checck dx ----
if(length(dx)>1)
	if(length(dx)~=ntr)
		error('Incorrect x specification')
	end
	x=dx;
	[nrow,nvol] = size(x) ;
	if nrow > nvol
		x = x' ;
	end
	dx=x(2)-x(1);
else
 x=(0:ntr-1)*dx;
end

%  ---- test velocity info ----

if( nvsamp==1 && nvtr==1)
    vmodel=vmodel*ones(nsamp,ntr);	
elseif(nvsamp==1 || nvtr==1)
	%might be transposed vector
    if(nvtr==nsamp)
        vmodel=vmodel';
        [nvsamp,nvtr]=size(vmodel);
    elseif(nvsamp~=nsamp)
        error('Velocity vector is wrong size');
    end
	%make velocity matrix
	vmodel=vmodel*ones(1,ntr);
else
	if(nvsamp~=nsamp)
		error('Velocity matrix has wrong number of rows');
	elseif(ntr~=nvtr)
		error('Velocity matrix has wrong number of columns');
	end
end
%
%ok, we now have a velocity matrix the same size as the data matrix
%

%  ---- examine parameters ----
nparams=12; 				% number of defined parameters
					
if(nargin<5) 				% no parameters inputted
	params= nan*ones(1,nparams); 
end 	 	

if(length(params)<nparams) 
	params = [params nan*ones(1,nparams-length(params))];
end

%assign parameter defaults

if( isnan(params(1)) ) 
	aper = abs(max(x)-min(x));
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
else
	width2 = params(5)*pi/180;
end
angle1 = ang_limit + width2;

if( isnan(params(6)) )
	itaper2 = 1;
else
	itaper2 = params(6);
end
if itaper2 ~= 1 && itaper2 ~= 0
	error('the angle limit taper type: params(6) should be 0 and 1 !');
end

if( isnan(params(7)) )
	interp_type = 1;
else
	interp_type = params(7);
end
if interp_type < 1 || interp_type > 4
	error('the interpolation indexx paarams(7) should be 1, 2, 3 and 4 !');
end

if( isnan(params(8)) ) 
		tmig1 = min(t);
else
		tmig1 = params(8);
end

if( isnan(params(9)) ) 
		tmig2 = max(t);
else
		tmig2 = params(9);
end
if tmig2 < tmig1
	error(['the target time window start time should be smaller than the end time !' ...
        ' i.e. paraams(8) < params(9)']);
end

if( isnan(params(10)) ) 
		xmig1 = min(x);
else
		xmig1 = params(10);
end
if( isnan(params(11)) ) 
		xmig2 = max(x);
else
		xmig2 = params(11);
end
if xmig2 < xmig1
 	error(['the start location of target trace range should be prerior to the end location'...
        ' i.e. params(10) < params(11)']);
end

if( isnan(params(12)) )
		ibcfilter = 0;
else
		ibcfilter = params(12);
end

if ibcfilter 
	% get a cumulative array from seis
	arycum=cumsum(seis);



end
	

%aperture in traces and the taper coefficient
traper0 = .5*aper/dx;
traper1 = width1/dx;
traper = round(traper0+traper1);
if itaper1 == 0
	coef1 = lin_taper(traper0,traper0+traper1);
else
	coef1 = cos_taper(traper0,traper0+traper1);
end

%one way time
dt1=.5*dt;
t1=t/2;
t2= t1.^2;

%compute maximum time needed
vmin=min(min(vmodel));
%vmax=max(max(vmodel));
tmax=sqrt( .25*tmig2^2 + ((.5*aper+width1)/vmin)^2);

%pad input to tmaxin
npad=ceil(tmax/dt1)-nsamp+5;
if( npad > 0)
	seis= [seis; zeros(npad,ntr)];
	t1 = [t1',(nsamp+1:nsamp+npad)*dt1]';
	if ibcfilter
		for j=1:npad
			arycum=[arycum; arycum(nsamp,:)];
		end
	end
end

%output samples targeted
samptarget=near(t,tmig1,tmig2);
tmig=t(samptarget);

%output traces desired
trtarget= near(x,xmig1,xmig2);
xmig=x(trtarget);

%initialize output array
seismig=zeros(length(samptarget),length(trtarget));

%loop over migrated traces
%
kmig=0;

disp([' ']);
disp([' --- Total number of traces to be migrated : ' int2str(length(trtarget)) ' ---']);
disp([' ']);

clock1=clock;

for ktr=trtarget 			% ktr--the location of output trace
	kmig=kmig+1; 			% numerator

	%determine traces in aperture
	n1=max([1 ktr-traper]);
	n2=min([ntr ktr+traper]);
	truse=n1:n2;

	%offsets and velocity 
	offset2=((truse-ktr)*dx).^2;
	v2 = vmodel(:,ktr).^2;
	
	% loop over traces in aperture
    if(rem(kmig,20)==0)
        timenow=clock;
        timeused=etime(timenow,clock1);
        timeremaining=(timeused/kmig)*(max(trtarget)-ktr);
	    disp([' Migrated trace no.' ,int2str(kmig) ,' of ' int2str(length(trtarget)) ', The traces in aperture : ' ,int2str(length(truse))]);
        disp(['Estimated time remaining ' int2str(timeremaining) ' seconds'])
    end
	for kaper=1:length(truse)
	
		% offset times
		t_aper = sqrt( offset2(kaper)./v2(samptarget) + t2(samptarget) );

		%cosine theta amplitude correction
% 		if truse(kaper) == ktr
% 			costheta = ones(size(samptarget'));
% 			tanalpha = zeros(size(samptarget'));
% 		else
			costheta = 0.5*tmig./(t_aper+100*eps);
            costheta(1)=1;%otherwise we get nan
			tanalpha = sqrt(1-costheta.^2);
					
			%angle limit and the taper
		
			ind = find( costheta < cos(angle1) );
            if(~isempty(ind))
                i1 = ind(length(ind));
                ind = find( costheta < cos(ang_limit) );
                i2 = ind(length(ind));

                if i1 < i2
                        if itaper2  ==  0
                        coef2 = lin_taper(i2,i1);
                    else
                        coef2 = cos_taper(i2,i1);
                    end
                    costheta(1:i1) = zeros(i1,1);
                    costheta(i1+1:i2) = coef2(i2-i1:-1:1)'.*costheta(i1+1:i2);
                end
            end
% 		end
	
		% boxcar anti-aliasing filter
		if ibcfilter
			lt0=round((dx*tanalpha./vmodel(samptarget,ktr)/dt1));
			indt = round((t_aper/dt1))+1;
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
			tmp0=t1;
			tmp0(1) = arycum(1,truse(kaper));
			ind = 2:lentr;
			tmp0(ind) = (arycum(l2(ind),truse(kaper))-arycum(l1(ind),truse(kaper)))...
				    ./(l2(ind)-l1(ind));
		else
			tmp0 = seis(:,truse(kaper));
		end	

		%interpolation
		% Linear
		if interp_type == 1
			tnumber = t_aper/dt1;
			it0 = floor( tnumber ) + 1;
			it1 = it0+1; 
			xt0 = tnumber - it0 + 1;
			xt1 = it0-tnumber;
			tmp = xt1.*tmp0(it0)+xt0.*tmp0(it1);
		end
		% Spline
		if interp_type == 2
			tmp = interp1(t1,tmp0,t_aper,'spline');
		end
		% Cubic
		if interp_type == 3
			tmp = interp1(t1,tmp0,t_aper,'cubic');
		end
		% Sinc
		if interp_type == 4
			tmp = sinci(tmp0,t1,t_aper);
		end

		% aperture taper
		ccoef = 1. ;
		if abs(truse(kaper)-ktr)*dx > 0.5*aper
			ccoef = coef1( round(abs(truse(kaper)-ktr)-traper0) );
		end
		if abs(1-ccoef) > 0.05
			tmp = tmp .* ccoef;
		end
		
		ind = find( costheta < 0.999);
		costheta(ind) = sqrt(costheta(ind).^3);
		tmp(ind) = tmp(ind) .* costheta(ind);

		seismig(:,kmig)= seismig(:,kmig)+tmp;
		
	end
	
	% scaling and 45 degree phase shift
	scalemig = vmodel(samptarget,kmig).*sqrt(pi.*(tmig+0.0001)) ;
	seismig(:,kmig) = seismig(:,kmig)./scalemig ;

end

time=etime(clock,clock1);
disp(['migration completed in ' int2str(time) ' seconds'])