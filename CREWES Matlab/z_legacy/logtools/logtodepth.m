function logout=logtodepth(login,tzobj,dzout,znot,x)
% logout=logtodepth(login,tzobj,dzout,znot,x)
% logout=logtodepth(login,tzobj,dzout,znot)
% logout=logtodepth(login,tzobj,dzout)
%
% LOGTODEPTH converts a well log from  time to depth given a time-depth
% (tz) function. The input arguments can have a variety of forms as 
% detailed below:
%
%	login ... this can be a Random Earth Object as used by LOGSEC or
%		a simple [n,2] matrix where the first column is depth
%		and the second is the log samples. Must be regularly sampled
%	tzobj ... this can be a time-depth object as used by LOGSEC, or a
%		simple [m,2] matrix where the first column is depth and the
%		second is time
%	dzout ... desired time sample rate of the output log. Internally, the
%		log will be converted to depth at whatever sample rate is 
%		needed to avoid aliasing and then resampled (with a 
%		zero phase antialias filter if needed) to dzout.
%	znot ... determines the depth grid for the output log. The output depth
%		samples will lie on a grid which includes znot. (Note that znot
%		may lie well outside of the output depth range, but if that range
%		is 'extrapolated', the grid will intercept znot.)
%	********************* default = 0 *******************
%	x ... the x coordinate of the well. If the log is off the ends of the
%		tz section, then constant extrapolation of the closest tz curve
%		occurs. Hence the default for x is appropriate with a single tz
%		curve.
%    ******************** default x=0 ******************
%
%	logout ... a log object of the same type as input. If both the log and
%		the tz functions were objects then the logout will have its datatype
%		set to 'zlog' and its username will be the id of the tzobj (defined 
%		to be tzid=objget(tzobj,'objmodified') )
%
% G.F. Margrave, March 1994
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
if(nargin<5) x=0; end
if(nargin<4) znot=0; end
if( isearthobj(login) )
	logobj=1;
	tsamps=objget(login,'samples');
	tlog=objget(login,'time');
else
	logobj=0;
	tsamps=logobj(:,2);
	tlog=logobj(:,1);
end
% 
% LOGSEC tz objects are container objects with datatype 'tzsc'. The container
% has the following attributes:
%	'tmatrix' ... stores a matrix of [nz,nx] of the times for the time depth curves
%   'zmatrix' ... stores a matrix of [nz,nx] of the depths for the time depth curves
%	'x' ... stores a vector of length nx of the x coordinates of the curves
%
if( isearthobj(tzobj) )
	tmtx=objget(tzobj,'tmatrix');
	zmtx=objget(tzobj,'zmatrix');
	xtz=objget(tzobj,'x');
	tzid=objget(tzobj,'objmodified');
else
	tmtx=tzobj(:,2);
	zmtx=tzobj(:,1);
	tzid=[];
end
% determine the tz function to use
 [nlegs,ntz]=size(tmtx);
 if( ntz==1 )
 	tz1=tmtx;
 	zt1=zmtx;
		tz2=[];
		zt2=[];
 else
	% check for exact equality
	ind=find(xtz==x);
	if(~isempty(ind))
		tz1=tmtx(:,ind);
		zt1=zmtx(:,ind);
		tz2=[];
		zt2=[];
	else
		
		%make sure its ordered
		[xtz,ix]=sort(xtz);
		ind=surround(xtz,x);
		%end cases
		if(isempty(ind))
			if(x<xtz(1))
				tz1=tmtx(:,ix(1));
				zt1=zmtx(:,ix(1));
				tz2=[];
				zt2=[];
			else
				tz1=tmtx(:,ix(ntz));
				zt1=zmtx(:,ix(ntz));
				tz2=[];
				zt2=[];
			end
		else
			%keep two functions
			zt1=zmtx(:,ix(ind));
			tz1=tmtx(:,ix(ind));
			zt2=zmtx(:,ix(ind+1));
			tz2=tmtx(:,ix(ind+1));
			f2=-(x-xtz(ind))/(xtz(ind)-xtz(ind+1));
			f1=-(x-xtz(ind+1))/(xtz(ind+1)-xtz(ind));
		end
	end
end
% ok, determine the internal sample rate
vins=2*diff(zt1)./diff(tz1);
dt=tlog(2)-tlog(1);
dz=dt*min(vins)/2;
if(isempty(zt2))
	
	%
	%determine the time axis for the output log. We make sure that the time
	%samples fall evenly on a grid which includes t=0
	%
	%map the end samples of the log
	%
	zends=interpextrap(tz1,zt1,[tlog(1) tlog(length(tlog))]);
	zmin=dz*ceil(zends(1)/dz);
	zmax=dz*floor(zends(2)/dz);
	zlog=zmin:dz:zmax;
	%determine the interpolation sites
	tint=interpextrap(zt1,tz1,zlog);
else
	
	% determine the depth coordinate for the log intermediate between the two
	% tz functions
	zends=interpextrap(tz1,zt1,[tlog(1) tlog(length(tlog))]);
	zends2=interpextrap(tz2,zt2,[tlog(1) tlog(length(tlog))]);
	zends=f1*zends+f2*zends2;
	zmin=dz*ceil(zends(1)/dz);
	zmax=dz*floor(zends(2)/dz);
	zlog=zmin:dz:zmax;
	%determine the interpolation sites on the first function
		
	tint=interpextrap(zt1,tz1,zlog);
	%determine the interpolation sites
	tint2=interpextrap(zt2,tz2,zlog);
	%average the results from the two functions
	tint=f1*tint+f2*tint2;
end
%ok sinc function interpolation of the depth trace to create the time trace
zsamps=sincinan(tsamps,tlog,tint);
%resample to dzout
zmin=dzout*ceil((zlog(1)-znot)/dzout)+znot;
zmax=dzout*floor((zlog(length(zlog))-znot)/dzout)+znot;
ilive=find(~isnan(zsamps));
zm=mean(zsamps(ilive));
[zsamps,zlog]=resamp(zsamps-zm,zlog,dzout,[zlog(1) zlog(length(zlog))],0);
zsamps=zsamps+zm;
%repackage the log as an object of the same type that was received
if(logobj)
	logout=randobj(objget(login,'name'),'zlog');
	if(~isempty(tzid))
		logout=objset(logout,'username',tzid);
	end
	logout=objset(logout,'samples',zsamps);
	logout=objset(logout,'depth',zlog);
else
	logout=[zsamps(:) zlog(:)];
end