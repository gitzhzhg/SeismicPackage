function logout=logtotime2(login,tzobj,dtout,x)
% logout=logtotime(login,tzobj,dtout,x)
% logout=logtotime(login,tzobj,dtout)
%
% LOGTOTIME converts a well log from depth to time given a time-depth
% (tz) function. The input arguments can have a variety of forms as 
% detailed below:
%
%	login ... this can be a Random Earth Object as used by LOGSEC or
%			a simple [n,2] matrix where the first column is depth
%			and the second is the log samples. Must be regularly sampled
%	tzobj ... this can be a time-depth object as used by LOGSEC, or a
%			simple [m,2] matrix where the first column is depth and the
%			second is time
%	dtout ... desired time sample rate of the output log. Internally, the
%			log will be converted to time at whatever sample rate is 
%			needed to avoid aliasing and then resampled (with a 
%			zero phase antialias filter if needed) to dtout.
%			(Note that the output samples will always lie on a grid which
%			includes t=0. While the time range of the samples may lie outside
%			t=0, if that range is extrapolated the grid will intercept t=0.)
%		NOTE:setting dtout to -1 will cause the algorithm to determine dtout
%			as the finest sample rate needed to avoid aliasing.
%	x ... the x coordinate of the well. If the log is off the ends of the
%		tz section, then constant extrapolation of the closest tz curve
%		occurs. Hence the default for x is appropriate with a single tz
%		curve.
%    ******************** default x=0 ******************
%
%	logout ... a log object of the same type as input. If both the log and
%		the tz functions were objects then the logout will have its datatype
%		set to 'tlog' and its username will be the id of the tzobj (defined 
%		to be tzid=objget(tzobj,'objmodified') ). If login was a simple 2
%		column matrix, then this will be also with the first column time
%		and the second samples.
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
%tic
if(nargin<4) x=0; end
if( isearthobj(login) )
	logobj=1;
	zsamps=objget(login,'samples');
	zlog=objget(login,'depth');
else
	logobj=0;
	zsamps=login(:,2);
	zlog=login(:,1);
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
%disp('tz function determined')
%toc
%tic
% ok, determine the internal sample rate
%vins=2*diff(zt1)./diff(tz1);
dz=zlog(2)-zlog(1);
%dt=2*dz/max(vins);
if(isempty(zt2))
	%
	%determine the time axis for the output log. We make sure that the time
	%samples fall evenly on a grid which includes t=0
	%
	%map the end samples of the log
	%
	tends=interpextrap(zt1,tz1,[zlog(1) zlog(length(zlog))]);
	tmin=dtout*ceil(tends(1)/dtout);
	tmax=dtout*floor(tends(2)/dtout);
	tlog=tmin:dtout:tmax;
	%determine the time each depth maps to
	tlogz=interpextrap(zt1,tz1,zlog);
else
	
	% determine the time coordinate for the log intermediate between the two
	% tz functions
	tends=interpextrap(zt1,tz1,[zlog(1) zlog(length(zlog))]);
	tends2=interpextrap(zt2,tz2,[zlog(1) zlog(length(zlog))]);
	tends=f1*tends+f2*tends2;
	tmin=dtout*ceil(tends(1)/dtout);
	tmax=dtout*floor(tends(2)/dtout);
	tlog=tmin:dtout:tmax;
	%determine the interpolation sites on the first function
		
	tlogz=interpextrap(zt1,tz1,zlog);
	%determine the interpolation sites
	tlogz2=interpextrap(zt2,tz2,zlog);
	%average the results from the two functions
	tlogz=f1*tlogz+f2*tlogz2;
end
%disp('interpolation sites determined');
%toc
%tic
% now round each t to nearest time sample
tlogz=dtout*round(tlogz/dtout);
%loop over output time samples and average the log samples
tsamps=nan*zeros(size(tlog));
for k=1:length(tlog)
	ind=find(tlogz==tlog(k));
	if(~isempty(ind))
		tsamps(k)=mean(zsamps(ind));
	end
end
%toc
%% added sat 30 Sept %%
if(~isempty(isnan(tsamps)))
	tsamps=fillholes(tsamps,'constant');
end
		
%repackage the log as an object of the same type that was received
if(logobj)
	logout=randobj(objget(login,'name'),'tlog');
	if(~isempty(tzid))
		logout=objset(logout,'username',tzid);
	end
	logout=objset(logout,'samples',tsamps);
	logout=objset(logout,'time',tlog);
else
	logout=[tlog(:) tsamps(:)];
end
%disp('object made')
%toc