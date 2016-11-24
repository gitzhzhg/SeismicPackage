function topout=toptotime(topin,tzobj)
% topout=toptotime(topin,tzobj)
%
% TOPTOTIME converts an array of well tops from depth to time given a
% time-depth (tz) function. The input arguments can have a variety of forms
% as detailed below:
%
%	topin ... a simple [n] vector of depths of the tops
%	tzobj ... this can be a time-depth object as used by LOGSEC, or a
%	          simple [m,2] matrix where the first column is depth and
%	          the second is time
%	topout ... an array of times of tops of the same type as topin. 
%
% T.N. Bishop, July 94 
%  (based on G. Margrave's logtotime.m)
%
% LOGSEC tz objects are container objects with datatype 'tzsc'. The container
% has the following attributes:
%	'tmatrix' ... stores a matrix of [nz,nx] of the times for the time
%	              depth curves
%   'zmatrix' ... stores a matrix of [nz,nx] of the depths for the time
%                 depth curves
%	'x' ... stores a vector of length nx of the x coordinates of the curves
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
%      the above code mostly taken from G.Margrave's
%      logtotime.m
%      i dont know what it does, but will leave it in
%      there for now since what i am using is the first
%      simple case where tz1=tmtx, zt1=zmtx
%      if this is a problem later on, take it out
%
  ntops = length(topin);
  if(ntops<1)
    fprintf(' no.of tops < 1, ntops=  %d  \n',ntops);
    return;
  end
  for itop=1:ntops
    z=topin(itop);
    ind=surround(zt1,z);
    if(length(ind)==1)  %interpolate, 2 surrounding pts.
      rat=(tz1(ind+1)-tz1(ind))/(zt1(ind+1)-zt1(ind));
      t=tz1(ind) + rat*(z-zt1(ind));
    elseif(length(ind)==2) %no interp.needed, right on pt.
      t=tz1(ind(2));
    elseif(length(ind)==0)  %out of range
      t=NaN;
    else  %hopefully, wont ever get this
      t=NaN;
      fprintf(' wierd ind in toptotime =  %d  \n',ind);
    end
    topout(itop)=t;
  end