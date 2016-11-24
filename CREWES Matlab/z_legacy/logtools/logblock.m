function logout=logblock(login,zlog,zbdys,flag)
% logout=logblock(login,zlog,zbdys,flag)
%
% Block a well log.
%
% login = column vector containing the log samples
% zlog = column vector containing the sample depths
% zbdys = vector of block boundaries. There must be at least
%	2 boundaries. Log will be blocked between each pair of
%	boundaries. Ends of log will not be blocked unless boundaries
%   are defined at the ends. Boundaries will be sorted into ascending
%	order so they need not be on input
% flag = 1 ... MEAN : each log segment will be replaced by the log
%              mean value.
%        2 ... MEDIAN : each log segment will be replaced by the log
%              median value.
%        3 ... LINEAR TREND : each log segment will be replaced by 
%              a linear trend determined by least squares
% Note that in all cases NAN's are ignored unless the entire log
% segment is NAN in which case NAN is returned.
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
if(length(zbdys)<2)
	error('Must have at least 2 defined boundaries');
end
zbdys=sort(zbdys);
logout=login;
nblks=length(zbdys)-1;
for k=1:nblks
	iblk=between(zbdys(k),zbdys(k+1),zlog,1);
	%iblk indexes the points in the zone. Now find the subset
	%of them that are live
	ilive=find(~isnan(login(iblk)));
	if(isempty(ilive))
		logout(iblk)=nan*ones(size(iblk));
	else
		lseg=login(iblk(ilive));
		zseg=zlog(iblk(ilive));
		%determine mean, median or trend
		if(flag==1)
			p=zeros(1,2);
			p(2)=mean(lseg);
		elseif(flag==2)
			p=zeros(1,2);
			p(2)=median(lseg);
		elseif(flag==3)
			if(length(zseg)>=2)
				p=polyfit(zseg,lseg,1); 
			else
				p=[lseg(1) 0];
			end
		end
		logout(iblk)=polyval(p,zlog(iblk));
	end
end