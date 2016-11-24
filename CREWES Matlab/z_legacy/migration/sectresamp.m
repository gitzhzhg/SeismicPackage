function [seis2,t2] = sectresamp(seis,t,dt2,timesout,flag,fparms)
% SECTRESAMP: runs resamp on each trace in a seismic section
%
%  [seis2,t2] = sectresamp(seis,t,dt2,timesout,flag,fparms)
%
% SECTRESAMP resamples (in time) a seismic section.
% It simply loops over columns and calls resamp for each trace.
%
% seis ... input section of size nsamp x ntr. That is one trace per
%	column.
% t ... nsamp long time coordinate vector for seis
% dt2= desired output sample size (in seconds). May be any floating point 
%           number. 
%   timesout = start and end time desired for the output trace: [tmin tmax]
%  	If dt2 does not divide evenly into this interval then the first
%  	These times need not fall on the input time grid represented by t
%  	sample will be at tmin while the last will be just less than tmax
%         ********* default [t(1) t(length(t))] **************
%   flag= antialias filter option needed if dt2> t(2)-t(1) 
%         0 -> a zero phase antialias filter is used
%         1 -> a minimum phase antialias filter is used
%         ***************** default = 1 *****************
%   fparms= 2 element vector of antialias filter parameters
%         fparms(1)= 3db down point of high frequency rolloff
%         expressed as a fraction of the new nyquist
%         fparms(2)= attenuation desired at the new Nyquist in db
%   *************** default = [.6,120] *******************
%
% seis2 ... output section of size nsamp x ntr.
% t2 ... output time coordinate vector
%
% G.F. Margrave, CREWES Project, University of Calgary, 1996
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


[nsamp,ntr]=size(seis);

if(nargin < 4)
	for k=1:ntr
		[tmp,t2] = resamp( seis(:,k),t,dt2);
		if(k==1)
			seis2=zeros(length(t2),ntr);
		end
		seis2(:,k)=tmp;
		if(rem(k,50)==0)
			disp(['finished ' num2str(k) ' traces'])
		end
	end
elseif( nargin < 5)
	for k=1:ntr
		[tmp,t2]  = resamp( seis(:,k),t,dt2,timesout);
		if(k==1)
			seis2=zeros(length(t2),ntr);
		end
		seis2(:,k)=tmp;
		if(rem(k,50)==0)
			disp(['finished ' num2str(k) ' traces'])
		end
	end
elseif( nargin < 6)
	for k=1:ntr
		[tmp,t2]  = resamp( seis(:,k),t,dt2,timesout,flag);
		if(k==1)
			seis2=zeros(length(t2),ntr);
		end
		seis2(:,k)=tmp;
		if(rem(k,50)==0)
			disp(['finished ' num2str(k) ' traces'])
		end
	end
else
	for k=1:ntr
		[tmp,t2]  = resamp( seis(:,k),t,dt2,timesout,flag,fparms);
		if(k==1)
			seis2=zeros(length(t2),ntr);
		end
		seis2(:,k)=tmp;
		if(rem(k,50)==0)
			disp(['finished ' num2str(k) ' traces'])
		end
	end
end