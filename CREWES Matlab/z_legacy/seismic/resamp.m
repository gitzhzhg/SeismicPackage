function [trout,tout]=resamp(trin,t,dtout,timesout,flag,fparms)
% [trout,tout]=resamp(trin,t,dtout,timesout,flag,fparms)
% [trout,tout]=resamp(trin,t,dtout,timesout,flag)
% [trout,tout]=resamp(trin,t,dtout,timesout)
% [trout,tout]=resamp(trin,t,dtout)
%
% RESAMP resamples an input time series using sinc function interpolation.
% If the output sample size is larger than the input, then an antialias filter
% will be applied which can be either zero or minimum phase.
%
%
% trin= input trace to be resampled
% t= time coordinate vector for trin
% dtout= desired output sample size (in seconds). May be any floating point 
%         number. 
% timesout = start and end time desired for the output trace: [tmin tmax]
%	If dtout does not divide evenly into this interval then the first
%	These times need not fall on the input time grid represented by t
%	sample will be at tmin while the last will be just less than tmax
%       ********* default [t(1) t(length(t))] **************
% flag= antialias filter option needed if dtout> t(2)-t(1) 
%       0 -> a zero phase antialias filter is used
%       1 -> a minimum phase antialias filter is used
%       ***************** default = 1 *****************
% fparms= 2 element vector of antialias filter parameters
%       fparms(1)= 3db down point of high frequency rolloff
%       expressed as a fraction of the new nyquist
%       fparms(2)= attenuation desired at the new Nyquist in db
% *************** default = [.6,120] *******************
%
% trout= output resampled trace
% tout= time coordinate vector for trout
%
% by G.F. Margrave, November 1991
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
 if nargin<6, fparms=[.6,120]; end
 if nargin<5, flag=1; end
 if nargin<4 timesout=[t(1) t(length(t))]; end
 % convert everything to a row vector
 [rr,cc]=size(trin);
 trflag=0;
 if( rr > 1)
	 trin=trin';
	 t=t';
	 trflag=1;
	end
% design and apply antialias filter
% Must be careful to apply this only to live samples
 dtin=t(2)-t(1);
% build output time vector 
 tout=timesout(1):dtout:timesout(2);
 % find live samples
ilive=find(~isnan(trin));
%check for completely dead trace
	if(isempty(ilive))
		trout=nan*ones(size(tout));
		return;
	end
		
 if dtout>dtin
	% divide up into zones based on nan's
	  ind=find(diff(ilive)>1);
	  zone_beg=[ilive(1) ilive(ind+1)];
	  zone_end=[ilive(ind) ilive(length(ilive))];
	  nzones=length(zone_beg);
	  trinf=nan*ones(size(trin));
		fnyqnew= .5/dtout;
		fmax=[fparms(1)*fnyqnew,fnyqnew*(1-fparms(1))*.3];
	  for k=1:nzones 
		% get whats in this zone
		n1=round((t(zone_beg(k))-t(1))/dtin)+1;
		n2=round((t(zone_end(k))-t(1))/dtin)+1;
		trinzone=trin(n1:n2);
		%tzone=t(n1:n2);
		tm=mean(trinzone);
		trinzone=filtf(trinzone-tm,t,[0,0],fmax,flag,fparms(2));
		trin(n1:n2)=trinzone+tm;
	  end
 end
% resample with sinc interpolation
 trout=sincinan(trin,t,tout);
 if(rr>1)
		[rr,cc]=size(trout);
		if( rr== 1)
			trout=trout.';
			tout=tout.';
		end
	end