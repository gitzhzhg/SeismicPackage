function [sectionout,flag]=validatesection(sectionin,psec,tzobj,direction,...
			dy,ynot,traceflags)
% sectionout=validatesection(sectionin,psec,tzobj,direction,dy,ynot,traceflags)
%
% VALIDATESECTION checks a LOGSEC derived section object against its primary
% section and the time depth object to determine if the section is valid. It
% is valid if it was converted from the primary with the tzobj and if the
% tzid saved in the section indicates that tzobj is the object which did the
% conversion. If the section is found to be invalid, then it is converted
% anew from the primary using the tzobj supplied.
%
% sectionin ... input section (container object) to be validated (may be null)
% psec ... primary section (from which sectionout can be computed
%			if sectionin is invalid). Cannot be null
% tzobj ... Time to Depth conversion object. Can be null
% direction ... 1 indicates conversion from depth to time is desired
%               2 indicates conversion from time to depth is desired
% dy = sample rate in either time or depth for the converted section if it is
%      invalid
% ynot = reference y. If conversion is done, the converted section will 
%        be on a grid with includes ynot (perhaps extrapolated).
% traceflags = one entry per trace in the same order that the traces appear
%              in the fleximat. If 0 (false) the trace is outdated and must
%              be reconverted.
%              **** default is all zeros ****
% 
% return values:
% flag == 0 ... input section invalid, no conversion possible
%               (because tzobj was null)
% flag == 1 ... input section was valid and no conversion was done
% flag == 2 ... input section was invalid and was converted
% sectionout == sectionin if no conversion was done for any reason
%               otherwise, it is the converted section
%
% G.F. Margrave May 1994
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
if(nargin<7)
	traceflags=0;
end
convert=0;
if(isempty(sectionin))
	if(~isempty(tzobj))
		convert=1;
	else
		convert=-1;
	end
else
	t1=objget(sectionin,'objmodified');
	t2=objget(psec,'objmodified');
	if( newest(t1,t2)==2)
		if(~isempty(tzobj))
			convert=1;
		else
			convert=-1;
		end
	elseif(~isempty(tzobj))
		tzid1=objget(sectionin,'username');
		tzid2=objget(tzobj,'objmodified');
		if(~strcmp(tzid1,tzid2))
			convert=1;
		end
	end
end
if( convert == -1)
	flag=0;
	sectionout=sectionin;
	return;
elseif( convert == 0)
	flag=1;
	sectionout=sectionin;
	return;
else
	flag=2;
	if( direction==1)
		if( sum(traceflags) == 0)
			sectionout=sectiontotime(psec,tzobj,dy);
		else
			% we only convert certain traces
			% unpack
			fmpsec=objget(psec,'fmat');
			fmsec=objget(sectionin,'fmat');
			x=fmget(fmpsec,'x');
			z=fmget(fmpsec,'y');
			plogs=fmget(fmpsec,'mat');
			for k=1:length(x)
				if(~traceflags(k))
					tlog=logtotime([z,plogs(:,k)],tzobj,dy,x(k));
					fmsec=fmset(fmsec,x(k),tlog(:,1),tlog(:,2));
				end
			end
			sectionout=objset(sectionin,'fmat',fmsec);
		end		
	else
		if( sum(traceflags) == 0)
			sectionout=sectiontodepth(psec,tzobj,dy,ynot);
		else
			% we only convert certain traces
			% unpack
			fmpsec=objget(psec,'fmat');
			fmsec=objget(sectionin,'fmat');
			x=fmget(fmpsec,'x');
			t=fmget(fmpsec,'y');
			plogs=fmget(fmpsec,'mat');
			for k=1:length(x)
				if(~traceflags(k))
					zlog=logtodepth([t,plogs(:,k)],tzobj,dy,x(k));
					disp(['Trace ',num2str(k),' converted']);
					fmsec=fmset(fmsec,x(k),zlog(:,1),zlog(:,2));
				end
			end
			sectionout=objset(sectionin,'fmat',fmsec);
		end		
	end
end