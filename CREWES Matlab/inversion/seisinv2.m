function imp=seisinv2(trin,implog,t,flow,fhigh,delf)
% imp=seisinv2(trin,implog,t,flow,fhigh,delf)
% imp=seisinv2(trin,implog,t,flow,fhigh)
%
% This is no longer supported. Use BLIMP.m in preference.
%
% SEISINV2 estimates acoustic impedence from a seismic trace
% using a well log to provide the low frequency component.
%
% trin ... input seismic trace
% implog ... input impedance log (in time)
% t ... time coordinate vector for trin
% flow ... lowest frequency in trin to keep
% fhigh ... highest signal frequency in trin
% delf ... width of Gaussian rolloff filter to be applied to
%	log at flow and trin at flow+delf
%	****** default min([5 flow/5]) *******
%
% G.F. Margrave, CREWES Project, U of Calgary, 1995-96
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
% check for row vector and transpose if needed
aa=size(trin);
bb=size(t);
cc=size(implog);
if aa(1)==1
	trin=trin';
end
if bb(1)==1
	t=t';
end
if cc(1)==1
	implog=implog';
end
%integrate
if(nargin<6)
	delf=min([5 flow/5]); %gaussian on low end
end
impbl=rcs2impbl(trin,t,flow+delf,fhigh,delf);
%zero pad to impbl
impbl=padpow2(impbl);
%impbl=padpow2(impbl,1); %pad twice for better f sampling
%remove trend of log
p=polyfit(t,implog,1);
implog=implog-polyval(p,t);
implog=pad_trace(implog,impbl);%zero pad
t2=xcoord(t(1),t(2)-t(1),implog);
%merge log and bandlimited impedance
imp=mergetrcs(implog,impbl,t2,flow,delf,fhigh);
imp=imp(1:length(trin));
%restore trand
imp=imp+polyval(p,t);