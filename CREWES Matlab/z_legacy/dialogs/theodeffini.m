function [sonicnum,densitynum,waveletnum,densityopt,holealg,a,m,theotype,name]=...
				theodeffini
%
% [sonicnum,densitynum,waveletnum,densityopt,holealg,a,m,theotype,name]=theodeffini
%
% Call this to complete the theogram definition dialog. Return values mean:
%	sonicnum = number of the sonic section selected for theogram
%	densitynum = number of the density section selected for theogram
%	waveletnum = number of the wavelet selected for theogram
%	densityopt = density option selection 1: use Gardners exclusively, 2: use
%		density logs but fill holes with Gardners 3: use density logs exclusively
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

%	holealg = hole filling algorithm selection 1: constant 2: linear 3: mean
	params=get(gca,'userdata');
	if( params==-1 ) %test for a cancel
		sonicnum=-1;
		return;
	end
	sonicnum=params(1);
	densitynum=params(2);
	waveletnum=params(3);
	densityopt=params(4);
	holealg=params(5);
%		4: layer mean 5: layer trend
%   domain = 1: compute rcs in time  or 2: compute rcs in depth
%	a= scalar coefficient in gardners relation. Will be -1 if densopt=3
%	m= scalar exponent in gardners relation. Will be -1 if densopt=3
%	name = name chosen for the rc section
	a=params(6);
	m=params(7);
	theotype=params(8);
	name=setstr(params(8:length(params)));