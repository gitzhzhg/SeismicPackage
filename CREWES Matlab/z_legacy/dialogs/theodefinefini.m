function [sonicnum,densitynum,densityopt,holealg,theopt,a,m,name]=...
				theodefinefini
%
% [sonicnum,densitynum,densityopt,holealg,theopt,a,m,name]=theodefinefini
%
% Call this to complete the theogram definition dialog. Return values mean:
%   sonicnum = number of the section selected for sonic logs
%   densitynum = number of the section selected for density logs
%   densityopt = density option selection 
%                1: use Gardners exclusively, 
%                2: use density logs but fill holes with Gardners 
%                3: use density logs exclusively
%   holealg = hole filling algorithm selection 1: constant 2: linear 3: mean
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
	params=get(gca,'userdata');
	if( params==-1 ) %test for a cancel
        densitynum=-1;densityopt=-1;
        holealg=-1;theopt=-1;a=-1;m=-1;name=-1;
		sonicnum=-1;
		return;
	end
	sonicnum=params(1);
	densitynum=params(2);
	densityopt=params(3);
	holealg=params(4);
	theopt=params(5);
%		4: layer mean 5: layer trend
%   theopt = 1: primaries only  or 2: primaries plus multiples
%	a= scalar coefficient in gardners relation. Will be -1 if densopt=3
%	m= scalar exponent in gardners relation. Will be -1 if densopt=3
%	name = name chosen for the rc section
	a=params(6);
	m=params(7);
	name=setstr(params(8:length(params)));