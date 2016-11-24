function prettyid=prettywid(uglyid)
% prettyid=prettywid(uglyid)
%
% PRETTYWID converts an ugly well id into a pretty one
% The prettyid will be: lsd-sect-twp-rng
% Code adapted from 
% void short_well_name_from_uwi(char* short_well_name,const char* uwi,
% 	const int form)
% found antares in /chap/extracts/prod/src/extinterp/extinterp.pc
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
if(length(uglyid)<12)
		prettyid=uglyid;
		if(length(prettyid)==0) prettyid=''; end
		return;
	end
prettyid= blanks(14);
	prettyid(1:2)=uglyid(4:5); %lsd (what a trip)
	prettyid(3)='-';
	prettyid(4:5)=uglyid(6:7); %section
	prettyid(6)='-';
	prettyid(7:9)=uglyid(8:10); %township
	prettyid(10)='-';
	prettyid(11:12)=uglyid(11:12); %range
	