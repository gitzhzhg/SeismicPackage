function [xout,ikeep]=nanclear(xin)
%
% [xout,ikeep]=nanclear(xin)
%
% NANCLEAR is used by LOGSEC to clear up un-needed NAN's in a multisegmented
% horizon. These are defined as NAN's aat the beginning or end of the 
% horizon and any occurrance of multiple NAN's in a row. Multiple NAN's 
% are reduced to a single NAN. ikeep is a vector of indicies such that
% xout=xin(ikeep)
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
id=isnan(xin);
idead=find(id);
ikeep=1:length(xin);
% simple return for no-nans
if( length(idead) == 0 )
	xout=xin;
	return;
end
% check for non-isolated nans
it=id+[0 id(1:length(id)-1)];
ikeep=find(it<2);
% toss any nans at the beginning or end
if(isnan(xin(ikeep(1))))
	ikeep=ikeep(2:length(ikeep));
end
if(isnan(xin(ikeep(length(ikeep)))))
	ikeep=ikeep(1:length(ikeep)-1);
end
xout=xin(ikeep);