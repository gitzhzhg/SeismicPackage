function [block,ibeg,iend]=lashgetblk(lash,bname)
% [block,ibeg,iend]=lashgetblk(lash,bname)
% 
% LASHGETBLK interrogates a LAS header to find a logical data
% 'block'. A block is defined as beginning with a line: 
% ~bname
% where bname is a string, and ending at the beginning of the next 
% block or the EOF. Note that the block is considered matched if
% the line in the LAS header matches ~bname up to the length of bname.
% Thus a line in an LAS header like 
% ~logedit parameters
% will match with bname of ~log, or ~logedit, etc... The returned block
% is the first matching one. The match is not case sensitive.
% lash ... LAS header in a string matrix
% bname ... string containing the block name
% block ... string matrix containing the block including the ~bname line.
% ibeg ... the line number in lash of the beginning of the block
% iend ... the line number in lash of the end of the block
%
% G.F. Margrave, Department of Geology and Geophysics,
%	University of Calgary, 1996
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
[nlines,m]=size(lash);
k=1;
ibeg=0;
iend=0;
target=['~' lower(bname)];
ln=length(target);
while k<=nlines
	% check for ~
	if(lash(k,1)=='~')
		str=lash(k,:);
		
		if(strcmp(lower(str(1:ln)),target) )
			ibeg=k;
		else
			if(ibeg)
				iend=k-1;
				break;
			end
		end
	end
	
	k=k+1;
end
if(iend & ibeg)
	block=lash(ibeg:iend,:);
else
	block=[];
end
	