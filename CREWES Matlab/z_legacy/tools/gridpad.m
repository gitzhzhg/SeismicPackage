function grout = gridpad( grin, nrows, ncols, padval)
% grout = gridpad( grin, nrows, ncols, padval)
% grout = gridpad( grin, nrows, ncols)
%  
% pad (or truncate) a grid to a new size
%
% grin = input grid
% nrow = number of rows on output
% ncols = number of columns on output
% padval = value to pad with
%		*********** default 0.0 ***********
% grout = output grid
%
% by G.F. Margrave October 1993
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
if( nargin < 4) padval = 0.0; end
[rin colin]=size(grin);
% pad the rows
	if( nrows <= rin ) % truncate
		grout = grin(1:nrows,:);
	else %pad
		grout = [grin; padval*ones(nrows-rin,colin)];
	end
	
% pad the columns
	if( ncols <= colin ) %truncate
		grout = grout(:,1:ncols);
	else % pad
		grout = [grout padval*ones(nrows,ncols-colin)];
	end