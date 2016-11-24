function indicies = between(x1,x2,testpts,flag)
% BETWEEN: logical test, finds samples in vector between given bounds
%
% indicies = between(x1,x2,testpts,flag)
% indicies = between(x1,x2,testpts)
%
% returns the indicies of those points in the array testpts which lie between
% the points x1 and x2. If no testpts are found between x1 and x2 then a
% single scalar 0 (false) is returned. Flag determines the exact nature of
% the inclusion of the endpoints:
%    if flag == 0, then not endpoints are included
%    if flag == 1, then x1 is included. i.e. if a test point is precisely
%                  equal to x1,	it will be considered "between"
%    if flag == 2, then both x1 and x2 are included
%  ******** flag defaults to 0 ****
%
% Function works regardless of whether x1 < x2 or x2 < x1.
%
% by G.F. Margrave, May 1991
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

if nargin < 4, flag =0; end

if(length(x1)~=1 || length(x2)~=1)
    error('x1 and x2 must be scalars')
end

if flag == 0
    if( x1 < x2)
        indicies = find( (testpts > x1)&(testpts < x2) );
        if(isempty(indicies))indicies=0;end
        return;
    else
        indicies = find( (testpts > x2)&(testpts < x1) );
        if(isempty(indicies))indicies=0;end
        return;
    end
end
if flag == 1
    if( x1 < x2)
        indicies = find( (testpts >= x1)&(testpts < x2) );
        if(isempty(indicies))indicies=0;end
        return;
    else
        indicies = find( (testpts > x2)&(testpts <= x1) );
        if(isempty(indicies))indicies=0;end
        return;
    end
end
if flag == 2
    if( x1 < x2)
        indicies = find( (testpts >= x1)&(testpts <= x2) );
        if(isempty(indicies))indicies=0;end
        return;
    else
        indicies = find( (testpts >= x2)&(testpts <= x1) );
        if(isempty(indicies))indicies=0;end
        return;
    end
end