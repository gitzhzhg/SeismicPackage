function [shotrange, shotlist, currentshot, axeslist, stddev] = ...
         avgcvpinfo(action, shotrange, shotlist, currentshot, axeslist, stddev)
% shotrange   - 
% shotlist    - 
% currentshot - 
% axeslist    - 
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

% stddev      -
if( nargin < 1 )
   action = 'get';
end
% Get the handle of the 'set range' uicontrol
c = get(gcf, 'children');
[nc tmp] = size(c);
for i=1:nc
   if( strcmp(get(c(i),'type'),'uicontrol') )
      if( strcmp(get(c(i),'string'), 'Set shot range'))
         rangeb = c(i);
      elseif( strcmp(get(c(i),'string'), 'Next shot'))
         nextb = c(i);
      elseif( strcmp(get(c(i),'string'), 'Previous shot'))
         prevb = c(i);
      elseif( strcmp(get(c(i),'string'), 'Done') )
         doneb = c(i);
      end
   end
end
if(isempty(rangeb))
   error('avgcvpinfo: did not find set range button!');
end
 
if(isempty(nextb))
   error('avgcvpinfo: did not find next pair button!');
end
 
if(isempty(prevb))
   error('avgcvpinfo: did not find previous pair button!');
end
if(isempty(doneb))
   error('avgcvpinfo: did not find done button!');
end
if( strcmp(action, 'get') )
   if( nargout < 5 )
      error('avgcvpinfo: not enough output arguements');
   else
      shotrange = get(doneb, 'userdata');
      shotlist = get(rangeb, 'userdata');
      tmp = get(nextb, 'userdata');
      if(~isempty(tmp))
         currentshot = tmp(1);
         stddev = tmp(2);
      else
         currentshot = [];
         stddev = [];
      end
      axeslist = get(prevb, 'userdata');
   end
end
if( strcmp(action, 'set') )
   if( nargin < 6 )
      error('avgcvpinfo: not enough input arguements');
   else
      set(rangeb, 'userdata', shotlist)
      if(isempty(currentshot))
         currentshot = 1;
      end
      if(isempty(stddev))
         stddev = 1;
      end
      set(nextb, 'userdata', [currentshot stddev]);
      set(prevb, 'userdata', axeslist);
      set(doneb, 'userdata', shotrange);
   end
end