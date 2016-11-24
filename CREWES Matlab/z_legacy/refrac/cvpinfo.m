function [shots, currentshot, axeslist] = cvpinfo(action, shots, currentshot, axeslist )
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

if( nargin < 1 )
   action = 'get';
end
% Get the handle of the 'set range' uicontrol
c = get(gcf, 'children');
[nc tmp] = size(c);
for i=1:nc
   if( strcmp(get(c(i),'type'),'uicontrol') )
      if( strcmp(get(c(i),'string'), 'Set range'))
         rangeb = c(i);
      elseif( strcmp(get(c(i),'string'), 'Next pair'))
         nextb = c(i);
      elseif( strcmp(get(c(i),'string'), 'Previous pair'))
         prevb = c(i);
      end
   end
end
if(isempty(rangeb))
   error('cvpinfo: did not find set range button!');
end
if(isempty(nextb))
   error('cvpinfo: did not find next pair button!');
end
if(isempty(prevb))
   error('cvpinfo: did not find previous pair button!');
end
if( strcmp(action, 'get') )
   shots = get(rangeb,'userdata');
   currentshot = get(nextb,'userdata');
   axeslist = get(prevb,'userdata');
end
if( strcmp(action, 'set') )
   if( nargin < 4 )
      error('cvpinfo: not enough input arguements');
   end
   set(rangeb, 'userdata', shots)
   set(nextb, 'userdata', currentshot);
   set(prevb, 'userdata', axeslist );
end