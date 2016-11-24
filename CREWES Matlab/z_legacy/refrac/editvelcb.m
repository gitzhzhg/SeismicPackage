function editvelcb(action)
% Determine the parameter for the velocity editing function
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

% median filter window length
if( nargin < 1 )
   action = 'init';
end
if( strcmp(action,'init'))
   q=str2mat('Apply a median filter ?',...
	'To which velocities?',...
	'Median filter window length (odd number):');
   
   a=str2mat('Yes|No','First|Second|Both','5');
   askthingsinit('editvelcb(''answer'')',q,a,[1 0 0],'Edit velocity');
   
elseif( strcmp(action,'answer'))
   a=askthingsfini;
   if( a ~= -1 )
     [strings tmp] = size(a);
     if( strcmp(deblank(a(1,:)), 'Yes'))
	edit=0;
     else
	edit=1;
     end
     
     if( strcmp(deblank(a(2,:)), 'First'))
	v=1;
     elseif( strcmp(deblank(a(2,:)), 'Second'))
	v=2;
     else
	v=3;
     end
     
     win=str2num(a(3,:));
     
     editvel(edit,v,win);
   end
end