function windowsize(w,h,hfig)
%
% windowsize(w,h,hfig)
% windowsize(w,h)
% windowsize
%
% WINDOWSIZE sets the size of a figure window to the specified 
% w and h, in pixels. Windowsize by itself gives the current 
% window size in pixels.
% 
% w	= width in pixels
% h	= height in pixels
% hfig	= figure handle
% ================== Default = current figure =================
%
% Darren Foltinek, CREWES Project, 1996
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
if( nargin < 3 )
   hfig = gcf;
end
pos = get( hfig, 'position' );
oldx = pos(1);
oldy = pos(2);
if( nargin < 2 )
   disp( sprintf('Window is currently %d x %d pixels\n', pos(3), pos(4) ) );
   disp('Usage: windowsize( X_size, Y_size)');
   return;
end
set( hfig, 'position', [oldx oldy w h]);