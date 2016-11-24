% figgif - Save a figure into a GIF file
% figgif by itself saves the current figure into a file 
%        called 'Figure No. 1.gif'
%
% figgif('beep') saves the current figure into a file called 'beep.gif'
%
% figgif('beep',h) saves the figure with the handle 'h' into a file
%        called 'beep.gif'
%
% D. Foltinek, August 1997
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
function figgif(filename, h)
if( nargin < 2 )
   h = gcf;
end
% Bring the figure to the foreground
figure(h);
% Build a string containing the name of the X window
name = get(h, 'name');
if( strcmp(name, '') == 1 )
   figname = sprintf('Figure No. %d ', h );
else
   figname = sprintf('Figure No. %d: %s', h, name );
end
if( nargin < 1 )
   filename = sprintf('%s.gif', figname);
else
   filename = sprintf('%s.gif', filename);
end
% Build the Unix command string to grab the named window, save as GIF
cmdstr = sprintf('xwd -name ''%s'' | xwdtopnm | ppmquant -fs 256 | ppmtogif > ''%s'' ', figname, filename );
unix(cmdstr);
fprintf('Saved figure to %s\n',filename);