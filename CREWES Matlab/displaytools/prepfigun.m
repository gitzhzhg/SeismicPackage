%PREPFIG ... usuful utility to make presentation quality figures
% PREPFIG calls the following utilities:
% bigfig,bigfont,boldlines(gcf,4,2),whitefig,hideui 
% Each utility is called with its default values except for boldlines. See
% help on each one for more info. The call to boldlines enlarges linewidths
% four times and marker sizes two times. On windows it also copies the
% modified figure to the clipboard. On other systems it writes a tiff file
% into your working directory.
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

%
bigfig; %enlarge the figure to get more pixels
bigfont(gcf,1.6,1); %enlarge the fonts in the figure
boldlines(gcf,4,2); %make lines and symbols "fatter"
whitefig; %make the background white
%hideui; %hide any user interface controls


if(ispc)
    fh = gcf;
    fh.Renderer = 'opengl';
    hgexport(fh, '-clipboard'); %copy the figure to the clipboard (windows only)
else
    print -dtiff
end