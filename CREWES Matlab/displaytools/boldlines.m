function boldlines(hdaddy,linewidth,markersize,kol)
% BOLDLINES: changes the thickness of lines and the saize of markers
%
% boldlines(hdaddy,linewidth,markersize,kol)
%
% Its simple, just type "boldlines" at the matlab prompt. The lines are
% made much thicker so that the figures can make good slides.
%
% hdaddy ... handle of a figure or axes object or other parent object 
%		which contains line objects
%   ******** default = gcf ******
% linewidth ... desired line width expressed as a ratio of output
%		to input size
%   ******** default 4 ********
% markersize ... desired line width expressed as a ratio of output
%		to input size
%   ******** default 2 ********
% kol ... color of marker or line
%   ******** default is no color change ********
%
% You can customize the default behavior of boldlines by defining some
%  globals: BOLDLINES_LW and BOLDLINES_MS . Set these to have your desired
%  default values of linewidth and markersize. A good place to define
%  these is in your startup.m file.
%
% Gary Margrave, CREWES
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

global BOLDLINES_MS BOLDLINES_LW

if(nargin<4)
    kol = [];
end
if(nargin<3)
    if(isempty(BOLDLINES_MS))
	    markersize=2;
    else
        markersize=BOLDLINES_MS;
    end
end
if(nargin<2)
    if(isempty(BOLDLINES_LW))
	    linewidth=4;
    else
        linewidth=BOLDLINES_LW;
    end
end
if(nargin<1)
	hdaddy=gcf;
end

if(~isgraphics(hdaddy))
    linewidth=hdaddy;
    hdaddy=gcf;
end

try
    %find all line object children in hdaddy
    hlines = findobj(hdaddy,'type','line'); 
catch ex
    error('crewes:displaytools:boldlines',...
        ['Input hdaddy is not a graphics handle: ',ex.message])
end

lwidth = get(hlines,'linewidth');
msize  = get(hlines,'markersize');

if ~iscell(lwidth) %happens if there is only one line in hdaddy
    lwidth = {lwidth};
    msize = {msize};
end

if isempty(kol)
    for kk = 1:length(hlines)
        set(hlines(kk),'linewidth',linewidth*lwidth{kk});
        set(hlines(kk),'markersize',markersize*msize{kk});
    end %end for
else
    for kk = 1:length(hlines)
        set(hlines(kk),'linewidth',linewidth*lwidth{kk});
        set(hlines(kk),'markersize',markersize*msize{kk});       
        set(hlines(kk),'color',kol);
    end %end for
end %end isempty

end %end function