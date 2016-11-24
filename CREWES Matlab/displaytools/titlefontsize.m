function titlefontsize(fs,fw,hdaddy)
% Adjust font size in title
% 
% titlefontsize(fs,fw,hdaddy)
%
% fs ... font size multiplier. fs=2 will double the font size
% fw ... font weight, 1 for normal, 2 for bold
% ******** default is no change *******
% hdaddy ... handle of parent
% ******** default is gcf ********
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

% ******** default is gcf *********

if(nargin<3)
    hdaddy=gcf;
end
if(nargin<2)
    fw=0;
end

if(strcmp(get(hdaddy,'type'),'figure'))
    haxes=findobj(hdaddy,'type','axes');
elseif(strcmp(get(hdaddy,'type'),'axes'))
    haxes=hdaddy;
else
    error('invalid parent object');
end

for k=1:length(haxes)
    set(haxes(k),'TitleFontSizeMultiplier',fs);
    if(fw==1)
        set(haxes(k),'TitleFontWeight','normal');
    elseif(fw==2)
        set(haxes(k),'TitleFontWeight','normal');
    end
end