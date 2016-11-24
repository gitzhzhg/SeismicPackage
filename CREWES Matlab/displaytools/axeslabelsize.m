function axeslabelsize(factor,hdaddy)
% AXESLABELSIZE .... chane the size of axes labels
%
% axeslabelsize(factor,hdaddy)
% factor ... scale factor for font (e.g 2 or .5 to double or halve the font)
% hdaddy ... handle of parent object
% ********** default gcf *****************
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


if(nargin<2)
    hdaddy=gcf;
end

if(strcmp(get(hdaddy,'type'),'figure'))
    haxes=findobj(hdaddy,'type','axes');
elseif(strcmp(get(hdaddy,'type'),'axes'))
    haxes=hdaddy;
else
    error('parent object of incorrect type')
end

for k=1:length(haxes)
    hlx=get(haxes(k),'xlabel');
    set(hlx,'fontsize',factor*get(hlx,'fontsize'));
    
    hly=get(haxes(k),'ylabel');
    set(hly,'fontsize',factor*get(hly,'fontsize'));
    
    hlz=get(haxes(k),'zlabel');
    set(hlz,'fontsize',factor*get(hlz,'fontsize'));
end