function legendfontsize(factor,hdaddy)
%
% legendfontsize(factor,hdaddy)
%
% factor ... scale factor for font size. factor=2 doubles the fonsize while
%           factor=0.5 halves it.
% hdaddy ... handle of parent of the legend. Defaults to gcf
%
% G.F. Margrave, CREWES, 2015
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

if(nargin<2)
    hdaddy=gcf;
end


if(strcmp(get(hdaddy,'type'),'figure'))
    hlegend   = findall(hdaddy,'tag','legend','-or','type','legend');
elseif(strcmp(get(hdaddy,'type'),'axes'))
    hlegend   = findall(hdaddy,'tag','legend','-or','type','legend');
elseif(strcmp(get(hdaddy,'type'),'legend'))
    hlegend=hdaddy;
else
    error('parent object of incorrect type')
end

for k=1:length(hlegend)
   set(hlegend(k),'fontsize',factor*get(hlegend(k),'fontsize'));
end