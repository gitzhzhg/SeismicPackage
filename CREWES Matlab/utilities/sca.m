function sca
% SCA: set current axis utility
%
% sca
%
% SCA is usefull with figures with multiple axes in setting the current axes as
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

% MATLAB seems to occaisionally screw this up

% search the figure for axes children
kids=get(gcf,'children');
axes=zeros(1,100); %allow for 100 in a figure
nax=0;
for k=1:length(kids)
   if(strcmp(get(kids(k),'type'),'axes'))
      nax=nax+1;
      axes(nax)=kids(k);
   end
end
p1=get(gca,'currentpoint');
% trap for matlab bug which does not set the current axes properly
% if(nax>1)
%    for k=1:nax
%       ylim=get(axes(k),'ylim');
%       xlim=get(axes(k),'xlim');
%       p1=get(axes(k),'currentpoint');
%       if( between(ylim(1),ylim(2),p1(1,2),2) && between(xlim(1),xlim(2),p1(1,1),2) )
% 	 set(gcf,'currentaxes',axes(k));
% 	 break;
%       end
%    end
% end
return;