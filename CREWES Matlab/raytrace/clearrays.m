function clearrays(figno)
% CLEARRAYS: clear (delete) the rays in a figure
%
% clearrays(figno)
%
% CLEARRAY searches the current axes of the desired figure
% for children of type 'line' and deletes all that it finds.
%
% figno ... the figure number to delete from
% ********** default = gcf ***********
%
%
% by G.F. Margrave, CREWES, June 2000
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
if(nargin<1) figno=gcf; end

% global PICKS
% 
% %resolve figure number
% [nfigs,nnn]=size(PICKS);
% doit=0;
% for kkk=1:nfigs
%     thisfig=PICKS{kkk,1};
%     if(thisfig==figno)
%         doit=1;
%         break
%     end
% end
% if(~doit)
%     error('invalid figure number')
% end

hax=get(figno,'currentaxes');
hkids=get(hax,'children');
for k=1:length(hkids)
	if(strcmp(get(hkids(k),'type'),'line'))
% 		x=get(hkids(k),'xdata');
% 		if(length(x)~=2)
% 			delete(hkids(k));
% 		else
% 			np=size(PICKS,1);
% 			y=get(hkids(k),'ydata');
% 			p=ones(np,1)*[x(1) y(1)];
% 			test=sum((PICKS-p)');
% 			ind=find(test==0);
% 			if(isempty(ind))
% 				delete(hkids(k));
% 			else
% 				p=ones(np,1)*[x(2) y(2)];
% 				test=sum((PICKS-p)');
% 				ind=find(test==0);
% 				if(isempty(ind))
% 					delete(hkids(k));
% 				end
% 			end
% 		end
        delete(hkids(k))
	end
end

% PICKS{kkk,2}=[];
% PICKS{kkk,3}=[];