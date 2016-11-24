function bigfont(haxes,fontsize,fontwt,tickflag)
%
% bigfont(haxes,fontsize,fontwt)
%
% haxes ... handle of an axes object or other parent object 
%		which contains text objects
%   ******** default = gca ******
% fontsize ... desired fontsize expressed as a ratio of output
%		to input size
%   ******** default 2 ********
% fontwt ... 1 = normal
%			 2 = bold
%   *********  default = 2 *********
% tickflag ... 1 ... change tick labels
%			   0 ... dont change tick labels
%   ********* default = 1 *********
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
if(nargin<4)
	tickflag=1;
end
if(nargin<3)
	fontwt=2;
end
if(nargin<2)
	fontsize=2;
end
if(nargin<1)
	haxes=gca;
end
hkids=allchild(haxes);
for k=1:length(hkids)
	if(strcmp(get(hkids(k),'type'),'text'))
		fsize=get(hkids(k),'fontsize');
		set(hkids(k),'fontsize',fontsize*fsize);
		if(fontwt==1)
			set(hkids(k),'fontweight','normal');
		elseif(fontwt==2)
			set(hkids(k),'fontweight','bold');
		end
	end
end
if(tickflag==1)
	fsize=get(haxes,'fontsize');
	set(haxes,'fontsize',fontsize*fsize);
	if(fontwt==1)
			set(haxes,'fontweight','normal');
	elseif(fontwt==2)
			set(haxes,'fontweight','bold');
	end
end
		