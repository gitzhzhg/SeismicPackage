function drawazi(action)
% Modified after drawline
% DRAWazi works just like SELBOX except that it draws a azi instead of a box
% There are also DRAWLINEINIT and DRAWLINEFINI just like SELBOX. See help for
% SELBOX for a description
%
% by G.F. Margrave, November 1993
%  T. N. BISHOP,  DECEMBER 1993,  CPTC CANADA
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
if(strcmp(action,'init') )
        p1=get(gca,'currentpoint');
        dat=get(gca,'userdata');
        if(length(dat)>=4) delete(dat(4)); delete(dat(5)); end
        set(gca,'userdata',p1(1,1:2));
        set(gcf,'windowbuttonmotionfcn','drawazi(''motion'')');
        return;
end
if(strcmp(action,'fini'))
        set(gcf,'windowbuttonmotionfcn','');
        return;
end
if( strcmp(action,'motion') )
% get the starting point from axes userdata
	h=get(gca,'userdata');
	
	p1=h(1:2);
	
	% delete any pre-existing azi
	if(length(h)>=4) delete(h(4)); delete(h(5)); end
	
% get the current point from the axes
	p2=get(gca,'currentpoint');
	p2=p2(1,1:2);
	
% draw the azi
	 h=line([p1(1),p2(1)],[p1(2),p2(2)],'erasemode','xor','color',[.5 .5 .5]);
         azi = atan2( (p2(2)-p1(2)),(p2(1)-p1(1)) );
         azi = 180.*azi/pi;
%  convert from math angle to compass angle, where n=0, e=90...
         azitxt = -azi + 90;
         if (azitxt < 0)
           azitxt = azitxt + 360;
         end
         s = sprintf('az = %6.1f',azitxt);
         h2 = text(p1(1),p1(2),s,'erasemode','xor','color',[.5 .5 .5]); 
         
	  
% update the info in userdata
 	set(gca,'userdata',[p1 azi h h2]);
return;
end