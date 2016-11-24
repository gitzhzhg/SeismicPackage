function points = movecvp(action)
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

if(nargin<1) %set the button down function
		set(gcf,'windowbuttondownfcn','movecvp(''init'')');
		return;
end
% Get the data needed for this function
cvpi = refdata('get','cvpi');
cvpj = refdata('get','cvpj');
shotcoord = refdata('get', 'shotcoord');
fbcoord = refdata('get', 'fbcoord');
fbtime = refdata('get', 'fbtime');
[shots, currentshot, axeslist] = cvpinfo('get');
if(~isempty(currentshot))
   i = shots(currentshot,1);
   j = shots(currentshot,2);
else
   return;
end
validxi = find(~isnan(fbcoord(i,:)));
validxj = find(~isnan(fbcoord(j,:)));
startp1x = min(fbcoord(i,validxi));
startp2x = min(fbcoord(j,validxj));
minx1 = max(startp1x, startp2x);
maxx1 = shotcoord(i);
endp1x = max(fbcoord(i,validxi));
endp2x = max(fbcoord(j,validxj));
maxx2 = min(endp1x, endp2x);
minx2 = shotcoord(j);
pt=get(gca,'currentpoint');
userx = pt(1,1);
% Check to see if the user clicked in the 'i' shot side or the 'j' side.
iniside = 0;
if( userx < maxx1 & userx > minx1 )
   iniside = 1;
end
injside = 0;
if( userx < maxx2 & userx > minx2 )
   injside = 1;
end
if( iniside == 0 & injside == 0 )
   disp('Selected point not in valid range.');
end
xi = cvpi(i,j);
xj = cvpj(i,j);
% Interpolate to find the time of each CVP point
ti = 0;
if( xi ~= NaN )
   ti = interp1(fbcoord(i,:),fbtime(i,:),xi);
end
tj = 0;
if( xj ~= NaN )
   tj = interp1(fbcoord(j,:),fbtime(j,:),xj);
end
  
if(strcmp(action,'init'))
    set(gcf,'windowbuttonmotionfcn','');
    set(gcf,'windowbuttonupfcn','');
    % Selectiontype normal means button 1  - add CVP
    if(strcmp(get(gcf,'selectiontype'),'normal'))
        if( iniside )
           cvp = cvpi(i,j);
        else
           cvp = cvpj(i,j);
        end
        
        if( cvp == NaN )    % Then we can create a point here
           str = sprintf('Creating a CVP point at %f ',userx);
           disp(str);
           if( iniside )
              xi = userx;
	      ti = interp1(fbcoord(i,:),fbtime(i,:),userx);
           else
              xj = userx;
	      tj = interp1(fbcoord(j,:),fbtime(j,:),userx);
           end
           plotCVPlines( 'draw', axeslist, xi, xj, ti, tj ); 
        else
           str = sprintf('Initiating move ',userx);
           disp(str);
        end
        set(gcf,'windowbuttonmotionfcn','movecvp(''move'')');
        set(gcf,'windowbuttonupfcn','movecvp(''fini'')');
	return;
    % Selectiontype alt means button 3  - delete CVP
    elseif(strcmp(get(gcf,'selectiontype'),'alt'))
	hpoint=gco;
	if(strcmp(get(hpoint,'type'),'line')& get(hpoint,'color') == [1 0 0])
		delete(gco);
		return;
	else
		return;
	end
    else
	return;
    end
end
if(strcmp(action,'move'))
   if( iniside )
      xi = userx;
      ti = interp1(fbcoord(i,:),fbtime(i,:),userx);
   else
      xj = userx;
      tj = interp1(fbcoord(j,:),fbtime(j,:),userx);
   end
   plotCVPlines( 'move', axeslist, xi, xj, ti, tj );
   str = sprintf( 'Moving a CVP point at %f ', userx );
   disp(str);
end
if(strcmp(action,'fini'))
   if( iniside )
      xi = userx;
      ti = interp1(fbcoord(i,:),fbtime(i,:),userx);
      cvpi(i,j) = userx;
      refdata('set','cvpi',cvpi);
   elseif( injside )
      xj = userx;
      tj = interp1(fbcoord(j,:),fbtime(j,:),userx);
      cvpj(i,j) = userx;
      refdata('set','cvpj',cvpj);
   else
      disp('Point not in valid range.  Not set.');
   end
   plotCVPlines('draw',axeslist,xi,xj, ti, tj);
   set(gcf,'windowbuttonmotionfcn','');
   set(gcf,'windowbuttonupfcn','');
end
if(strcmp(action,'save'))
	objs = get(gca,'children');
	n = 1;   % Counter for the number of picked points
	[number x] = size(objs);
	for i=1:number    % i is the counter for the number of axes children
		if(strcmp(get(objs(i),'linestyle'),'*'))
			point(n) = objs(i);
			get(point(n),'xdata')   % This just prints X
			n = n+1;
		end
	end
	points = point;
end
		
		