% Function to allow manual editing of CVP average locations
%
% Usage   movecvpavg(action)
% where action is one of:
%
%  init  - Initialize function.  Turns on average editing.
%  fini  - Saves current CVP average locations in database (refdata)
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

% clear  - Clears button callback function assignments
function points = movecvpavg(action)
if(nargin<1 | strcmp(action, 'init'))    % set the button down function
  set(gcf,'windowbuttondownfcn','movecvpavg(''init'')');
end
% Get the data needed for this function
[shotrange shotINClist shot fullaxeslist stddev] = avgcvpinfo('get');
axeslist = fullaxeslist(1:5);
cvpavg = refdata('get', 'cvpavg');
shotcoord = refdata('get', 'shotcoord');
fbcoord = refdata('get', 'fbcoord');
fbtime = refdata('get', 'fbtime');
validx = find(~isnan(fbcoord(shot,:)));
xrange = fbcoord(shot,validx);
maxx = max(xrange);
minx = min(xrange);
shotx = shotcoord(shot);
% Get the location of the current mouse-click
pt=get(gca,'currentpoint');
userx = pt(1,1);
% Check to see if the user clicked in the 'i' shot side or the 'j' side.
iniside = 0;
if( userx > minx & userx < shotx )
  iniside = 1;
end
injside = 0;
if( userx < maxx & userx > shotx )
  injside = 1;
end
if( iniside == 0 & injside == 0 )
  disp('Selected point not in valid range.');
  return;
end
xi = cvpavg(shot,1);
xj = cvpavg(shot,2);
% Interpolate to find the time of each CVP point
ti = 0;
if( ~isnan(xi) )
  ti = interp1(fbcoord(shot,:),fbtime(shot,:),xi);
end
tj = 0;
if( ~isnan(xj) )
  tj = interp1(fbcoord(shot,:),fbtime(shot,:),xj);
end
  
if(strcmp(action,'init'))
  disp('Movecvpavg: initializing...');
  set(gcf,'windowbuttonmotionfcn','');
  set(gcf,'windowbuttonupfcn','');
  set(gcf,'windowbuttondownfcn','movecvpavg(''pick'')' );
end
if( strcmp(action,'pick') )
  % Selectiontype normal means button 1 - move CVP average to this point
  if(strcmp(get(gcf,'selectiontype'),'normal'))
    if( iniside )
      xi = userx;
      ti = interp1(fbcoord(shot,:),fbtime(shot,:),userx);
    else
      xj = userx;
      tj = interp1(fbcoord(shot,:),fbtime(shot,:),userx);
    end
    plotCVPlines( 'draw', axeslist, xi, xj, ti, tj ); 
  else
    str = sprintf('Placing CVP average at:',userx);
    disp(str);
  end
  
  set(gcf,'windowbuttonupfcn','movecvpavg(''fini'')');
  return;
end
if(strcmp(action,'fini'))
  str = sprintf('Movecvpavg: finishing shot %d ...', shot);  
  disp(str);
  if( iniside )
    xi = userx;
    ti = interp1( fbcoord(shot,:), fbtime(shot,:), userx );
    cvpavg(shot,1) = userx;
    refdata('set', 'cvpavg', cvpavg);
  elseif( injside )
    xj = userx;
    cvpavg(shot,2) = userx;
    refdata('set', 'cvpavg', cvpavg);
    tj = interp1(fbcoord(shot,:),fbtime(shot,:),userx);
  else
    disp('Point not in valid range.  Not set.');
  end
  plotCVPlines('draw',axeslist,xi,xj, ti, tj);
  
  set(gcf,'windowbuttonmotionfcn','');
  set(gcf,'windowbuttonupfcn','');
end
 
 
% if(strcmp(action,'save'))
%    objs = get(gca,'children');
%    n = 1;   % Counter for the number of picked points
%    [number x] = size(objs);
%    for i=1:number    % i is the counter for the number of axes children
%       if(strcmp(get(objs(i),'linestyle'),'*'))
%          point(n) = objs(i);
%          get(point(n),'xdata')   % This just prints X
%          n = n+1;
%       end
%    end
%    points = point;
% end
		
		
if( strcmp(action, 'clear'))
   set(gcf, 'windowbuttonmotionfcn', '');
   set(gcf, 'windowbuttonupfcn', '');
   set(gcf, 'windowbuttondownfcn', '');
end