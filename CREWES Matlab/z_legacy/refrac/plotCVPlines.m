function plotCVPlines( action, axeslist, xi, xj, ti, tj )
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

[tmp numaxes] = size(axeslist);
k = 1;
if(isempty(xi))
   xi = NaN;
end
if(isempty(xj))
   xj = NaN;
end
linehandles = get(axeslist(1),'userdata');
[nlines tmp] = size(linehandles);
if( strcmp(action,'move') )
   thecolor = [0 0 0];
   if( ~isnan(xi) )
      thecolor = [0 0 1];    % Blue is the color of the 'i' lines
      newx = xi;
   elseif( ~isnan(xj) )
      thecolor = [0 1 0];    % Green is the color of the 'j' lines
      newx = xj;
   end
   
   str = sprintf('drawing lines at %f',newx);
   disp(str);
   for i=1:nlines
      l = linehandles(i);
      if( get(l,'color') == thecolor )
         set(l,'xdata',newx);
      end
   end
   drawnow;
end
if( strcmp(action,'draw') )
   drawmode = 'normal';
   if( nlines > 0 )
      delete(linehandles);
   end
   k = 1;
   for i=1:numaxes
      axes(axeslist(i));
      yr = get(gca, 'ylim');
      if( ~isnan(xi) )
         linehandles(k) = line([xi xi], yr, 'color', 'g' );
         set(linehandles(k),'erasemode','xor');
         k = k + 1;
      end
      if( ~isnan(xj) )
         linehandles(k) = line([xj xj], yr, 'color', 'b' );
         set(linehandles(k),'erasemode','xor');
         k = k + 1;
      end
   end
   axes(axeslist(1));
   if( ~isnan(xi) )
      linehandles(k)= line(xi, ti, 'linestyle', 'o', 'color', 'c');
      k = k+1;
   end
   if( ~isnan(xj) )
      linehandles(k) = line(xj, tj, 'linestyle', '+', 'color', 'c');
      k = k+1;
   end
   
   set(axeslist(1), 'userdata', linehandles);
end