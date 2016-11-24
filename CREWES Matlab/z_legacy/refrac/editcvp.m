% Edit CVP matrix
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

% Functions called - movecvp cvpinfo plotCVPlines rangecb nextcb shotrange
function editcvp(action)
if( nargin < 1)
   action='init';
end
% First, delete any axes in the main figure
c = get(gcf, 'children');
[nc tmp] = size(c);
for i=1:nc
   type = get(c(i),'type');
   if( strcmp(type,'axes') )
      delete(c(i));
   end
end
% We are saving information in the 'userdata' spaces of the edit CVP
% buttons:
% Set range - shot pair matrix => sets of valid shot pairs, for
%             editing.  The 'next' and 'previous' buttons step through
%             this list.
%
% Next pair - Index of current shot pair
% The init function clears the figure window, and displays buttons
% to control the edit functions.
if( strcmp(action,'init') )
   % We also need to delete any buttons that are on the screen
   c = get(gcf, 'children');
   [nc tmp] = size(c);
   for i=1:nc
      type = get(c(i),'type');
      if( strcmp(type,'uicontrol') )
         delete(c(i));
      end
   end
   rangeb = uicontrol('style', 'pushbutton', 'units', 'normalized' );
   set(rangeb, 'position', [.01 .01 .15 .05] );
   set(rangeb, 'string', 'Set range');
   set(rangeb, 'callback', 'rangecb');
   nextb = uicontrol('style', 'pushbutton', 'units', 'normalized' );
   set(nextb, 'position', [.18 .01 .15 .05] );
   set(nextb, 'string', 'Next pair');
   set(nextb, 'callback', 'nextcb(''next'')');
   set(nextb, 'visible', 'off');
   previousb = uicontrol('style', 'pushbutton', 'units', 'normalized' );
   set(previousb, 'position', [.35 .01 .17 .05] );
   set(previousb, 'string', 'Previous pair');
   set(previousb, 'callback', 'nextcb(''prev'')');
   set(previousb, 'visible', 'off');
   doneb = uicontrol('style', 'pushbutton', 'units', 'normalized' );
   set(doneb, 'position', [.54 .01 .15 .05] );
   set(doneb, 'string', 'Done');
   set(doneb, 'callback', 'editcvp(''done'')');
end
% 
if( strcmp(action,'run'))
   [shots, k, axeslist] = cvpinfo('get');
   % Get the current shot pair index
   [nshotpairs tmp] = size(shots);
   if( k > nshotpairs )
      k = k-1;
   end
   i = shots(k,1);
   j = shots(k,2);
   
   % get variables
   fbcoord = refdata('get', 'fbcoord');
   fbtime = refdata('get', 'fbtime');
   shotcoord = refdata('get', 'shotcoord');
   window = refdata('get', 'window');
   nd = refdata('get', 'nd');
   cvpi = refdata('get', 'cvpi');
   cvpj = refdata('get', 'cvpj');
   windmn = refdata('get', 'windmn');
   offsetrange1 = refdata('get', 'offsetrange1');
   offsetrange2 = refdata('get', 'offsetrange2');
   
   % Assign axis [left bottom width height]
   fbaxis = axes('position', [.1 .59 .80 .35]);     % First break
   tdaxis = axes('position', [.1 .46 .80 .10]);     % Time difference
   mdaxis = axes('position', [.1 .32 .80 .10]);     % Median filtered TD
   d1axis = axes('position', [.1 .20 .80 .10]);     % 1st derivative of TD
   d2axis = axes('position', [.1 .08 .80 .10]);     % 2nd derivative of TD
   set(gcf,'units','pixels','position',[0 0 864 864]);   
   % First call the traveltime substraction function to calculate
   % the traveltime difference on each side of the shot pair locations  
   [tdiff1, tdiff2, s1j, s2j, e1j, e2j, i, j] = tsub(i, j, shotcoord, ...
       fbcoord, fbtime);
   [mddiff1, deltf1, delts1, cvp1] = pickTD(tdiff1, ...
       fbcoord(j,s1j:e1j), window, windmn, nd, offsetrange1, offsetrange2, ...
       shotcoord(i));
   [mddiff2, deltf2, delts2, cvp2] = pickTD(tdiff2, ...
       fbcoord(j,s2j:e2j), window, windmn, nd, offsetrange1, offsetrange2, ...
       shotcoord(j));
   % Plot the refracted arrivals for the two shots
   axes(fbaxis);
   plot(fbcoord(i,:), fbtime(i,:));  hold on;
   plot(fbcoord(j,:), fbtime(j,:));
   string = sprintf('First breaks for shot pair: %d %d',i,j);
   text('units', 'normalized', 'position', [0.5 0.88], 'string', string);
   ylabel('Time (ms)');
   hold off;
   
   yr = get(fbaxis, 'ylim');
   cvpxi = cvpi(i,j);
   cvpxj = cvpj(i,j);
   
   % Set flags for valid left and right side 
   haveleft = (~isempty(tdiff1));
   haveright = (~isempty(tdiff2));
   
   % Get the xlimits of the fbtime plot
   xlimits = get(fbaxis, 'xlim');
   
   % Plot time difference
   axes(tdaxis);
   if( haveleft )
      plot(fbcoord(j,s1j:e1j), tdiff1);
      hold on;
   end
   
   if( haveright )
      plot(fbcoord(j,s2j:e2j), tdiff2);
   end
   set(tdaxis, 'xlim', xlimits);
   hold off;
   string = 'Time difference (TD)';
   text('units', 'normalized', 'position', [0.6 0.88], 'string', string);
   ylabel('Time (ms)');
   
   % Plot the median filter output
   axes(mdaxis);
   if( haveleft )
      plot(fbcoord(j,s1j:e1j), mddiff1);
      hold on;
   end
   if( haveright )
      plot(fbcoord(j,s2j:e2j), mddiff2);
   end
   set(mdaxis, 'xlim', xlimits);
   set(mdaxis,'xtick',[]);
   hold off;
   string = 'Median filtered TD';
   text('units', 'normalized', 'position', [0.6 0.88], 'string', string);
   ylabel('Time (ms)');
      
   % Plot the first derivative
   axes(d1axis);
   if( haveleft & ~isempty(deltf1))
      plot(deltf1(2,:), deltf1(1,:));
      hold on;
   end
   if( haveright & ~isempty(deltf2))
      plot(deltf2(2,:), deltf2(1,:)); 
   end
   set(d1axis, 'xlim', xlimits);
   set(d1axis,'xtick',[]);
   hold off;
   string = '1st derivative of filtered TD';
   text('units', 'normalized', 'position', [0.6 0.88], 'string', string);
      
   % Plot the 2nd derivative
   axes(d2axis);
   if( haveleft & ~isempty(delts1))
      plot(delts1(2,:), delts1(1,:));
      hold on;
   end
   if( haveright & ~isempty(delts2))
      plot(delts2(2,:), delts2(1,:));
   end
   
   set(d2axis, 'xlim', xlimits);
   set(d2axis,'xtick',[]);
   hold off;
   string = '2nd derivative of filtered TD';
   text('units', 'normalized', 'position', [0.6 0.88], 'string', string);
   % Plot the lines showing the CVPs
   axeslist = [fbaxis tdaxis mdaxis d1axis d2axis];
   xi = NaN;   xj = NaN;
   ti = 0;     tj = 0;
   if( haveleft & find(isnan(cvpi(i,j)))~=1 )
      xi = cvpi(i,j);
      ti = interp1(fbcoord(i,:),fbtime(i,:),xi);
   end
   if( haveright & find(isnan(cvpj(i,j)))~=1 )
      xj = cvpj(i,j);
      tj = interp1(fbcoord(j,:),fbtime(j,:),xj);
   end
   plotCVPlines( 'draw', axeslist, xi, xj, ti, tj );
   cvpinfo('set',shots, k, axeslist);   
   movecvp;
end
if strcmp(action, 'done')
   % Delete any buttons that are on the screen
   c = get(gcf, 'children');
   [nc tmp] = size(c);
   for i=1:nc
      type = get(c(i),'type');
      if( strcmp(type,'uicontrol') )
         delete(c(i));
      end
   end
   % Reset the figure mouse functions
   set(gcf,'windowbuttondownfcn','');
   set(gcf,'windowbuttonmotionfcn','');
   set(gcf,'windowbuttonupfcn','');
end