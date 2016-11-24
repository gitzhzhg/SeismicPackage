% Edit average CVP values
% Functions called - moveavgcvp avgcvpinfo plotavgCVPlines 
%                    avgrangecb avgnextcb avgCVPshotrange
%
% Data storage is handled by the avgcvpinfo function, which uses
% the userdata of various buttons.
%
% Actions:
%   init  - creates buttons and axes
%    run  - does everything, calculates and plots results
%  label  - prints out current parameter values
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

%  clear  - removes callback function assignments
function editcvpavg(action)
if( nargin < 1)
  action='init';
end
% The init function clears the figure window, and displays buttons
% to control the edit functions.
if( strcmp(action, 'init') )
  % First, delete any axes and uicontrols in the main figure
  c = get(gcf, 'children');
  [nc tmp] = size(c);
  for i=1:nc
    type = get(c(i),'type');
    if( strcmp(type,'axes') | strcmp(type,'uicontrol') )
      delete(c(i));
    end
  end
  
  % Assign buttons [left bottom width height]
  rangeb = uicontrol('style', 'pushbutton', 'units', 'normalized' );
  set(rangeb, 'position', [.01 .01 .18 .04] );
  set(rangeb, 'string', 'Set shot range');
  set(rangeb, 'callback', 'avgrangecb');
  
  nextb = uicontrol('style', 'pushbutton', 'units', 'normalized' );
  set(nextb, 'position', [.20 .01 .15 .04] );
  set(nextb, 'string', 'Next shot');
  set(nextb, 'callback', 'avgnextcb(''next'')');
  set(nextb, 'visible', 'off');
  
  previousb = uicontrol('style', 'pushbutton', 'units', 'normalized' );
  set(previousb, 'position', [.36 .01 .17 .04] );
  set(previousb, 'string', 'Previous shot');
  set(previousb, 'callback', 'avgnextcb(''prev'')');
  set(previousb, 'visible', 'off');
  
  compb = uicontrol('style', 'pushbutton', 'units', 'normalized' );
  set(compb, 'position', [.54 .01 .15 .04] );
  set(compb, 'string', 'Recompute');
  set(compb, 'callback', 'editcvpavg(''run'')');
  set(compb, 'visible', 'off');
  
  doneb = uicontrol('style', 'pushbutton', 'units', 'normalized' );
  set(doneb, 'position', [.80 .01 .12 .04] );
  set(doneb, 'string', 'Done');
  set(doneb, 'callback', 'editcvpavg(''done'')');
  
  applyb = uicontrol('style', 'pushbutton', 'units', 'normalized' );
  set(applyb, 'position', [.62 .10 .26 .04]);
  set(applyb, 'string', 'Averaging parameters');
  set(applyb, 'callback', 'cvpavgSRcb');
  
  nshots = refdata('get', 'nshots');
  shotrange = [1 nshots];
  shotINClist = 1:nshots;
  shot = 1;
  stddev = 1;
  % Assign axis [left bottom width height]
  fbaxis  = axes('position', [.1 .64 .45 .30]);     % First break
  tdaxis  = axes('position', [.1 .48 .45 .10]);     % Time difference
  mdaxis  = axes('position', [.1 .34 .45 .10]);     % Median filtered TD
  d1axis  = axes('position', [.1 .21 .45 .10]);     % 1st derivative of TD
  d2axis  = axes('position', [.1 .08 .45 .10]);     % 2nd derivative of TD
  
  cvpaxis = axes('position', [.62 .64 .33 .30]);    % CVP value 
  diffaxis =axes('position', [.62 .34 .33 .24]);    % CVP - average 
  
  axeslist = [fbaxis tdaxis mdaxis d1axis d2axis cvpaxis diffaxis];
  avgcvpinfo('set', shotrange, shotINClist, shot, axeslist, stddev );
  
  set(gcf,'units','pixels','position',[0 0 864 864]);
end
if( strcmp(action,'run'))
  % get variables
  fbcoord = refdata('get', 'fbcoord');
  fbtime = refdata('get', 'fbtime');
  shotcoord = refdata('get', 'shotcoord');
  window = refdata('get', 'window');
  nd = refdata('get', 'nd');
  cvpi = refdata('get', 'cvpi');
  cvpj = refdata('get', 'cvpj');
  nshots = refdata('get', 'nshots');
  tolerance = refdata('get', 'flatslope');
  shotgap = refdata('get', 'shotgap' );
  shotlength = refdata('get', 'shotlength' );
  
  % Get info from buttons
  [shotrange shotINClist shot axeslist stddev] = avgcvpinfo('get');
  fbaxis = axeslist(1);
  tdaxis = axeslist(2);
  mdaxis = axeslist(3);
  d1axis = axeslist(4);
  d2axis = axeslist(5);
  cvpaxis = axeslist(6);
  diffaxis = axeslist(7);
  
  % Plot the refracted arrivals for the  shot   -------------------------
  axes(fbaxis);   hold off;
  plot(fbcoord(shot,:), fbtime(shot,:)); hold on;
  string = sprintf('First breaks for shot %d ', shot);
  text('units', 'normalized', 'position', [0.1 0.88], 'string', string);
  string = 'cyan - From CVP average database';
  text('units', 'normalized', 'position', [0.1 0.78], 'string', string);
  string = ' red - Repicked from TD stack';
  text('units', 'normalized', 'position', [0.1 0.68], 'string', string);
  %   string = 'blue - Average of std.dev. reject';
  %   text('units', 'normalized', 'position', [0.1 0.58], 'string', string);
  ylabel('Time (ms)');
  
  % Find the valid shots for the left and right side   -----------------
  % This cannot be done with one 'find' because of the double 
  % indexing (non-NaN cpv's among the shotINClist)
  shotlistindex = find( ~isnan(cvpi(shot, shotINClist)) );
  cvpINCleft = shotINClist(shotlistindex);
  cvpALLleft = find( ~isnan(cvpi(shot, :)) );
  
  shotlistindex = find( ~isnan(cvpj(shotINClist, shot)) );
  cvpINCright = shotINClist(shotlistindex);
  cvpALLright = find( ~isnan(cvpj(:, shot)) );
  
  % Set flags for valid left and right side 
  haveleft = (~isempty(cvpINCleft));
  haveright = (~isempty(cvpINCright));
  
  % Calculate the average, the standard deviation and the fold
  % for the left and right cross over point
  if( haveleft )
    cvpavgleft= mean(cvpi(shot,cvpINCleft));
    foldleft = length(cvpINCleft);
    cvpstdleft = std(cvpi(shot,cvpINCleft));
      cvpdiffleft = abs(cvpavgleft - cvpi(shot,cvpINCleft));
    end
    if( haveright )
      cvpavgright = mean(cvpj(cvpINCright,shot));
      foldright = length(cvpINCright);
      cvpstdright = std(cvpj(cvpINCright,shot));
      cvpdiffright = abs(cvpavgright - cvpj(cvpINCright,shot));
    end
    
    % Plot the CVP values        ---------------------------------------
    axes(cvpaxis);    hold off;
    if( haveleft )
      plot(cvpALLleft,cvpi(shot,cvpALLleft), 'r');      hold on;
      plot(cvpINCleft,cvpi(shot,cvpINCleft), 'g');
      l = line([cvpALLleft(1) cvpALLleft(foldleft)], [cvpavgleft cvpavgleft]);
    set(l, 'color', 'y');   end
    if( haveright )
      plot(cvpALLright,cvpj(cvpALLright,shot), 'r');   hold on;
      plot(cvpINCright,cvpj(cvpINCright,shot), 'g');
      r = line([cvpALLright(1) cvpALLright(foldright)],...
	  [cvpavgright cvpavgright]);
      set(r, 'color', 'c');
    end
    string = sprintf('CVPs + CVPavg');
    text('units', 'normalized', 'position', [0.1 0.88], 'string', string);
    string = sprintf('left(yellow), right(cyan)');
    text('units', 'normalized', 'position', [0.1 0.78], 'string', string);
    hold off;   
    
    % Plot the difference (CVP-average) and std deviation lines  --------
    axes(diffaxis);    hold off;
    if( haveleft )
      plot(cvpINCleft,cvpdiffleft, 'y');
      hold on;
      ld = line([cvpINCleft(1) cvpINCleft(foldleft)], [cvpstdleft cvpstdleft]);
      set(ld, 'color', 'y');
    end
    if( haveright )
      plot(cvpINCright,cvpdiffright, 'c');
      rd = line([cvpINCright(1) cvpINCright(foldright)], ...
	  [cvpstdright cvpstdright]);
      set(rd, 'color', 'c');
    end
    
    % userstdev = 1.5;
    string = sprintf('CVP diff with CVPavg + STD DEV');
    text('units', 'normalized', 'position', [0.1 0.88], 'string', string);
    string = sprintf('left(yellow), right(cyan)')
    text('units', 'normalized', 'position', [0.1 0.78], 'string', string);
    % This plot must have the same x-axis as the CVP value plot, above.
    xlimits = get(cvpaxis, 'xlim');
    set(diffaxis, 'xlim', xlimits);
    hold off;
    
    % Ok, now grab the time difference curves and average them (stack)
    shotsCVPleft = find(shotcoord(shotINClist) > shotcoord(shot));
    shotsCVPright = find(shotcoord(shotINClist) < shotcoord(shot));
    [a b]=size(shotsCVPleft);
    [c d]=size(shotsCVPright);
    if(b==1)
      shotsCVPleft=shotsCVPleft';
    end
    if(d==1)
      shotsCVPright=shotsCVPright';
    end
    
    % Use the first break coordinates
    xcoords = fbcoord(shot,:);
    tdfold = zeros(size(xcoords));
    tdiffleft = zeros(size(xcoords));
    tdiffright = zeros(size(xcoords));
    
    f= gcf;
    %  figure;
    
    % Find all time difference curves for the left CVP --------------------
    maxtolerance = 10*tolerance;
    
    % Sort the shotlist (shotsCVPright) in order of increasing distance
    % from the shot
    leftcoords = shotcoord(shotsCVPleft);
    [sortedcoord, sortorder] = sort(leftcoords - shotcoord(shot));
    leftCVPlist = shotsCVPleft(sortorder);
    fvelleft = zeros(size(shotcoord));
    
    f = gcf;  figure('menubar','none');
    stackcount = 0;
    for k=leftCVPlist
      [tdiff1, tdiff2, start1indexj, start2indexj, end1indexj, end2indexj]...
	  = tsub(shot, k, shotcoord, fbcoord, fbtime);
      kcoord = fbcoord(k,start1indexj:end1indexj);
      tdiff = tdiff1; 
      if(~isempty(tdiff))
	side = 1;
	firstshot = (stackcount==0);
	secondshot = (stackcount==1);
	[tdiffleft,tdfold,tolerance,spref,epref,tdref,shotref,lastvel] = ...
	    stackTD(tdiff, kcoord, xcoords, shotcoord, tdfold, ...
	    tdiffleft, side, k, tolerance, maxtolerance, lastvel, ...
	    spref, epref, tdref, shotref, firstshot, secondshot);
	if(isempty(lastvel))
	  fprintf('Whoa!  Flattened velocity is empty... using zero.\n');
	else
	  fvelleft(k) = abs(lastvel);
	end
	stackcount = stackcount+1;
      end
    end
    title('Left TD curves getting stacked (g=found flat,y=not)');
    
    % Plot the velocities used to stack the TD curves
    velfig = figure('menubar','none');  
    plot(shotcoord, fvelleft, 'y');  title('Velocity (y=left,c=right)');
    sprefleft = spref;
    eprefleft = epref;
    shotrefleft = shotref;
    hold off;
    
    % Find all time difference curves for the right CVP  --------------------
    
    % Sort the shotlist in order of increasing distance from the shot
    rightcoords = shotcoord(shotsCVPright);
    [sortedcoord, sortorder] = sort(shotcoord(shot) - rightcoords);
    rightCVPlist = shotsCVPright(sortorder);
    fvelright = zeros(size(shotcoord));
    
    figure('menubar','none');
    stackcount = 0;
    for k=rightCVPlist
      [tdiff1, tdiff2, start1indexj, start2indexj, end1indexj, end2indexj]...
        = tsub(shot, k, shotcoord, fbcoord, fbtime);
    kcoord = fbcoord(shot,start2indexj:end2indexj);
    tdiff = tdiff2;
    if(~isempty(tdiff))
      side = -1;
      firstshot = (stackcount==0);
      secondshot = (stackcount==1);
      [tdiffright,tdfold,tolerance,spref,epref,tdref,shotref,lastvel] = ...
	  stackTD(tdiff, kcoord, xcoords, shotcoord, tdfold, ...
	  tdiffright, side, k, tolerance, maxtolerance, lastvel, ...
	  spref, epref, tdref, shotref, firstshot, secondshot);
      if(isempty(lastvel))
	fprintf('Whoa!  Flattened velocity is empty... using zero.\n');
      else
	fvelright(k) = abs(lastvel);
      end
      stackcount = stackcount+1;
    end
  end
  title('Right TD curves getting stacked (g=found flat,y=not)');
  
  figure(velfig);  hold on;  plot(shotcoord, fvelright, 'c');
  sprefright = spref;
  eprefright = epref;
  shotrefright = shotref;
  
  % Now replace all the 0's in the TD curves with NaN's
  badTDright = find(tdiffright == 0 & isnan(tdiffright)==1);
  goodTDright = find(tdiffright ~= 0 & isnan(tdiffright)~=1);
  tdiffright(badTDright) = NaN.*ones(size(badTDright));
  rightTDcoords = fbcoord(shotrefright, goodTDright);
  
  badTDleft = find(tdiffleft == 0 & isnan(tdiffleft)==1);
  goodTDleft = find(tdiffleft ~= 0 & isnan(tdiffleft)~=1);
  tdiffleft(badTDleft) = NaN.*ones(size(badTDleft));
  leftTDcoords = fbcoord(shotrefleft, goodTDleft);
  
  figure(f); set(gcf,'menubar','none');
  
  % Now average
  k = find(tdfold ~= 0);
  tdiffleft(k) = tdiffleft(k) ./ tdfold(k);
  tdiffright(k) = tdiffright(k) ./ tdfold(k);
  
  % Get the xlimits of the fbtime plot
  xlimits = get(fbaxis, 'xlim');
  
  % Pick the CVP and get the intermediate results
  % Note that the returned derivatives (deltf and delts) have the
  % coordinates as the 2nd row (deltf(2,:), delts(1,:) )
  nd = refdata('get','nd');
  windmn = refdata('get', 'windmn');
  offsetrange1 = refdata('get', 'offsetrange1');
  offsetrange2 = refdata('get', 'offsetrange2');
  
  [mddiffleft, deltfleft, deltsleft, avgcvpleft] = ... 
      pickTD(tdiffleft(goodTDleft), ...
      leftTDcoords, window, windmn, nd, offsetrange1, offsetrange2, ...
      shotcoord(shotrefleft) );
  
  [mddiffright, deltfright, deltsright, avgcvpright] = ...
      pickTD(tdiffright(goodTDright), ...
      rightTDcoords, window, windmn, nd, offsetrange1, offsetrange2, ...
      shotcoord(shotrefright) );
  
  % Plot graphs (TD, median, 1st derv, 2nd derv)
  cvpavgplot('td', tdaxis, xcoords, tdiffleft, xcoords, tdiffright, xlimits);
  cvpavgplot('median', mdaxis, leftTDcoords, mddiffleft, ...
      rightTDcoords, mddiffright, xlimits );
  cvpavgplot('1std', d1axis, deltfleft(2,:), deltfleft(1,:), ...
      deltfright(2,:), deltfright(1,:), xlimits );
  cvpavgplot('2ndd', d2axis, deltsleft(2,:), deltsleft(1,:), ...
      deltsright(2,:), deltsright(1,:), xlimits );
  
  % Plot the CVPavg from the second derivative     ----------------------
  axes(fbaxis);   hold on;
  tleft = 0;     tright = 0;
  if( haveleft & ~isempty(avgcvpleft) & isnan(avgcvpleft)~=1 )
    tleft = interp1(fbcoord(shot,:), fbtime(shot,:), avgcvpleft );
    line( avgcvpleft, tleft, 'linestyle', '+', 'color', 'r');      
  end
  if( haveright & ~isempty(avgcvpright) & isnan(avgcvpright)~=1 )
    tright = interp1(fbcoord(shot,:), fbtime(shot,:), avgcvpright );
    line( avgcvpright, tright, 'linestyle', '+', 'color', 'r');      
  end
  
  % Print out parameter values
  editcvpavg('label');
  
  % Plot the CVPs that were in the database
  cvpal = [];  tl = [];
  cvpar = [];  tr = [];
  cvpavg = refdata('get', 'cvpavg');
  if( haveleft & isnan(cvpavg(shot,1))~=1 )
    cvpal = cvpavg(shot,1);
    tl = interp1(fbcoord(shot,:), fbtime(shot,:), cvpal );
  end
  if( haveright & isnan(cvpavg(shot,2))~=1 ) 
    cvpar = cvpavg(shot,2);
    tr = interp1(fbcoord(shot,:), fbtime(shot,:), cvpar );
  end
  axeslist = [fbaxis tdaxis mdaxis d1axis d2axis];
  plotCVPlines( 'draw', axeslist, cvpal, cvpar, tl, tr );
  
  % Initialize the movement function, to allow manual editing of picks
  movecvpavg('init');
end
  
if( strcmp(action, 'label') )
   [shotrange shotINClist shot axeslist stddev] = avgcvpinfo('get');
   axes(axeslist(7));   % Use the diffaxis
   tolerance = refdata('get', 'flatslope');
   shotgap = refdata('get', 'shotgap' );
   shotlength = refdata('get', 'shotlength' );
   window = refdata('get','window');
   windowmn = refdata('get','windmn');
   nd = refdata('get', 'nd');
 
   % Delete the current text labels
   c = get(gca, 'children');
   for i=1:length(c)
      type = get(c(i),'type');
      if( strcmp(type,'text') )
         delete(c(i));
      end
   end
   % Write out the parameter values, attached to the CVP difference axes
   texty = -.3;
   t1 = text('units', 'normalized', 'position', [0 texty] );
   ts = sprintf('Maximum flat slope (%%): %3.1f', tolerance*100);
   set(t1, 'string', ts);
   tmp = get(t1, 'extent');
   textheight = tmp(4) * 0.6;
   texty = texty - textheight;
   t2 = text('units', 'normalized', 'position', [0 texty] );
   ts = sprintf('Standard deviation limit: %3.1f', stddev);
   set(t2, 'string', ts);
   texty = texty - textheight;
   t3 = text('units', 'normalized', 'position', [0 texty] );
   ts = sprintf('Shot gap: %d, shots/side: %d', shotgap, shotlength);
   set(t3, 'string', ts);
   texty = texty - textheight;
   t4 = text('units', 'normalized', 'position', [0 texty] );
   ts = sprintf('Current shot: %d in range %d - %d', ...
                 shot, shotrange(1), shotrange(length(shotrange)) );
   set(t4, 'string', ts);
   texty = texty - textheight;
   t5 = text('units', 'normalized', 'position', [0 texty] );
   ts = sprintf('Diffentiation seperation: %d', nd);
   set(t5, 'string', ts);
   texty = texty - textheight;
   t6 = text('units', 'normalized', 'position', [0 texty] );
   ts = sprintf('Filtering windows: median: %d, mean: %d', window, windowmn);
   set(t6, 'string', ts);
end
if( strcmp(action,'done'))
   movecvpavg('clear');
   % Delete any buttons or axes that are on the screen
   c = get(gcf, 'children');
   [nc tmp] = size(c);
   for i=1:nc
      type = get(c(i),'type');
      if( strcmp(type,'uicontrol') | strcmp(type,'axes') )
         delete(c(i));
      end
   end
end