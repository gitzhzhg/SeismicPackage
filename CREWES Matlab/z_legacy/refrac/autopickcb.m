function autopickcb(action)
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

% Determination of the shot pair(s) entering in the cross over point calculation 
if( nargin < 1 )
   action = 'init';
end
if( strcmp(action,'init'))
   q=str2mat('CVP autopick range:', ...
             'Enter shot pair if "shot pair" is selected:',...
             'Remove some shot pairs if "all" is selected?',...
             'If removing some shots, enter the reciprocal time difference limit:', ...
             'Median filter window length (odd number, minimum 3):',...
             'Seperation length for the differentiation (max 7):',...
             'Apply mean filter to first derivative?', ...
             'Mean filter window length (min. 2):', ...
             'Limit the offset?',...
             'Specify offset limit:');
   a=str2mat('all|shot pair (i,j)',...
             '10 15',...
             'No|Yes', '5', '5', '1', 'No|Yes', ...
             '5', 'No|Yes', '0 600' );
   askthingsinit('autopickcb(''answer'')',q,a,[1 0 0 0 1 1 1 0 1 0],...
                 'Parameter for the Autopicking function');
elseif( strcmp(action,'answer'))
   a=askthingsfini;
   [strings tmp] = size(a);
   appair = sscanf(a(2,:), '%d %d');
   appair1 = appair(1);
   appair2 = appair(2);
   rtmax = str2num(a(4,:));
   window = str2num(a(5,:));
   nd = str2num(a(6,:));
   refdata('set', 'window', window);
   refdata('set','nd',nd);
   if( strcmp( deblank(a(1,:)), 'all') )
	aprange=0;
	if( strcmp( deblank(a(3,:)), 'Yes') )
           apsub = 0;
	   refdata('set','rtmax',rtmax);% reciprocal time difference limit accepted
        else
           apsub = 1;   
	end
   else
      aprange=1;
      apsub=refdata('get','apsub');
      refdata('set', 'appair1', appair(1) );
      refdata('set', 'appair2', appair(2) );
   end
   refdata('set','aprange',aprange);   %  Single pair aprange is 1
   refdata('set','apsub',apsub);   % Shot pair subtraction from the autopicking
   if( strcmp( deblank(a(7,:)), 'No' ))
      windmn=NaN;
   else
      windmn=str2num(a(8,:));
   end   
   if( strcmp( deblank(a(9,:)), 'No' ))
      offsetrange1 = NaN;
      offsetrange2 = NaN;
   else
      offsetrange = sscanf(a(10,:), '%d %d');
      offsetrange1 = offsetrange(1,1);
      offsetrange2 = offsetrange(2,1);
   end
 
   refdata('set','windmn',windmn);
   refdata('set','offsetrange1',offsetrange1);
   refdata('set','offsetrange2',offsetrange2);
   fbcoord = refdata('get','fbcoord');
   shotcoord = refdata('get','shotcoord');
   fbtime = refdata('get','fbtime'); 
   nshots = refdata('get','nshots');
   nrecs = refdata('get','nrecs');
   if( aprange==0)
        % Then do all shots   if( strcmp( deblank(a(1,:)), 'all') )
 	if( apsub==0)
                % Then subtract shot pairs with reciprocal time > rtmax
		rtrange=aprange
		rtpair1=appair1
		rtpair2=appair2
		mint=rtmax
		diffmat = refdata('get','diffmat');
		[a b] = size(diffmat);
		if( a < nshots-1 )
		[diffmat]=rectime(rtrange,rtpair1,rtpair2,fbcoord,shotcoord,fbtime,mint,nshots);
		% And save the diffmatrix
		refdata('set','diffmat',diffmat);
		end
		absdiffmat=abs(diffmat);
		% gs1 & gs2 are the shot pairs with a reciprocal time less than rtmax
		[gs1,gs2]=find(absdiffmat<rtmax);
		m=size(gs1);
	else
		k = 1;
		for i=1:nshots-1
			for j=i+1:nshots
				gs1(k) = i;
				gs2(k) = j;
				k = k+1;
			end
		end
	end
   else
	gs1=appair1
	gs2=appair2
   end
   % Call the Cross over point picking function
   [cvpi, cvpj, tdiff1, tdiff2, mddiff1, mddiff2, ...
    deltf1, deltf2, delts1, delts2, ...
    start1indexj, end1indexj, start2indexj, end2indexj ] = ...
    autopick( gs1, gs2, shotcoord, fbcoord, fbtime, window, ...
              nshots, nd, offsetrange1, offsetrange2, windmn );
   % Only save the reciprocal time difference matrix if all shot pairs have been computed
   if( aprange == 0 )
      refdata('set','cvpi',cvpi);
      refdata('set','cvpj',cvpj);
   else
      % Update the cvpi and cvpj matricies for the two new picks.
      if (shotcoord(gs1)>shotcoord(gs2))
	j=gs1;
	i=gs2;
      else
	i=gs1;
	j=gs2;
      end
      cvpis = refdata('get','cvpi');
      cvpis(i,j) = cvpi(i,j);
      refdata('set','cvpi',cvpis);
      cvpjs = refdata('get','cvpj');
      cvpjs(i,j) = cvpj(i,j);
      refdata('set','cvpj',cvpjs);
   end
   fbtime=refdata('get','fbtime');
   fbcoord=refdata('get','fbcoord');
   shotcoord=refdata('get','shotcoord');
   % Update menus
   PMTsetmenus;
 
   % We need to save the current figure, and restore it afterwards, so that
   % refdata (which stores data under the MAIN figure) will work.
   f = gcf;
   % Display of the two cross over point matrix: cvpi and cvpj
   if( aprange==0)
	figure('menubar','none');
	plot(cvpi','c.');
	hold on;
	plot(cvpj,'r.');
	xlabel('Shot number');
	ylabel('Coordinate (m)');
	title('Cross over point coordinates for all shots (cvpi in blue and cvpj in red)')
        figure('menubar','none');
        hold on
        for n=1:nshots
           validi=find(isnan(cvpi(n,:)));
           validj=find(isnan(cvpj(:,n)));
           offi=cvpi(n,validi)-shotcoord(n);
           offj=cvpj(validj,n)-shotcoord(n);
           l=length(offi);
           r=length(offj);
           plot(shotcoord(n)*ones(1,l),offi,'c.')
           plot(shotcoord(n)*ones(1,r),offj,'r.')
        end
        xlabel('Shot coordinate (m)')
        ylabel('Offset (m)')
        title('Cross over point offsets for all shots (cvpi (left) in blue and cvpj (right) in red)')
else
   % Display of the two cross over point (cvpi and cvpj) for one shot pair 
   % with the time difference curves (TD), the Median filter of the TD curves,
   % and the first and second derivative of the filtered   curves
   figure('menubar','none')
   % Assign axis [left bottom width height]
   fbaxis = axes('position', [.1 .59 .80 .35]);     % First break
   tdaxis = axes('position', [.1 .46 .80 .10]);     % Time difference
   mdaxis = axes('position', [.1 .32 .80 .10]);     % Median filtered TD
   d1axis = axes('position', [.1 .20 .80 .10]);     % 1st derivative of TD
   d2axis = axes('position', [.1 .08 .80 .10]);     % 2nd derivative of TD
   s1j = start1indexj;
   s2j = start2indexj;
   e1j = end1indexj;
   e2j = end2indexj;
   % Plot the refracted arrivals for the two shots
   axes(fbaxis);
   plot(fbcoord(i,:), fbtime(i,:));  hold on;
   plot(fbcoord(j,:), fbtime(j,:));
   string = sprintf('Arrivals for shot pair: %d %d', i, j);
   text('units', 'normalized', 'position', [0.38 0.88], 'string', string);
   ylabel('Traveltime (ms)');
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
   text('units', 'normalized', 'position', [0.7 0.88], 'string', string);
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
%   set(mdaxis,'xtick',[]);
   hold off;
   string = 'Median filtered TD';
   text('units', 'normalized', 'position', [0.7 0.88], 'string', string);
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
   text('units', 'normalized', 'position', [0.7 0.88], 'string', string);
   ylabel('1st amp.');
      
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
   xlabel('Coordinate (m)')
   ylabel('2nd amp.')
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
%   set(gcf,'units','pixels','position',[0 0 864 864]);
%   figure
%   hold on;
%   plot(fbcoord(i,:),fbtime(i,:))
%   plot(fbcoord(j,:),fbtime(j,:))
%   xlabel('xcoordinate (m)');
%   ylabel('traveltime (ms)');
%   titlestr = sprintf('Cross over point for the shot pair (i,j): %d %d',i,j);
%   title(titlestr);
%   str=sprintf('%d',i); 
%   text(shotcoord(i),10,str)
%   str=sprintf('%d',j); 
%   text(shotcoord(j),10,str)
%
%   cvpi(i,j)
%   cvpj(i,j)
%   if (isnan(cvpi(i,j))~= 1)
%      timei=interp1(fbcoord(i,:),fbtime(i,:),cvpi(i,j));
%      plot(cvpi(i,j),timei,'co')
%   end
%   
%   if (isnan(cvpj(i,j))~= 1)
%      timej=interp1(fbcoord(j,:),fbtime(j,:),cvpj(i,j));
%      plot(cvpj(i,j),timej,'r*')
%   end
   end
   figure(f); set(gcf,'menubar','none');
end