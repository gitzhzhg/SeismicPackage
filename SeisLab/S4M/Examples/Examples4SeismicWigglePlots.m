% Examples4SeismicWigglePlots
% The script illustrates the use of some of the parameters available to control
% a wiggle-trace plot

keep WF
presets
global S4M    %#ok


%%     Get data for the examples
seismic=s_data;


%% 1  All kewords are set to defaults
s_wplot(seismic)
mytitle('Example 1: Default settings')


%% 2  Create a high-quality plot with gray background 
%    (setting the quality parameter to 'high' prevents vertical zero-deflection
%    lines which show up under some circumstances --- in particular when 
%    saving or printing the plot; it is much slower than the draft-quality plot)
s_wplot(seismic,{'quality','high'},{'background','gray'}, ...
                {'title','Example 2: high-quality and gray background'})

 
%% 3  Use the "quality" keyword to create a spike display 
%    (intended to display reflection coefficients) and background color defind
%    via an RGB triplet; the thickness of the spikes can be controlled via 
%    keyword "wiggle_width".
s_wplot(seismic,{'quality','spikes'},{'background',[0.25,0.25,0.25]}, ...
                {'title','Example 3: Spike display with dark-gray background'})

 
%% 4  Trace annotation by CDP number, but only annotate traces 1, 3, 7,
%     8, and 12; this assumes that the header "CDP" exists
s_wplot(seismic,{'annotation','CDP'},{'aindex',[1 3 7 8 12]}, ...
                {'title','Example 4: Trace annotation by CDP number'})
 
 
%% 5   No wiggle but peak filled read and trough filled gray
s_wplot(seismic,{'peak_fill','red'},{'trough_fill','gray'},{'wiggle_color',[]}, ...
                {'title','Example 5: Peak and trough fill but no wiggle_color'})
 

%% 6  Top plot: traces plotted from right to left (default is left to right);
%    it uses a trace deflection of 0.9 traces spacings; The font
%    size of the axis annotation etc. is 13 points.
%    Bottom plot: traces plotted from left to right (default); 
%    the plot uses the output of the first plot function to display the 
%    the traces with the same trace deflection; it is wiggle-only and 
%    uses the default 11-point font.
pfigure        % Plot figure in portrait format
subplot(2,1,1)
   aux=s_wplot(seismic,{'direction','r2l'},{'deflection',0.9},{'figure','old'}, ...
                       {'fontsize',13'},{'title',''});
subplot(2,1,2)
   s_wplot(seismic,{'direction','l2r'},{'scale',aux.scale},{'figure','old'}, ...
                   {'peak_fill',''},{'title',''});
mysuptitle('Example 6: trace deflection is 0.9 trace spacings',{'factor',0.93})


%% 7a Plot the 5-th and the 8-th trace in red (brute force approach)
temp=seismic;
bool=false(1,12);                % The seismic dataset has 12 traces
bool([5,8])=true;
temp.traces(:,bool)=NaN;         % Set traces 5 and 7 to NaN
temp.null=NaN;                   % Dataset has null values (this is only done to avoid warnings)
s_wplot(temp,{'deflection',1})   % Plot the datset
hold on
temp=seismic;
temp.traces(:,~bool)=NaN;        % Set all traces except 5 and 7 to NaN
temp.null=NaN;                   % Dataset has null values (this is only done to avoid warnings)
s_wplot(temp,{'deflection',1},{'wiggle_color','red'},{'peak_fill','red'}, ...
             {'figure','old'})   % Plot this dataset on top of the one 
                                                      % plotted before
mytitle('Example 7a: Traces plotted in black and red')


%% 7b Plot the 5-th and the 8-th trace in different colors (handle manipulation)
aux=s_wplot(seismic,{'trough_fill','white'},{'deflection',1});   % Plot the datset.
                                  % Notice that the trough-fill color is specified
set(aux.handles([5,8],1),'FaceColor','green')          % Peak fill (green)
set(aux.handles([5,8],2),'FaceColor',[0.6,0.6,0.6])    % Trough fill (gray)
set(aux.handles([5,8],3),'Color','red','LineWidth',1)  % Wiggle width and color (red)
mytitle('Example 7b: Traces plotted in black and other colors')


%% 9 Wiggle trace over color
s_cplot(seismic,{'shading','interp'})
hold on
s_wplot(seismic,{'figure','old'},{'peak_fill',''},{'wiggle_width',1.5}, ...
                {'deflection',0.6},{'wiggle_color','black'})
mytitle('Example 9: Wiggle trace over color')


%% 10 Custom wiggle plot with color-coded troughs
s_xplot(seismic,{'title','Example 10: Custom wiggle plot with color-coded troughs'})


%% 11 Use plot scale factor of one dataset for another dataset
seismic1=s_data;
seismic2=0.5*seismic1; % Create a dataset with twice the amplitudes
lfigure
subplot(1,2,1)
   aux=s_wplot(seismic1,{'figure','old'},{'title', 'Dataset'});
subplot(1,2,2)
   s_wplot(seismic2,{'figure','old'},{'scale',aux.scale},{'title', '0.5*Dataset (same scale)'})
%   mytitle(' ')
   mysuptitle('Example 11: Plot of two equally scaled datasets',{'color','blue'})
   
  
