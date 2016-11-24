% Examples4SeismicColorPlots
% The script illustrates the use of some of the parameters available to control
% a color plot

keep WF
presets
global S4M    %#ok


%%     Get data for the examples
seismic=s_data;

%% 1  All kewords are set to defaults
   s_cplot(seismic)
   mytitle('Example 1: Default settings')


lfigure
subplot(1,2,1)
%% 2a  All kewords are set to defaults
   s_cplot(seismic,{'figure','old'},{'title','Example 2b: Colors not interpolated'})
   set(gca,'XAxislocation','bottom')
%   title('Example 2a: Colors not interpolated')

subplot(1,2,2)
%% 2b  Create an interpolated color plot
   s_cplot(seismic,{'shading','interp'},{'figure','old'}, ...
                {'title','Example 2b: Colors are interpolated'})
   set(gca,'XAxislocation','bottom')

   
%% 3  Trace annotation by CDP number; this assumes that the header "CDP" 
%%     exists
s_cplot(seismic,{'annotation','CDP'}, ...
                {'title','Example 3: Trace annotation by CDP number'})
 
 
%% 4  Top plot: traces plotted from right to left (default is left to right);
%%    size of the axis annotation etc. is 11 points (default)
%%    Bottom plot: traces plotted from left to right (default); 
%%    This subplot uses the 13-point font size.
pfigure        % Plot figure in portrait format
subplot(2,1,1)
   aux=s_cplot(seismic,{'direction','r2l'},{'figure','old'}, ...
                       {'fontsize',11'}, ...
                       {'title','Traces plotted from right to left; normal font size'});
   set(gca,'XAxislocation','bottom')
   
subplot(2,1,2)
   s_cplot(seismic,{'direction','l2r'},{'figure','old'}, ...
                   {'fontsize',13'}, ...
                   {'title','Traces plotted from left to right; big font size'});
   set(gca,'XAxislocation','bottom')


                
%% 5 Wiggle trace over color
s_cplot(seismic,{'shading','interp'})
hold on
s_wplot(seismic,{'figure','old'},{'peak_fill',''},{'wiggle_width',1.0}, ...
                {'deflection',0.6},{'wiggle_color','white'})
mytitle('Example 5: Wiggle trace over color')

