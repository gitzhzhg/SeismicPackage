% Examples4LogOnSeismicPlot
% Plot a log curve at a specified CDP on top of a seismic section

keep WF
presets

% global S4M

%     Get test data
seismic=s_data;
wlog=l_data;

%%     Convert log from depth to time
%     Add curve with two-way time (TWT)
wlog=l_depth2time(wlog,{'depth_time',8000,100}); 
%     Remove all rows for which TWT has null values
wlog=l_rm_nulls(wlog,'anywhere','twt');
%      Make two-way time the first column
wlog=l_switch_depth(wlog,'twt');
%     Select the desired curve (density); remove all others
wlog=l_select(wlog,{'curves','rho'});
%     Create equi-distantly samples log curves in two-way travel time
step=2;      % Sample interval in ms of the depth-to-time converted well log
wlog=l_resample(wlog,step);

%     Convert log structure into a seismic structure for the requested curve
tlog=s_log2seismic(wlog);
%     Select the same time interval as the seismic
tlog=s_select(tlog,{'times',seismic.first,seismic.last},{'null',NaN});
%     Remove the mean so that the curve will be plotted centered about the
%     trace number
tlog=tlog-nan_mean(tlog.traces);

%%     Create a copy of the seismic dataset with all trace values set to
%      NaNs and sampled with the sample interval selected for the log curve;
slog=s_resample(seismic,step);
slog.traces(1:end,1:end)=NaN;

%     Find the trace number with the requested CDP (107) and copy the log curve 
%     to that trace; the only trace with valid values will be the one with the
%     log curve
loc=s_trace_numbers(seismic,'CDP',107); % Find the trace number with CDP == 107
slog.traces(:,loc)=tlog.traces;         % Copy the log curve to that trace


%%     Plot seismic
s_wplot(seismic)                        % Plot the seismic in wiggle trace format

%     Plot log curve on top of the seismic data
s_wplot(slog,{'figure','old'},{'wiggle','red'},{'peak_fill',''}, ...
   {'interp','linear'},{'wiggle_width',1.5},{'annotation','CDP'}, ...
   {'deflection',1})


%%     Plot seismic in color
s_cplot(seismic,{'shading','interp'})    % Plot the seismic as a color image

%     Plot log curve on top of the seismic data
s_wplot(slog,{'figure','old'},{'wiggle','white'},{'peak_fill',''}, ...
   {'interp','linear'},{'wiggle_width',1.5},{'annotation','CDP'}, ...
   {'deflection',1})
