function [combi,syn,aux]=s_2d_spliced_synthetic(seismic,wavelet,refl,varargin)
% Function creates display of seismic data with spliced-in synthetic
% Also computes trace-to-trace correlations for in-line and cross-line
% Written by: E. Rietsch; April 3, 2001
% Last updated: September 13, 2004: bug fix in call to "s_trace_numbers"
%
%             combi=s_2d_spliced_synthetic(seismic,wavelet,refl,varargin)
% INPUT
% seismic     seismic data set 
% wavelet     wavelet to use; it is assumed that wavelet and seismic data have headers
%             defined via keyword 'headers' 
% refl        reflection coefficient series
% varargin one or more cell arrays; the first element of each cell array is a keyword
%          string, the following arguments contains a parameter(s). 
%          Accepted keywords are:
%          'frequencies' first and last frequency to display in spectrum plot
%                 Default: {'frequencies',0,fny} where "fny" denotes the Nyquist frequency
%          'header'  header of seismic to annotate traces
%          'orient'  orientation of seismic plot; possible values are 'portrait' and 'landscape'.
%                 Default: {'orient','portrait'}
%          'quality' quality of seismic display; posssible values are 'high' and 'draft'
%                 Default: {'quality','high'}
%          'scale'       2-element cell array which specifies if traces should be scaled individually.
%                 Possible values are 'yes', 'no', or the actual scale. This may be a scalar or a
%                 vector whose number of elements is equal to the number of traces.
%                 separation. The scale actually used must be obtained by specifying the output
%                 argument "aux".
%                 Default: {'scale','no'}
%          'times'   start and end time of displayed seismic traces
%                 Default: {'times',ta,te} where "ta" is the greatest multiple of 100
%                       less than the start time of the synthetic and "te" is the smallest
%                       multiple of 100 greate than the end time of the synthetic
%          'traces' number of synthetics spliced in.
%                 Default: {'traces',3}
% OUTPUT
% combi    seismic in-line with spliced-in synthetics
% syn      synthetic
% aux      structure with figure handles
%     'figure_handles'  Figure handles of seismic plot and trace-to-trace discrepancy


%       Compute synthetic
syn=s_convolve(wavelet,refl);

%       Set defaults for input parameters
param.frequencies={0,500/seismic.step};
param.header='cdp';
param.orient='portrait';
param.quality='high';
param.scale='no';
param.times={floor(syn.first/100)*100,ceil(syn.last/100)*100};
param.traces=3;

%       Decode and assign input arguments
param=assign_input(param,varargin);

ntr=size(seismic,2);

%       Prepare synthetics for being spliced into seismic
syns=s_select(syn,{'traces',ones(param.traces,1)});

%	Find cdp (header) number of best-matching wavelets
header=s_gh(wavelet,param.header);
% iline=s_select(seismic,{'traces',[param.header,' == ',num2str(header)]});
iline=seismic;

%       Plot seimic line with spliced-in synthetic
iindex=s_trace_numbers(iline,param.header,header);
if iindex == ntr
  iindex=iindex-1; 
end

combi=s_append(s_select(iline,{'traces',1:iindex}),syns);
combi=s_append(combi,s_select(iline,{'traces',iindex+1,inf}));
%iplot_title=[s_gd(seismic,param.header),' ',num2str(header),' with synthetic'];
iplot_title=[seismic.name,' with synthetics'];

aux=s_iplot(combi,{'times',param.times{1},param.times{2}},{'scale',param.scale}, ...
   {'orient',param.orient},{'title',iplot_title},{'quality',param.quality}, ...
   {'annotation',param.header});


%       Plot trace-to-trace correlations
ntr=size(combi.traces,2);
temp1=s_select(combi,{'times',max([iline.first,syns.first]),min([iline.last,syns.last])}, ...
      {'traces',1:ntr-1});
temp2=s_select(combi,{'times',max([iline.first,syns.first]),min([iline.last,syns.last])}, ...
      {'traces',2:ntr});

icorr=s_correlate(temp1,temp2,{'option','corresponding'},{'normalize','traces'},{'lags',0,0});

fig_handle=pfigure;
plot(icorr.traces,'r','LineWidth',2)
vv=axis;
axis([vv(1:2),0,1]);
title(iplot_title)
grid on, zoom on
mytitle('Trace-to-trace correlation')
ylabel('Correlation coefficient')
set(gca,'XAxisLocation','top')
% figure_export_menu(fig_handle)

if nargout > 2
   aux.figure_handles=[aux.figure_handle,fig_handle];
end
