function [icombi,xcombi,syn,aux]=s_3d_spliced_synthetic(seismic,wavelet,refl,varargin)
% Function creates display of seismic data with spliced-in synthetic
% Also computes trace-to-trace correlations for in-line and cross-line
% Written by: E. Rietsch; January 6, 2001
% Last updated: June 5, 2004: Removed spectrum computation; added output of figure handles
%
%             [icombi,xcombi,syn,aux]=s_3d_spliced_synthetic(seismic,wavelet,refl,varargin)
% INPUT
% seismic     seismic data set 
% wavelet     wavelet to use; it is assumed that wavelet and seismic data have headers
%             defined via keyword 'headers' 
% refl        reflection coefficient series
% varargin one or more cell arrays; the first element of each cell array is a keyword
%          string, the following arguments contain(s a) parameter(s). 
%          Accepted keywords are:
%          'frequencies' first and last frequency to display in spectrum plot
%                 Default: {'frequencies',0,fny} where "fny" denotes the
%                 Nyquist frequency
%          'headers' headers of seismic to use to locate well on seismic lines
%          'mark' mark range of reliabily of synthetic by red vertical bars along
%                 vertical axis. The range of reliability is the time interval
%                 for which the synthetic does not need data outside the
%                 reflevtion coefficient interval.
%                 Default: {'mark','yes'}
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
% icombi    seismic in-line with spliced-in synthetics
% xcombi    seismic cross-line with spliced-in synthetics
% syn       synthetic
% aux       structure with figure handles
%     'figure_handles'  Figure handles of inline plot, cross-line plot, and 
%           trace-to-trace discrepancy

% global S4M

%       Compute synthetic
syn=s_convolve(wavelet,refl);

%       Set defaults for input parameters
param.frequencies={0,500/seismic.step};
param.headers={'iline_no','xline_no'};
param.mark='yes';
param.orient='portrait';
param.quality='high';
param.scale='no';
param.times={floor(syn.first/100)*100,ceil(syn.last/100)*100};
param.traces=3;

%       Decode and assign input arguments
param=assign_input(param,varargin);

%       Prepare synthetics for being spliced into seismic
syns=s_select(syn,{'traces',ones(param.traces,1)});

%	Find in-line number and cross-line number of best-matching wavelets
header1=s_gh(wavelet,param.headers{1});
header2=s_gh(wavelet,param.headers{2});
%iline=s_select(seismic,{'traces',[param.headers{1},' == ',num2str(header1)]});
iline=s_select(seismic,{'traces',param.headers{1},header1});
xline=s_select(seismic,{'traces',param.headers{2},header2});


%       Plot seimic line with first header with spliced-in synthetic
iindex=s_trace_numbers(iline,param.headers{2},header2);
if iindex == size(iline.traces,2)
  iindex=iindex-1; 
end

icombi=s_append(s_select(iline,{'traces',1:iindex}),syns);
icombi=s_append(icombi,s_select(iline,{'traces',iindex+1,inf}));

iplot_title=[s_gd(seismic,param.headers{1}),' ',num2str(header1),' with synthetic'];
aux1=s_iplot(icombi,{'times',param.times{1},param.times{2}},{'annotation','xline_no'}, ...
   {'orient',param.orient},{'title',iplot_title},{'quality',param.quality}, ...
   {'scale',param.scale});

if strcmp(param.mark,'yes')
   wtime=wavelet.last-wavelet.first;
   ta=syns.first+wtime;
   te=syns.last-wtime;
   hold on
   v=axis;
   plot(v([1,1]),[ta,te],'r','Linewidth',5)
   plot(v([2,2]),[ta,te],'r','Linewidth',5)
end

%       Plot seimic line with second header with spliced-in synthetic
xindex=s_trace_numbers(xline,param.headers{1},header1);
if xindex == size(xline.traces,2)
   xindex=xindex-1; 
end
xcombi=s_append(s_select(xline,{'traces',1:xindex}),syns);
xcombi=s_append(xcombi,s_select(xline,{'traces',xindex+1,inf}));

xplot_title=[s_gd(seismic,param.headers{2}),' ',num2str(header2),' with synthetic'];
aux2=s_iplot(xcombi,{'times',param.times{1},param.times{2}},{'annotation','iline_no'}, ...
   {'orient',param.orient},{'title',xplot_title},{'quality',param.quality}, ...
   {'scale',param.scale});

if strcmp(param.mark,'yes')
  v=axis;
  hold on
  plot(v([1,1]),[ta,te],'r','Linewidth',5)
  plot(v([2,2]),[ta,te],'r','Linewidth',5)
end

%       Plot trace-to-trace correlations
ntr=size(icombi.traces,2);
temp1=s_select(icombi,{'times',max([iline.first,ta]),min([iline.last,te])}, ...
      {'traces',1:ntr-1});
temp2=s_select(icombi,{'times',max([iline.first,ta]),min([iline.last,te])}, ...
      {'traces',2:ntr});

icorr=s_correlate(temp1,temp2,{'option','corresponding'},{'normalize','traces'},{'lags',0,0});

ntr=size(xcombi.traces,2);
temp1=s_select(xcombi,{'times',max([xline.first,ta]),min([xline.last,te])}, ...
      {'traces',1:ntr-1});
temp2=s_select(xcombi,{'times',max([xline.first,ta]),min([xline.last,te])}, ...
      {'traces',2:ntr});
xcorr=s_correlate(temp1,temp2,{'option','corresponding'},{'normalize','traces'},{'lags',0,0});

fig_handle=pfigure;

subplot(2,1,1)
plot(icorr.traces,'r','LineWidth',2)
vv=axis;
axis([vv(1:2),0,1]);
mytitle(iplot_title)
grid on, zoom on

subplot(2,1,2)
plot(xcorr.traces,'r','LineWidth',2)
vv=axis;
axis([vv(1:2),0,1]);
mytitle(xplot_title)

grid on, zoom on
mysuptitle('Trace-to-trace correlation for in-line and cross-line')
figure_export_menu(fig_handle)

if nargout > 3
  aux.figure_handles=[aux1.figure_handle,aux2.figure_handle,fig_handle];
end
