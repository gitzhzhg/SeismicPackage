function aux=s_header_plot(seismic,headers,varargin)
% Function plots values of seismic headers
%
% Written by: E. Rietsch: July 22, 2000
% Last updated: January 30, 2007: add cursor tracking
%
%           aux=s_header_plot(seismic,headers,varargin)
% INPUT
% seismic   seismic structure whose headers should be plotted
% headers   header mnemonic or cell array with header mnemonics
%           if one header mnemonic is given it is plotted as a function of trace number
%           if two or more headers are specified, the first one represents the x-axis;
%           the other headers are plotted against the first
%           if the the headers are to be plotted against the trace number, the first 
%           element of cell array headers can be the empty string ('') or 'trace_no'
%           No defaults.
%           Examples:  'cdp',  {'trace_no','iline_no','xline_no'}, {'cdp_x','cdp_y'}
% varargin  one or more cell arrays; the first element of each cell array is a 
%           keyword, the other elements are parameters. Presently, keywords are:
%           'colors'     Colors to be used for consecutive curves.
%                  Possible values: any permissible colors and line styles
%                  Default: {'colors','r','g','b','m','k','c', ...
%                               'r--','g--','b--','m--','k--','c--', ...
%                               'r:','g:','b:','m:','k:','c:'};
%           'linewidth'  Width of the lines. Default: {'linewidth',2}
%           'figure'   If set to 'new' a new figure is created. Any other string
%                  plots to the current axes of an existing figure. 
%                  Default: {'figure','new'} 
%           'orient'    Possible values are 'portrait' and 'landscape'. 
%                  Default: 'landscape'.
%           'scale'       2-element cell array which specifies if curves should 
%                  be scaled individually. Possible values are 'yes' or 'no'. 
%                  Default: {'scale','yes'}
%           'title'     Plot title. Default: 'Headers of seismic data set "seismic"'
%                  where "seismic" is the name of the seismic data set (first input argument)
% OUTPUT
% aux       structure with possibly relevant information
%      figure_handle   figure handle
%      curve_handles         vector with the curve handles, so that curve attributes can be changed

if ~istype(seismic,'seismic')
   error(' First input argument must be a seismic dataset.')
end

%	Set default values
param.colors={'r','g','b','m','k','c','r--','g--','b--','m--','k--','c--', ...
              'r:','g:','b:','m:','k:','c:'};
param.figure='new';
param.linewidth=2;
param.lloc=[];
param.orient='landscape';
param.scale='yes';
param.title='';

%       Decode and assign input arguments
param=assign_input(param,varargin);

if ~iscell(param.colors)
   param.colors={param.colors};
end

%       Check if new figure is requested
if strcmpi(param.figure,'new')
   if strcmpi(param.orient,'portrait')
      figure_handle=pfigure;
   else
      figure_handle=lfigure;
   end

   if isempty(param.title)
     mytitle(['Headers of seismic data set "', ...
         strrep(seismic.name,'_','\_'),'"'])
   else
     mytitle(param.title)
   end
end
hold on

if iscell(headers) && length(headers) > 1
   if isempty(headers{1}) || strcmpi(headers{1},'trace_no')
      x=1:size(seismic.headers,2);
      idx=header_index(seismic,headers(2:end));
      xlabel('Trace number')
      xinfo={'trace_no,','n/a','Trace number'};
   else
      idx=header_index(seismic,headers);
      x=seismic.headers(idx(1),:);
      xlabel(header_label(seismic.header_info(idx(1),:)))
      xinfo=seismic.header_info(idx(1),:);
      idx=idx(2:end);
   end
   curve_handles=zeros(length(headers)-1,1);
   ltext=cell(length(headers)-1,1);

   for ii=2:length(headers)
      if length(headers) > 2 && strcmpi(param.scale,'yes')
         curve_handles(ii-1)=plot(x,seismic.headers(idx(ii-1),:)/max(abs(seismic.headers(idx(ii-1),:))), ...
            param.colors{ii-1},'LineWidth',param.linewidth);
      else
         curve_handles(ii-1)=plot(x,seismic.headers(idx(ii-1),:), ...
            param.colors{ii-1},'LineWidth',param.linewidth);
      end
      ltext(ii-1)={strrep(headers{ii},'_','\_')};
   end
   if length(headers) > 2
      if isempty(param.lloc)     % Set location of legend
        loc=1;
      else
        loc=param.lloc;
      end
      legend(char(ltext),loc);
      if strcmpi(param.scale,'yes')
        ylabel('Values scaled to a maximum of 1')
      end
   else
      ylabel(header_label(seismic.header_info(idx,:)))
   end
   initiate_2d_tracking(xinfo,{'header_value','','Header value'})
      
else
   if iscell(headers), headers=headers{1}; end
   if iscell(param.colors), param.colors=param.colors{1}; end
   idx=header_index(seismic,headers);
   curve_handles=plot(seismic.headers(idx,:),param.colors(1),'LineWidth',param.linewidth);
   xlabel('Trace number')
   yinfo=seismic.header_info(idx,:);
   ylabel(header_label(yinfo))
   initiate_2d_tracking({'trace_no','n/a','Trace number'},yinfo)
end

grid on
zoom on

if nargout == 1
   aux.figure_handle=figure_handle;
   aux.curve_handles=curve_handles;
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function text=header_label(header_info)
% Function composes a label for plots from the header information in header_info
% INPUT
% header_info   Cell array; second element represents header units, third element
%     header description
% OUTPUT
% text  text for headef label in plots

text=strrep(header_info{3},'_','\_');
if ~isempty(header_info{2}) && ~strcmpi(header_info{2},'n/a')
  text=[text,' (',header_info{2},')'];
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [index,ier]=header_index(seismic,varargin)
% Function outputs index (indices) of header(s) "headers" in seismic structure "seismic".
% Aborts with an error message if one or more of the headers do not exist.
%         [index,ier]=header_index(seismic,varargin)
% INPUT
% seismic  seismic structure whose headers are requested
% varargin one or more header mnemonics
% OUTPUT
% index    index into fields header_info and headers (the headers are seismic.headers(index,:)
%          and the header information is seismic.header_info(index,:))
% ier      error code; if a second output argument is specified the function will not abort
%          if one or more header nmemonics are not found. It will issue a message and
%          return with "ier" set to 1. If no error exists, "ier" is set to 0.

ier=0;
 
if iscell(varargin{:})
   varargin=varargin{:};
end

index=zeros(length(varargin),1);
for ii=1:length(varargin)
  [idx,ier1]=header_index1(seismic,varargin{ii});
%  idx=find(ismember(seismic.header_info(:,1),lower(char(varargin{ii}))));
  if ier1
    ier=1;
    disp([' Header "',varargin{ii},'" does not exist.'])
  else
    index(ii)=idx;
  end
end

if ier == 1
%  temp=[char(seismic.header_info{:,1})';blanks(size(seismic.header_info,1))]';
  disp(' The following header mnemonics exist: ')
  disp(seismic.header_info(:,1)');
  if nargout < 2
     error(' Abnormal Termination')
  end
end
