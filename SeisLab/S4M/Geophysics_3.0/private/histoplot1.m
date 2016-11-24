function aux=histoplot1(x,edges,varargin)
% Histogram plot (1-D)
%
% Written by: E. Rietsch: September 19, 2003
% Last updated: April 10, 2008: Generalize plotting of percentile lines
%
%        aux=histoplot1(x,edges,varargin)
% INPUT
% x      column vector of samples
% edges  either the vector of edges for "x"
%        or the number of bins (positive integer)
%        or the bin size [negative; bin size, then, is abs(edges)].
%        In the second case the first bin starts at the smallest sample of
%        "x" and the last bin ends at the largest sample of "x";
%        in the third case the first bin starts at the smallest sample of
%        "x" - bin size and the last bin ends at the largest sample 
%        of "x" + bin size
% varargin  one or more cell arrays; the first element of each cell array is a
%        keyword, the other elements are parameters. Presently, keywords are:
%    'xinfo'     three-element cell array; all elements are strings; first is a 
%        mnemonic (for cursor tracking), second units of measurement, 
%        the third string is the x-axis label
%        Default: {'xinfo',{'x','n/a','x'}}
%    'yinfo'     three-element cell array; all elements are strings; first is a 
%        mnemonic (for cursor tracking), second units of measurement, 
%        the third string is the x-axis label
%        Default: {'yinfo',{'counts','n/a','Counts'}}
%    'style'   possible values are: 'line' (line representation of histogram) 
%                               and  'bar'  (bar representation of histogram)
%        Default: {'style','line'}
%    'linewidth'  only used if "style" is 'line'; width of line
%        Default: {'linewidth',3}
%    'percentiles'  percentile locations that should be marked by vertical
%        lines; 1 or 3 percentile locations are supported.
%        No percentile lines if empty.
%        Default: {'percentiles',[10,50,90]}
%     'colors'    line color
%        Default: {'colors','r'}
%     'scale'     scale bars to represent percent
%        Default: {'scale','no'}
%     'width'     specifies the width of the bars. Values  > 1, produce 
%                 overlapped bars (only used if keyword "style" is 'bar').
%                 Default: {'width',1}
% OUTPUT
% aux    structure with fields 'handle' (handle of histogram curve) and 
%                              'edges'  (edges of histogram bins)
%                              'nn'     (histogram values; e.g.  "mystairs(edges,nn)")
%                              'percentile_handles (if percentiles are plotted)
%
% EXAMPLE
%        x=randn(10000,1);
%        lfigure
%        histoplot1(x,11)
%        lfigure
%        histoplot1(x,(-4.25:0.5:4.25),{'style','bar'})

% UPDATE HISTORY
%        August 31, 2007: add keyword 'width', bug fix in bar plot


global S4M

%       Set defaults of input parameters
param.xinfo={'x','n/a','x'};
param.yinfo=[];
param.style='line';
param.linewidth=3;
param.colors='r';
param.percentiles=[10,50,90];
param.scale='no';
param.width=1;

%       Replace defaults bu actual input paramters
param=assign_input(param,varargin);

if length(param.xinfo) == 1
   param.xinfo=param.xinfo{1};
end

if isempty(param.yinfo)
   switch param.scale
      case 'yes'
         param.yinfo={'counts','n/a','%'};
      case 'no'
         param.yinfo={'counts','n/a','Sample count'};
      otherwise
         error(['Unknown scale option: ',param.scale])
   end
else
   if length(param.yinfo) == 1
      param.yinfo=param.yinfo{1};
   end
end
 
       
% userdata=get(gca,'UserData');

if length(edges) == 1
   if edges > 0
      edges=linspace(min(x(:)),max(x(:))*(1+2*eps),edges+1)';
   else
      edges=equal_bins_from_samples(min(x(:)),max(x(:)),-edges);
   end
end

%       Compute histogram
x=x(:);
nn=histc(x,edges);

if strcmpi(param.scale,'yes')
   nn=nn*100/sum(nn);
end


if strcmpi(param.style,'bar')
   hs=bar(centers_from_edges(edges),nn(1:end-1),param.width,param.colors);
   if S4M.matlab_version < 7.2
      %         Get rid of asterisks along the x-axis
      line_handle=findobj(gca,'Type','line');   % Use the "findobj" function to
                                                % get a handle to the line object
      delete(line_handle);                      % Delete the line object
      clear line_handle;                        % Clear the handle 
                                                % reference variable
   end
   lhs=length(hs);
   if lhs > 1
      for ii=1:length(hs)
         set(hs(ii),'FaceColor',get_color(ii))
      end
   else
      set(hs,'FaceColor',param.colors)
   end
   
else
   ntr=size(nn,2);
   if ntr == 1
      hs=mystairs(edges,nn(1:end-1));
      set(hs,'LineWidth',param.linewidth,'Color',param.colors)
     
   else
      for ii=1:ntr
         hs=mystairs(edges,nn(:,ii));
         set(hs,'LineWidth',param.linewidth,'Color',get_color(ii))
         hold on
      end
   end
end

grid on

xlabel(info2label(param.xinfo));
ylabel(info2label(param.yinfo));

bgGray  % Create gray background

%    Implement cursor tracking
initiate_2d_tracking(param.xinfo,param.yinfo)

%       Save handles for output (if requested)
if nargout == 1
   aux.handle=hs;
   aux.edges=edges;
   aux.nn=nn;
end

%       Plot percentile lines (if requested)
if ~isempty(param.percentiles)
   if iscell(param.percentiles)
      param.percentiles=cell2mat(param.percentiles);
   end   
   param.percentiles=sort(param.percentiles);
   loc=percentiles_from_samples1d(x,param.percentiles);
   if strcmp(param.style,'bar')
      handles=plot_percentile_lines_no1(loc,param.colors,4,[]);
      handles1=plot_percentile_lines_no1(loc,'white',1.6,[]);
   else
      handles=plot_percentile_lines_no1(loc,param.colors,2,[]);
   end
   
   if nargout == 1
      aux.percentile_handles=handles;
      if exist('handles1','var')
         aux.percentile_handles1=handles1;
      end
  %    aux.percentile_handles1=handles1;
      aux.percentiles=param.percentiles;
      aux.percentile_locations=loc;
   end        
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function handles=plot_percentile_lines_no1(loc,color,linewidth,linestyles)

nloc=length(loc);
handles=zeros(nloc,1);
if mod(nloc,2) == 1           % odd
   mloc=(nloc+1)/2;
   temp=mygrid(loc(mloc),'v',color,linewidth,get_linestyle(1,linestyles));
   handles(1)=temp.handles;
   for ii=1:mloc-1
      temp=mygrid(loc(mloc+ii),'v',color,linewidth,get_linestyle(ii+1,linestyles));
      handles(mloc+ii)=temp.handles;
      temp=mygrid(loc(mloc-ii),'v',color,linewidth,get_linestyle(ii+1,linestyles)); 
      handles(mloc-ii)=temp.handles;
   end

else                        % even
   for ii=1:nloc
      temp=mygrid(loc(ii),'v',color,linewidth,get_linestyle(ii+1,linestyles)); 
      handles(ii)=temp.handles;
   end    
end

