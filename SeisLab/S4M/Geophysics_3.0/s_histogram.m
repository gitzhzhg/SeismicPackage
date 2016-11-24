function histogram=s_histogram(seismic,varargin)
% Function computes histogram of seismic amplitudes; such histograms can be
% used to check if seismic data were mistakenly red from an SEG-Y file with 
% the wrong data format (IBM format read as IEEE and vice versa).
%
% Written by: E. Rietsch: September 14, 2000
% Last updated: December 16, 2007: reduce and simplify input parameters
%
%         histogram=s_histogram(seismic,varargin)
% INPUT
% seismic       seismic data set
% varargin      one or more cell arrays; the first element of each cell array is a keyword,
%               the other elements are parameters. Presently, keywords are:
%         'abs'       specifies if the absolute value of the traces is to be taken.
%             Default: {'abs','no'}
%         'edges'   either the vector of histogram edges or the number
%             of bins (positive integer)
%             or the bin size [negative number; bin size, then, is abs(edges)].
%             In the second case the first bin starts at the smallest sample of
%             the curve and the last bin ends at the largest sample of the curve;
%             If edges is empty or not given the number of bins is selected by
%             Sturges' rule: edges=1+log2(size(wlog.curves,1)).
%             In the third case the first bin starts at the smallest sample of the
%             curve - bin size and the last bin ends at the largest sample 
%             of the curve + bin size.
%             Default: {'edges',1+log2(size(seismic.traces,1))}%
%         'option'    specifies what to compute. Presently, the following
%             options are available:
%             'tracewise'  compute a histogram for each trace
%             'dataset'    computes one histogram for the whole data set
%             Default: {'option','dataset')
%         'output_type' Describes the type of output. The two options are '%' and
%             'samples'. If the former is chosen, the output represents the 
%             percentage of samples falling into each bin. In the latter
%             case, the output is the number of samples in each bin.
%             Default: {'output_type','%'}
% OUTPUT
% histogram   histogram of of seismic amplitudes.
%             The first bin starts at histogram.first-histogram.step and ends at
%             histogram.first  
%
% EXAMPLE
%            seismic=s_data;
%            histogram=s_histogram(seismic,{'edges',-1:0.0625:1},{'option','tracewise'});
%            s_wplot(histogram,{'deflection',1},{'interpol','linear'})
%            histogram1=s_histogram(seismic,{'edges',-1:0.0625:1});
%            s_wplot(histogram1,{'quality','spikes'})

% UPDATE HISTORY
%            January 18, 2005: add required fields to seismic structure


%       Set defaults
param.abs='no';
param.edges=[];      % Bin edges
param.option='dataset';
param.output_type='%';

%       Replace defaults by actual input arguments
param=assign_input(param,varargin);

if ~istype(seismic,'seismic')
   error(' First input argument must be seismic dataset.')
end

if isyes(param.abs)
   traces=abs(seismic.traces);
else
   traces=seismic.traces;
end

[nsamp,ntr]=size(traces);

switch param.option
case 'dataset'
   nsamples=nsamp*ntr;
case 'tracewise'
   nsamples=nsamp;
otherwise
   error(['Unknown parameter "option": ',param.option])
end

%	Determine edges
if isempty(param.edges)
   param.edges=1+log2(nsamples);
end

if length(param.edges) == 1
   if param.edges > 0
      edges=linspace(min(traces(:)),max(traces(:)),param.edges+1);
   else
      edges=equal_bins_from_samples(min(traces(:)),max(traces(:)),-param.edges);
   end
   equal=true;           % Bin width is constant
else
   edges=param.edges;
   if isconstant(diff(edges),eps*max(abs(edges)));
      equal=true;        % Bin width is constant
   else
      equal=false;       % Bin width is not constant
   end
end

histogram.type='seismic';
histogram.tag='histogram';
histogram.name=['Histogram (',seismic.name,')'];

switch param.option
case 'dataset'
   histogram.traces=histc(traces(:),edges);

case 'tracewise'
   histogram.traces=histc(traces,edges);
   if isfield(seismic,'header_info')
      histogram.header_info=seismic.header_info;
      histogram.headers=seismic.headers;
   end

otherwise
   error(' Unknown value for keyword "option".')
end

if strcmpi(param.output_type,'%')
   histogram.traces=histogram.traces*(100/nsamples);
end

histogram.traces=histogram.traces(1:end-1,:); 	% The last value is generally zero 
     %    or irrelevant since it must fall exactly on the end of the last interval 
     %    (edges(end)) which has earlier been increased by 1+eps.
						% 

if equal
   histogram.first=0.5*(edges(1)+edges(2));
   histogram.last=0.5*(edges(end-1)+edges(end));
   histogram.step=edges(2)-edges(1);
   histogram.units='amplitude';
   histogram.binsize='equal';

else
   histogram.first=1;
   histogram.last=length(edges)-1;
   histogram.step=1;
   histogram.units='bin index';
   histogram.binsize='unequal';

end
   
if any(isnan(histogram.traces(:)))
   histogram.null=NaN;
else
   histogram.null=[];
end

histogram.binedges=edges;

%    Append history field
if isfield(seismic,'history')
   histogram.history=seismic.history;
   htext=param.option;
   histogram=s_history(histogram,'append',htext);
end 


%	Convert dataset to the precision of the seismic input dataset
if strcmp(class(seismic.traces),'single')
   histogram=single(histogram);
end
