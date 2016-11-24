function [colors,linestyles]=split_colors_linestyles(colors_linestyles)
% Split color/line-style combinations into cell vectors of colors and linestyles.
%
% Written by: E. Rietsch: September 19, 2007
% Last updated:
%
%             [colors,linestyles]=split_colors_linestyles(colors_linestyles)
% INPUT
% colors_linestyles   cell vector of color/line-style combinations; e.g. {'r-','g:','b--'}
% OUTPUT
% colors      cell vector with colors only; e.g {'r','g','b'}
% linestyles  cell vector with linestyles only; e.g {'-',':','--'}
%
% EXAMPLE
%             cl={'r-','g:','b--'}
%             [c,l]=split_colors_linestyles(cl)


lcolors=length(colors_linestyles);

colors=cell(1,lcolors);
linestyles=cell(1,lcolors);
for ii=1:lcolors
   colors{ii}=colors_linestyles{ii}(1:1);
   linestyles{ii}=colors_linestyles{ii}(2:end);
end

%	Handle the case that no linestyle has been specified (default to solid line)
linestyles(cellfun(@isempty,linestyles))={'-'};
