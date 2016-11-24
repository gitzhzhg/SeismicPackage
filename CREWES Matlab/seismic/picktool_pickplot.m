function hhors=picktool_pickplot(hparent,hornames,horcolors,horpicks,linewidths)
% PICKPLOT: Plot picks on top of a seismic image plot (used by PICKTOOL)
% 
% Given a figure window with a seismic gather displayed as an image plot,
% plot a set of horizon picks on top of the seismic. 
%
% hparent ... handle of figure window or axes containing the seismic image display
% hornames ... cell array of horizon names of existing picks
% horcolors ... cell array of RGB color vectors used to display the horizon picks
% NOTE: hornames and norcolors must be cell arrays of exactly the same size
% horpicks ... cell array of picks for each horizon. Each entry in horpicks
%       must be a npick-by-3 matrix where npicks is the number of picks
%       currently existing on the horizon. Column 1 is the pick times,
%       column 2 is the pick x coordinates, column 3 is the pick y
%       coordinates. The y coordinates may be all zero (or omitted) if the
%       data is 2D. The pick times may be nans or actual times.
%       Picks that fall outside the spatial cooordinate range of the
%       seismic image will be discarded.
% linewidths ... vector of linewidths for each horizon. This is optional.
%       If not provided, lines will have the standard width (0.5).
%
%
% hhors ... handles of the plotted pick lines (not a cell array)
% 
% 

%now plot
hhors=zeros(1,nhors);
for k=1:nhors
    picks=horpicks{k};
    hhors(k)=line(picks(:,2),picks(:,1),'color',horcolors{k},'linewidth',linewidths(k),...
        'tag',hornames{k});
end

legend(hhors,hornames)
