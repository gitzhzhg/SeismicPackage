function title_handle=mytitle(string,varargin)
% Function creates a "standard" title
%
% Written by: E. Rietsch: April 5, 2001
% Last updated: November 15, 2007: Better computation of font size
%
%           mytitle(string,varargin)
% INPUT
% string    title string
% varargin  one or more cell arrays; the first element of each cell array is a
%           keyword, the other elements are parameters. Presently, keywords are:
%      'fig'      figure handle. 
%                 Default: {'fig',gcf}   i.e. current figure
%      'color'    Title color. Any of the Matlab colors or their abbreviations
%                 Default {'color','r'}
%      'fontsize' Font size of title text. 
%                 Default: {'fontsize',ftsze}
%                 where ftsze=min(15,2.25*pos(3)/lstring), pos(3) is the length 
%                           of the x-axis in points, and "lstring" title length 
%      'fontname' Font name. 
%                 Default: {'fontname',S4M.font_name}
%      'tex'      Possible values are 'yes' and 'no'; 'yes' means that 
%                 underscores (_) are treated as LaTeX symbols; 'no' means they are not
%                 Default: {'tex','no'}
%

% UPDATE HISTORY
%                 March 1, 2006: Add "tex" option


global S4M

% 	Set default parameters
param.fig=gcf;
param.color='r';
param.fontsize=[];
param.fontname=S4M.font_name;
param.tex='no';

%	Replace defaults by actual input arguments
param=assign_input(param,varargin);

figure(param.fig);
axis_handle=gca;

if isempty(param.fontsize)      % Compute font size since it is not specified
   if iscell(string)            % Header may have more than one line
   %      Find longest line
      lstring=0;
      for ii=1:length(string)
         lstring=max([lstring,length(string{ii})]);
      end
      pos=get(axis_handle,'Position');
      pos(2)=0.95*pos(2);
      pos(4)=0.91*pos(4);
      set(axis_handle,'Position',pos); 
   else
      lstring=length(string);
   end

   %    Compute width of axis in points
   units=get(axis_handle,'Units');
   set(axis_handle,'Units','points');
   pos=get(axis_handle,'Position');
   set(axis_handle,'Units',units);   % Restore original axis units

   param.fontsize=min(15,2.25*pos(3)/lstring);
end


if ~isyes(param.tex)
   string=mnem2tex(string);
end

title_handle=title(string,'Color',param.color,'FontSize',param.fontsize,'FontName',param.fontname);

if nargout == 0
   clear title_handle
end
