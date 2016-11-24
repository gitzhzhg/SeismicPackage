function titleHandle=mysuptitle(titleText,varargin)
% Function plots "supertitle" above several subplots. 
% To create room above the plots the vertical dimensions of all plots are
% reduced in size. Hence, it should be alled after all subplots have been 
% created.
%
% Written by: E. Rietsch: July 16, 2009
% Last updated: 
%
% INPUT
% titleText   string or cell array with title
% varargin    one or more cell arrays; the first element of each cell array 
%             is a keyword, the other elements are parameters. 
%             Presently, keywords are:
%     'color'   Color of title text;
%             Default: {'color','red'}
%     'factor'   Factor by which the vertical dimensions of the subplots are 
%             reduced. If the supertitle interfers with the plot annotation 
%             (in particular if the x-label annotation is along the top, 
%             the factor should be reduced.
%             Default: {'factor','0.9}
%     'fontsize'  Size of the title font
%             Default: {'fontsize',14} (actually, get(gcf,'defaultaxesfontsize')+4)
%     'titleypos'  y-location of the super title
%             Default: {'titleypos',0.98}
% OUTPUT
% titleHandle  handle to the super title (allows subsequent changes of the title
%              and its properties)
%
% EXAMPLE
%      lfigure
%      subplot(2,2,1);ylabel('ylabel1');title('title1')
%      subplot(2,2,2);ylabel('ylabel2');title('title2')
%      subplot(2,2,3);ylabel('ylabel3');xlabel('xlabel3')
%      subplot(2,2,4);ylabel('ylabel4');xlabel('xlabel4')
%
%      title_handle=mysuptitle(mnem2tex('Super Title for dataset x_y_z'));
%      set(title_handle,'FontSize',10,'Color','blue')


%     Set defaults for input parameters
param.color='red';
param.factor=0.9;
param.fontsize=get(gcf,'defaultaxesfontsize')+4;
param.titleypos=0.98;
param.yloc=[];                 % Deprecated. Alert issued if used.

%     Replace defaults by input arguments
param=assign_input(param,varargin);

%     Handle legacy use
if ~isempty(param.yloc)
   alert('Keyword ''yloc'' has been deprecated; use keyword ''factor'' instead.')
end

%     Find all the axes of the figure
axesHandles = findobj(gcf,'Type','axes');

%     Reduce the position data of these axes by factor "param.factor"
for ii=1:length(axesHandles)
   hh=axesHandles(ii);
   pos=get(hh,'Position');
   pos([2,4])=pos([2,4])*param.factor;
   set(hh,'Position',pos);
end

%     Save present axis
oldaxis=gca;

%     Create new axis to plot supertitle
axes('pos',[0 1 1 1],'visible','off','Tag','suptitle');
titleHandle=text(.5,param.titleypos-1,titleText);
set(titleHandle,'HorizontalAlignment','center','fontsize',param.fontsize,'Color',param.color);

%     Restore old axis
axes(oldaxis)

%     Remove title handle if there is no output argument
if nargout == 0
   clear titleHandle
end
