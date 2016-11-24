function [idx,ok]=mylistdlg(strings,varargin)
% Open a list for interactive selection of items; based on Matlab function
% "listdlg"
%
% Written by: E. Rietsch: August 4, 2003
% Last updated: February 10, 2004: change dimensions of dialog box
%
%           [idx,ok]=mylistdlg(strings,varargin)
% INPUT
% strings   cell vector of strings with the items of the list
% varargin  one or more cell arrays; the first element of each cell array is a
%           keyword, the other elements are parameters. Presently, keywords are:
%           'promptstring' string matrix or cell array of strings which appears 
%                 as text above the list box.
%                 Default: {promptstring','Select one or more curves:'}
%           'selectionmode'  string; can be 'single' or 'multiple';
%                 Default: {'selectionmode','multiple'}
%           'minmax' minimum and maximum number of items to select; ignore when
%                 'selectionmode' is 'single'.
%                 Default: {'minmax',1,length(strings)]}
%           'name'  String for the figure's title.
%                 Default: {'name',S4M.name}
%           'initialvalue'  vector of indices of which items of the list box
%                 are initially selected;
%                 Default: {'initialvalue',1} unless the previous call to 
%                      "mylistdlg" used the same cell vector "strings";
%                      in that case the previously chosen selection is used.
%           'previous'  string or cell array with the names of functions
%                 to be checked for preset values
%                 Default: {'previous',[]}   no presets (or rather the first
%                          item on the list)
%                 Example: {'previous','l_plot','l_plot1'}
% OUTPUT
% idx       row index vector; the list entries chosen are "strings(idx)". This 
%           will be [] when OK is 0.
% ok        "ok" is 1 if you push the OK button, or 0 if you push the Cancel 
%           button or close the figure.

global S4M
global S4M_LISTDIALOG   % Global structure used to save selection of items
                        % S4M_LISTDIALOG is a structure matrix with 3 fields:
                        % 'function','index','strings' to store the name of
                        % the function to which it relates, the strings selected
                        % and their positions in the list

%       Set defaults for input arguments
param.initialvalue=[];
param.minmax=[1,length(strings)];
param.name=S4M.name;
param.previous=[];
param.promptstring={'Select one or more curves:'};
param.selectionmode='multiple';
param.listsize='adapt';
 
%       Decode and assign input arguments
param=assign_input(param,varargin);

try
param.promptstring=param.promptstring{1};
catch
  % keyboard
end
promptstring=param.promptstring;


if isempty(param.listsize)
   param.listsize=[160,300];

elseif strcmp(param.listsize,'adapt')
   if ischar(strings)
      [m,n]=size(strings);
   else
      n=max(cellfun('length',strings));
      m=length(strings);
   end
   if ischar(promptstring)
      nn=length(promptstring);
   else
      nn=max(cellfun('length',promptstring));
   end      
   n=max([n,nn,12]);
   m=min(m,20);
   param.listsize=[7*n+5,max(20*m+10,50)];
end

%lprompt=length(promptstring)

if iscell(param.minmax)
   param.minmax=[param.minmax{1},param.minmax{2}];
end

if ~isempty(param.previous)
   if ~iscell(param.previous)
      param.previous={param.previous};
   end
end

if isempty(param.initialvalue)
   initialvalue=mylistdlg_no1(strings,param.previous);   %#ok Used in "eval".
else
   initialvalue=param.initialvalue;                      %#ok Used in "eval".
end


if iscell(param.minmax)
   param.minmax=[param.minmax{1},param.minmax{2}];
end

if ~isempty(param.previous)
   if ~iscell(param.previous)
      param.previous={param.previous};
   end
end

if isempty(param.initialvalue)
   initialvalue=mylistdlg_no1(strings,param.previous);
else
   initialvalue=param.initialvalue;                   %#ok Used in "eval".
end

ierr=true;

while(ierr)
   [idx,ok] = listdlg('PromptString',promptstring,...
                 'SelectionMode',param.selectionmode,...
                 'Name',param.name,...
                 'InitialValue',initialvalue, ...
                 'ListString',strings, ...
                 'ListSize',param.listsize);

   if ~ok
      return
   end
   if strcmpi(param.selectionmode,'single') || ...
             (length(idx) >= param.minmax(1) && length(idx) <= param.minmax(2))
      ierr=false;
   else
      uiwait(msgbox(['You must select ',num2str(param.minmax(1)),' to ', ...
         num2str(param.minmax(2)),' items from the list'],param.name,'warn'));
   end
end

%       Save selected strings and their location in the cell vector "strings"
%       in global structure vector "S4M_LISTDIALOG"
if ~isempty(param.previous)
   nlist=length(S4M_LISTDIALOG);
   old=cell(nlist,1);
   for ii=1:nlist
      old{ii}=S4M_LISTDIALOG(ii).function;
   end
   idx1=find(ismember(old,param.previous{1}));
   if idx1 > 0
      S4M_LISTDIALOG(idx1).indices=idx;
      S4M_LISTDIALOG(idx1).strings=strings(idx);
   else
      S4M_LISTDIALOG(nlist+1).function=param.previous{1};
      S4M_LISTDIALOG(nlist+1).indices=idx;
      S4M_LISTDIALOG(nlist+1).strings=strings(idx);
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function initialvalue=mylistdlg_no1(strings,previous)
% Extract initial selection of list items from a previous use
%
% INPUT
% strings   strings in the current list
% previous  cell array with relevant function names in the order of their relevance
% OUTPUT
% initialvalue  index vector; "strings(initialvalue)" are preselected

global S4M_LISTDIALOG   % Global structure used to save selection of items

initialvalue=1;         % Default initial value

nlist=length(S4M_LISTDIALOG);
if nlist > 0
   if iscell(previous)
      nprevious=length(previous);
   else
      nprevious=length(previous);
      if nprevious > 0
         nprevious=1;
         previous={previous};
      end     
   end

   old=cell(nlist,1);
   for ii=1:nlist
      old{ii}=S4M_LISTDIALOG(ii).function;
   end
   
   for ii=1:nprevious
      idx=find(ismember(old,previous{ii}));
      if idx > 0
         oldindex=S4M_LISTDIALOG(idx).indices;
         oldstrings=S4M_LISTDIALOG(idx).strings;
         if length(strings) >= max(oldindex)
            idx1=find(ismember(strings(oldindex),oldstrings));
            if length(idx1) == length(oldindex)
               initialvalue=oldindex;
               break
            end
         end
      end
   end
end
