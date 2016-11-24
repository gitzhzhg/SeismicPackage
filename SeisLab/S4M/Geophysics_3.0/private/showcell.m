function showcell(varargin)
%SHOWCELL   Displays cell array with long strings in the command window.
%   SHOWCELL(A) displays the contents of a cell array A in the command
%   window.  It will format the display so that long strings will display
%   appropriately.  A can be a cell array of numbers, strings, and/or other
%   objects.
%
%   Typically, if a cell array contains long strings, it will not display
%   the text:
%
%   >> A
% 
%   A = 
% 
%       [3]    'this is a text.'    'hello'
%       [4]    'More text'          [   32]
%       [6]          [1x54 char]    [   53]
% 
%   SHOWCELL will display it properly:
%
%   >> showcell(A)
%     [ 3]    'this is a text.'                                           'hello'
%     [ 4]    'More text'                                                 [32]
%     [ 6]    'This is a very long text that may not show up properly'    [53]
%
%   Acceptable numbers are of class DOUBLE, SINGLE, LOGICAL, UINT8, UINT16,
%   UINT32, UINT64, INT8, INT16, INT32, INT64. Elements other than CHAR or
%   numbers are displayed as the size and name of the object,
%     e.g. [1x1 struct]
%
%   SHOWCELL(A,'option1',value1,...) specifies optional arguments passed
%   in in pairs.  Valid options are (abbreviated names accepted):
%
%      'spacing'   - column spacing.  Default is 4 spaces.
%      'numformat' - number of digits OR format string (see SPRINTF) for
%                    numerical values.  Default is 5 digits.
%
%   Example:
%       showcell(A, 'spacing', 5);
%       showcell(A, 'numformat', 3);
%       showcell(A, 'n', '%0.4f');
%       showcell(A, 'sp', 2, 'nu', 6);
%
%   See also DISP, DISPLAY
%
%
%   VERSIONS:
%     v1.0 - first version
%     v1.1 - add quotes around strings (Jan 2006)
%     v1.2 - accepts uint8, uint16, uint32, uint64, int8, int16, int32,
%             int64, single, double, logical for numeric values.
%     v2.0 - each column does not have to be of the same class. the cell
%             elements can be of any class. (Jan 2006)
%     v2.1 - fixed problems with displaying empty cell elements. (Jan 2006)
%     v2.2 - fixed displaying an empty cell {}. Remove MORE function, since
%             this can be achieved externally by calling MORE. (Jan 2006)
%     v2.3 - now displays multi-dimension cells (Feb 10, 2006)
%

%   Jiro Doke
%   June 2004

%-----------------------------------------------------------------------
% Check cell array                  
%-----------------------------------------------------------------------
if ~nargin              
   return;              
end                     
                        
arg = varargin{1};                  
                        
if ~iscell(arg)                     
   error('This is not a cell array.');          
end                     
                        
%-----------------------------------------------------------------------
% Parse optional arguments          
%-----------------------------------------------------------------------
                        
% Default values                    
num_spaces = 4;                     
num_digits = 5;                     
                        
% Possible optional arguments                   
optSpacing   = 'spacing  ';         
optNumformat = 'numformat';         
                        
if nargin > 1                       
   vars = varargin(2 : end);                    
                        
   if mod(length(vars) , 2)         
     error('The optional arguments must come in pairs.');   
   end                  
                        
  for id = 1 : 2 : length(vars)                 
                        
    % Get number of characters provided for optional arguments          
    % Accepts abbreviated option names          
     varC = min([length(vars{id}), 9]);         
                        
     switch lower(vars{id})         
        case optSpacing(1 : varC)     % SPACING             
           if isnumeric(vars{id + 1})           
              num_spaces = round(vars{id + 1});             
           else                     
              error('Bad value for SPACING. Must be an integer');       
           end                      
                        
      case optNumformat(1 : varC)   % NUMFORMAT             
         if isnumeric(vars{id + 1})             
            num_digits = round(vars{id + 1});               
         else                       
            num_digits = vars{id + 1};          
         end            
                        
      otherwise                     
        error('Unknown option.');               
      end               
   end                  
end                     
                        
%-----------------------------------------------------------------------
% Deal with multi-dimension cells               
%-----------------------------------------------------------------------
                        
isLoose = isequal(get(0,'FormatSpacing'),'loose');          
                        
if ndims(arg) > 2                   
   sz = size(arg);                  
   id = cell(ndims(arg) - 2, 1);                
else                    
   sz = [0 0 1];                    
end                     
                        
for ii = 1:prod(sz(3:end))          
   if exist('id', 'var')            
      [id{:}]  = ind2sub(sz(3:end), ii);      %#ok  
      str      = ['(:,:', sprintf(',%d', id{:}), ')'];      
      this_arg = arg(:, :, id{:});              
   else                 
      this_arg = arg;               
      str      = '';                
   end                  
   if ~isempty(inputname(1))                    
      if isLoose                    
         disp(' ');                 
         fprintf('%s%s =\n', inputname(1), str);            
         disp(' ');                 
      else              
         fprintf('%s%s =\n', inputname(1), str);            
      end               
   end                  
                        
   if isequal(size(this_arg), [0 0])            
      disp('     {}');              
      if isLoose
         disp(' ');
      end         %
   elseif ismember(0, size(this_arg))           
      fprintf('   Empty cell array: %d-by-%d\n', size(this_arg));       
       if isLoose
          disp(' ')
       end         %
   else                 
       showcellEngine(this_arg, num_spaces, num_digits);    
   end                  
end                     
                        
                        
%-----------------------------------------------------------------------
%-----------------------------------------------------------------------
% showcellEngine                    
%-----------------------------------------------------------------------
%-----------------------------------------------------------------------

function showcellEngine(arg, num_spaces, num_digits)
                        
%-----------------------------------------------------------------------
% Determine class of cell elements              
%-----------------------------------------------------------------------
                        
cellArg = arg(:);                   
                        
isNumChar        = false(length(cellArg), 12);              
isNumChar(:, 1)  = cellfun('isclass', cellArg, 'char'   );              
isNumChar(:, 2)  = cellfun('isclass', cellArg, 'double' );              
isNumChar(:, 3)  = cellfun('isclass', cellArg, 'single' );              
isNumChar(:, 4)  = cellfun('isclass', cellArg, 'uint8'  );              
isNumChar(:, 5)  = cellfun('isclass', cellArg, 'uint16' );              
isNumChar(:, 6)  = cellfun('isclass', cellArg, 'uint32' );              
isNumChar(:, 7)  = cellfun('isclass', cellArg, 'uint64' );              
isNumChar(:, 8)  = cellfun('isclass', cellArg, 'int8'   );              
isNumChar(:, 9)  = cellfun('isclass', cellArg, 'int16'  );              
isNumChar(:, 10) = cellfun('isclass', cellArg, 'int32'  );              
isNumChar(:, 11) = cellfun('isclass', cellArg, 'int64'  );              
isNumChar(:, 12) = cellfun('isclass', cellArg, 'logical');              
                        
% Number of elements in cell element            
numElmt             = cellfun('prodofsize', cellArg);       
                        
% Remove number cells with vectors (more than a scalar)     
isNumChar(:, 2:end) = isNumChar(:, 2:end) & repmat(numElmt <= 1, 1, 11);
                        
% Number elements                   
isNum   = ~~sum(isNumChar(:, 2:end), 2);        
                        
% Cell elements                     
cellElements        = cellfun('isclass', cellArg, 'cell');              
                        
% Empty elements                    
emptyElements       = cellfun('isempty', cellArg);          
emptyCells          = emptyElements & cellElements;         
emptyNums           = emptyElements & isNum;                
                        
%  All other objects (including objects with more than one element)      
isObj   = xor(emptyCells, ~sum(isNumChar, 2));              
                        
%  Discard empty number elements. These will be processed separately.    
isNumChar(isNumChar & repmat(emptyNums, 1, size(isNumChar, 2))) = false;
                        
%-----------------------------------------------------------------------
%  Deal with empty elements          
%-----------------------------------------------------------------------
if any(emptyCells)                  
  cellArg(emptyCells) = {'{}'};                 
end                     
                        
if any(emptyNums)                   
  cellArg(emptyNums)  = {'[]'};                 
end                     
                        
%-----------------------------------------------------------------------
%  Deal with numeric elements                    
%-----------------------------------------------------------------------
                        
numID = logical(sum(isNumChar(:, 2:end), 2));               
if ~isempty(find(numID,1))            
                        
  TOdouble = repmat(NaN, length(cellArg), 1);               
                        
  % Convert the numeric/logical values to double            
  useIDX = find(sum(isNumChar(:, 2:end)));      
  % Only parse through valid types              
  for iType = useIDX + 1            
    TOdouble(isNumChar(:, iType), 1) = ...      
      double([cellArg{isNumChar(:, iType)}]');              
  end                   
                        
  TOdouble(~numID) = [];            
  % Convert DOUBLE to strings and put brackets around them              
  try                   
    tmp = strcat({'['}, num2str(TOdouble, num_digits), {']'});          
  catch % ME 
%    getReport(ME)
    error('Abnormal termination')              
  end                   
  cellArg(numID) = tmp;             
end                     
                        
%-----------------------------------------------------------------------
% Deal with string elements         
%-----------------------------------------------------------------------
% Put single quotes around the strings          
stringCell = strcat({''''}, cellArg(isNumChar(:, 1)), {''''});          
cellArg(isNumChar(:, 1)) = stringCell;          
                        
%-----------------------------------------------------------------------
% Deal with elements other than string or numeric           
%-----------------------------------------------------------------------
objID = find(isObj);                
objCell = cell(length(objID), 1);               
for iObj = 1:length(objID)          
   sz = size(cellArg{objID(iObj)});              
   cl = class(cellArg{objID(iObj)});             
  % Display size and class type, wrapped by brackets        
   switch cl             
   case 'cell'                     
      if length(sz) < 4             
        objCell{iObj} = ['{', sprintf('%dx', sz(1:end-1)), ...          
            num2str(sz(end)), sprintf(' %s}', cl)];         
      else              
        objCell{iObj} = sprintf('{%d-D %s}', length(sz), cl);           
      end               
   otherwise                       
      if length(sz) < 4             
        objCell{iObj} = ['[', sprintf('%dx', sz(1:end-1)), ...          
            num2str(sz(end)), sprintf(' %s]', cl)];         
      else              
        objCell{iObj} = sprintf('[%d-D %s]', length(sz), cl);           
      end               
   end                   
end                     
cellArg(isObj) = objCell;           
                        
% Reconstruct the original size                 
arg = reshape(cellArg, size(arg));              
                        
%-----------------------------------------------------------------------
% Create FPRINTF format string based on length of strings   
%--------------------------------------------------------------------------
char_len = cellfun('length', arg);                %
if 0  % Change this to 1 in order to right justify numeric elements.
      % This will be slightly slower.       
   conv_str = '    ';            
   for iCol = 1:size(arg, 2);    
      if length(unique(char_len(:, iCol))) == 1           
         conv_str = [conv_str, ...             
             sprintf('%%-%ds%s', unique(char_len(:, iCol)), ...        
             blanks(num_spaces))];    %#ok Don't bother ...         
      else            
         tmp = char(arg(:, iCol));             
         idx1 = strfind(tmp(:, 1)', '[');      
         idx2 = strfind(tmp(:, 1)', '{');      
         tmp([idx1 idx2], :) = strjust(tmp([idx1 idx2], :), 'right');  
         arg(:, iCol) = cellstr(tmp);          
         conv_str = [conv_str, ...             
              sprintf('%%-%ds%s', max(char_len(:, iCol)), ... 
              blanks(num_spaces))];  %#ok Don't bother ...               
      end             
   end               
else                
  % Create array of max character lengths and blank pads
   char_max = [num2cell(max(char_len, [], 1)); ...       
      repmat({blanks(num_spaces)}, 1, size(char_len, 2))]; 
   conv_str = ['    ', sprintf('%%-%ds%s', char_max{:})];
end                 
                    
% Add carrige return at the end             
conv_str = [conv_str(1 : end - num_spaces) '\n'];       
                    
%--------------------------------------------------------------------------
% Display in command window     
%--------------------------------------------------------------------------
                    
% Must transpose for FPRINTF to work        
arg = arg';         
                    
% If arg is a single EMPTY cell/string/numeric element, 
% then wrap it with {}          
if length(arg) == 1             
   switch arg{1}                 
   case {'{}', '''''', '[]'}   
      conv_str = '    {%s}\n';              
   end               
end                 
                    
try                 
  % Wrap around TRY ... END in case the user quits out of MORE      
   fprintf(1, conv_str, arg{:});             
   if isequal(get(0,'FormatSpacing'),'loose')
      disp(' ');      
   end 
catch
   % Do nothing
end                 
                    
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------