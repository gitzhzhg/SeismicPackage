function show(var)
% Function displays numeric variables, structures, and cell arrays in a
% more general form than provided by Matlab and uses "disp" for all other
% variables
% 
% Written by: E. Rietsch: January 22, 2006
% Last updated: March 6, 2006: cosmetics
%
%       show(var)
% INPUT
% var   variable to display in the command window

disp(inputname(1))

if all(isnumeric(var))  &&  ~all(ishandle(var))
   show_no1(var)

elseif isstruct(var)
   disp(orderfields(var))

elseif iscell(var)
   var1.var=var;        % This fancy footwork is designed to prevent the 
                        % name "var" to show up in the list
   showcell(var1(1).var)

elseif isobject(var)
   display(var)

else
   disp(var)
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function []=show_no1(array)
%   Function displays 1-D array
%         show_no1(array)

% n=length(array);
% scale=10^ceil(log10(max(abs(array))));
scale=1;

fprintf('%7.5g %7.5g %7.5g %7.5g %7.5g %7.5g %7.5g %7.5g %7.5g %7.5g\n',array/scale)
if rem(length(array),10) ~= 0, 
   fprintf('\n'); 
end
