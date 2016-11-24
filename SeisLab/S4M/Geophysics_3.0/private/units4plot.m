function units=units4plot(units)
% Replace "n/a" by zero length string

if strcmpi(units,'n/a')
   units='';
end