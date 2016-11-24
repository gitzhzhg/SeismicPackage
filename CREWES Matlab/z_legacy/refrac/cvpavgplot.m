function cvpavgplot( graph, axishandle, xleft, yleft, xright, yright, xlimits )
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE

axes(axishandle);
cla;
hold off;
if( length(yleft) > 0 )
   plot( xleft, yleft, 'y');
   hold on;
end
if( length(yright) > 0 )
   plot( xright, yright, 'c');
   hold on;
end
set( axishandle, 'xlim', xlimits );
hold off;
if( strcmp(graph, 'td') )            % time difference curves 
   string = 'Time difference (TD)';
   ylabel('Time (ms)');
end
if( strcmp(graph, 'median') )        % Plot the median filter
   set(axishandle, 'xtick', [] );
   string = 'Median filtered TD';
   ylabel('Time (ms)');
end
if( strcmp(graph, '1std') )          % Plot the 1st derivative
   set(axishandle, 'xtick', [] );
   string = '1st derivative of filtered TD';
end
if( strcmp(graph, '2ndd') )           % Plot the 2nd derivative
   set(axishandle,'xtick',[]);
   string = '2nd derivative of filtered TD';
end
text('units', 'normalized', 'position', [0.3 0.88], 'string', string);