function avgrangecb(action)
%
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

 
if( nargin < 1 )
   action = 'init';
end
 
[shotrange shotINClist shot axeslist stddev] = avgcvpinfo('get');
 
if( strcmp(action,'init'))
   nshots = refdata('get', 'nshots');
   q = str2mat('First shot number:', 'Last shot number:');
   if(~isempty(shotrange))
      rangeend = max(shotrange);
      if( rangeend > nshots )
         rangeend = nshots;
      end
      a = str2mat( num2str(shotrange(1)), num2str(rangeend) );
   else
      a = str2mat( '1', sprintf('%d',nshots));
   end
   askthingsinit('avgrangecb(''answ'')',q,a,[1 1],'Shot range');
elseif( strcmp(action, 'answ') )
   a = askthingsfini;
   % a is -1 if 'cancel' was pressed
   if( a ~= -1 )
      [strings tmp] = size(a);
 
      shotrange = str2num(a(1,:)) : str2num(a(2,:));
      shot = shotrange(1);
   
      if(isempty(shotINClist))
         nshots = refdata('get', 'nshots');
         shotINClist = 1:nshots;
      end
      avgcvpinfo('set', shotrange, shotINClist, shot, axeslist, stddev);
      % Turn on the buttons that can now work.
      c = get(gcf, 'children');
      [nc tmp] = size(c);
      for i=1:nc
         if( strcmp(get(c(i),'type'),'uicontrol') )
            if( strcmp(get(c(i),'string'), 'Next shot') | ...
               strcmp(get(c(i),'string'), 'Previous shot') | ...
               strcmp(get(c(i),'string'), 'Recompute') )
               set(c(i), 'visible', 'on');
            end
         end
      end
      editcvpavg('label');
   end
end
  