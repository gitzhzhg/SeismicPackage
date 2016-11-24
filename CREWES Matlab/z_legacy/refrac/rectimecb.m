function rectimecb(action)
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

% Determine the parameter for the reciprocal time difference function
if( nargin < 1 )
   action = 'init';
end
if( strcmp(action,'init'))
   q=str2mat('Reciprocal time check on which shot pairs ?',...
             'Enter the shot pair if "all" is not selected:',...
             'Identify the shot pairs having a reciprocal time check difference over (ms)?');
   a=str2mat('all|shot pair (i,j)','10 15','5');
   askthingsinit('rectimecb(''answer'')',q,a,[1 0 1],'Parameter for the Reciprocal time check');
elseif( strcmp(action,'answer'))
   a=askthingsfini;
   [strings tmp] = size(a);
   if( strcmp(deblank(a(1,:)), 'all') )
      rtrange = 0;
   else
      rtrange = 1;
   end
   refdata('set','rtrange',rtrange);
   disprectimem = refdata('get','disprectimem');
   set(disprectimem,'enable','on');
   rtpair = sscanf(a(2,:), '%d %d')
   rtpair1 = rtpair(1);
   rtpair2 = rtpair(2);
   refdata('set', 'rtpair1', rtpair1 );
   refdata('set', 'rtpair2', rtpair2 );
   mint = str2num(a(3,:));
   refdata('set', 'mint', mint);
   fbcoord = refdata('get','fbcoord');
   shotcoord = refdata('get','shotcoord');
   fbtime = refdata('get','fbtime');
   nshots = refdata('get','nshots');
   % Call the reciprocal time difference function
   [diffmat]=rectime(rtrange,rtpair1,rtpair2,fbcoord,shotcoord,fbtime,mint,nshots);
 
   % Only save the diffmatrix if all shot pairs have been computed
   if( rtrange == 0 )
      refdata('set','diffmat',diffmat);
   end
end