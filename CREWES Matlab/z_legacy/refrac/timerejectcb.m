function timerejectcb(action)
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

if( nargin < 1)
   action = 'init';
end
if( strcmp(action,'init'))
   q = str2mat('Autorejection based on constant or standard devation limit?',...
               'Enter the limit:');
   a = str2mat('Standard deviation|Constant limit','1');
   askthingsinit('timerejectcb(''answer'')',q,a,[1 1],'Autoreject time parameters');
elseif( strcmp(action, 'answer'))
   a=askthingsfini;
   [strings tmp] = size(a);
   dev = str2num(a(2,:));
   refdata('set', 'dev', dev);
   if(strcmp( deblank(a(1,:)),'standard deviation'))
	standard=1;
   else
	standard=0;
   end
   refdata('set','standard',standard);
   fbcoord = refdata('get', 'fbcoord');
   td1 = refdata('get', 'td1');
   plustreject = 1;     % True, do the rejection
   plust = avgplustime(fbcoord, td1, standard, plustreject, dev);
   refdata('set','plust', plust);
end