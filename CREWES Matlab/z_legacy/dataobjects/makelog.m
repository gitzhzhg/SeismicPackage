function wellog=makelog(name,data,flag,metric,logtype)

% wellog=makelog(name,data,flag,metric)
%
% Make a random Earth Object containing a well log.
% The storage conventions used here are documented in logobj.txt
%
%	name =  string naming the log
%	data = m rows by 2 columns. First column is either the depth or
%		time coordinate of a log sample and the second column is
%		the actual log samples.
%	flag = 0 ... its in depth
%	       1 ... its in time
%	metric = 0 ... the log is in imperial units
%	metric = 1 the log is in metric units
%	logtype = integer flag giving the log type as documented in
%		LAS2LOGTYPE or logobj.txt
%		default is -1 (unknown log type)
%
% by G.F. Margrave
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

if(nargin<5)
	logtype=-1;
end

wellog=objrand(name,'samples',data(:,2));
if(flag)
	wellog=objset(wellog,'time',data(:,1));
	wellog=objset(wellog,'datatype','tlog');
else
	wellog=objset(wellog,'depth',data(:,1));
	wellog=objset(wellog,'datatype','zlog');
end

%set the metric flag
wellog=objset(wellog,'dely',metric);

%set logtype
wellog=objset(wellog,'xnot',logtype);