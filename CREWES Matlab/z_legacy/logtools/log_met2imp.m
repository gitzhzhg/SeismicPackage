function logout=log_met2imp(login,logtype)
% logout=log_met2imp(login,logtype)
%
% LOG_MET2IMP converts a wellog from metric to imperial units.
% The intent is to recognize all log types and convert their units
% correctly. If you feel this is not being properly done, please
% contact the programer/author listed below.
%
%	login ... input log object
%	May be an earth object as described in logobj.txt or a 2 column matrix
%	where the first column is depth and the second the log samples
% 	logtype ... one of the following strings: 'sonic' 'density'
%       or a numeric code as described in logobj.txt. The best way is to use
%	the numeric code because that allows the specification of lots of
%	different log types. The two ttext strings are maintained as a 
%	nod to the past.
%
%	logout ... output log object or 2 column matrix. Will be the same
%		kind of object as was input
%
% G.F. Margrave, June 1994
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
logout=login;
if(~isstr(logtype))
	if(logtype==0) logtype='sonic';
	elseif( logtype==1 | logtype==2 | logtype==3) logtype='density';
	elseif( logtype==6 )
		logtype='caliper';
	elseif( logtype==7 )%s and p sonics are the same for unit changes
		logtype='sonic';
	elseif( logtype==13 | logtype==14 |logtype==15 )
		logtype='resistivity';
	else
		logtype='whatever';
	end
end
objin=0;
if( isearthobj(login) )
	objin=1;
	%get the metric flag
	flag=objget(login,'dely');
	if(flag==0)
		error(' Log is already imperial');
	end
	logout=objset(logout,'dely',1);
	% get the samples
	samps=objget(login,'samples');
	z=objget(login,'depth');
	t=objget(login,'time');
else
	z=login(:,1);
	samps=login(:,2);
end
if(~isempty(z))
		z=z/(.3048);
end
if( strcmp(lower(logtype),'sonic') )
	samps=samps/3.280840;
elseif( strcmp(lower(logtype),'density'))
	samps=samps/1000;
elseif( strcmp(logtype,'caliper') )
	samps=samps/25.4;
elseif( strcmp(logtype,'resistivity') )
	samps=samps/.3048;
end
if( objin )
		logout=objset(logout,'samples',samps);
		if(~isempty(z))
			logout=objset(logout,'depth',z);
		end
	else
		logout=[z(:) samps(:)];
end