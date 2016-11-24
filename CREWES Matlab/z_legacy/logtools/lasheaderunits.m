function lasheader = lasheaderunits(lasheader,units,logsonly)
% lasheader = lasheaderunits(lasheader,units,logsonly)
%
% Given an las header in a string matrix (such as is provided by
% readlas) LASHEADERUNITS cchanges its units from whatever it is to
% either metric or imperial. The second argument (units) should either
% be the string 'metric' or 'imperial'. The third argument is a logical
% flag. If 1, then only units corresponding to logs are changed and depths
% are left alone. It defaults to 0.
%
% G.F. Margrave, CCR, Aug 94
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
if(nargin<3)
    logsonly=0;
end
[nlines,nchar]=size(lasheader);
%
%  need to modify the lasheader
%
	k=4;
	%start with fourth line
	units=lower(units);
	if( ~strcmp(units,'metric') & ~strcmp(units,'imperial') )
		error(' units string must be either imperial or metric')
	end
	flag=0;
	if( strcmp(units,'metric') )
		flag=1;
	end
	str=lasheader(k,:);
	[nrows,ncols]=size(lasheader);
	% define the units fields
		ft= '.FT  ';
		ft2= '.F   ';
		m=  '.M   ';
		mm= '.MM  ';
		in= '.IN  ';
		cm= '.CM  ';
		kgm='.KG/M';
		lbf='.LB/F';
		om= '.OHMM';
		of= '.OHMF';
		msm='.US/M';
		msm2='.USM ';
		msf='.US/F';
		kg= '.KG  ';
		lb= '.LB  ';
		m2ft = 3.2808;
		ft2m = 1/m2ft;
		mm2in= .03937;
		in2mm= 1./mm2in;
		cm2in= 10*mm2in;
		in2cm= 1./cm2in;
		kg2lb= 2.2046;
		lb2kg= 1/kg2lb;
		kgm2lbf = kg2lb*m2ft;
		lbf2kgm = 1/kgm2lbf;
		msm2msf = ft2m;
		msf2msm = 1./msm2msf;
		om2of = m2ft;
		of2om = 1./om2of;
	while( ~strcmp(str(1:2),'~A') )
%
		ind=find(str=='.');
		if(isempty(ind)) izone=6:10;
		else izone=ind(1):ind(1)+4;
		end
		test=str(izone);
		ind=find(str==':');
		number=str(izone(5)+1:ind-1);
		cmt=str(ind:length(str));
		if( str(1)=='~')
			if( strcmp(upper(str(1:6)),'~CURVE') )
				chgnum=0;
			else
				chgnum=1;
			end
		end
        %if logsonly is 1, then only information in the ~Curve block is changed
        %if logsonly is 0, then all changes are made
		if(~isempty(ind) & ind>(izone(5)+1))
		if( flag==1 )
			%convert from imperial to metric
			if( strcmp(test,ft) | strcmp(test,ft2) )
				%convert to meters
                testold=test;
				test=m;
				if( chgnum & ~logsonly)
					number=str_num2num(number,ft2m);
                elseif(chgnum)
                    test=testold; %change it back
				end
			elseif( strcmp(test,in) )
				%comvert to mm
                testold=test;
				test=mm;
				if( chgnum & ~logsonly)
					number=str_num2num(number,in2mm);
                elseif(chgnum)
                    test=testold;
				end
			elseif( strcmp(test,lb) )
				%convert to kg
                testold=test;
				test=kg;
				if( chgnum & ~logsonly)
					number=str_num2num(number,lb2kg);
                elseif(chgnum)
                    test=testold;
				end
			elseif( strcmp(test,lbf) )
				%convert to kgm
                testold=test;
				test=kgm;
				if( chgnum & ~logsonly)
					number=str_num2num(number,lbf2kgm);
                elseif(chgnum)
                    test=testold;%change it back
				end
			elseif( strcmp(test,msf) )
				%convert to msm
                testold=test;
				test=msm;
				if( chgnum & ~logsonly)
					number=str_num2num(number,msf2msm);
                elseif(chgnum)
                    test=testold;%change it back
				end
			elseif( strcmp(test,of) )
				%convert to om
                testold=test;
				test=om;
				if( chgnum & ~logsonly)
					number=str_num2num(number,of2om);
                elseif(chgnum)
                    test=testold;%change it back
				end
			end
		else % convert from metric to imperial
			if( strcmp(test,m) )
				%convert to ft
                testold=test;
				test=ft;
				if( chgnum & ~logsonly)
					number=str_num2num(number,m2ft);
                elseif(chgnum)
                    test=testold;%change it back
				end
			elseif( strcmp(test,mm) )
				%comvert to in
                testold=test;
				test=in;
				if( chgnum & ~logsonly)
					number=str_num2num(number,mm2in);
                elseif(chgnum)
                    test=testold;%change it back
				end
			elseif( strcmp(test,cm) )
				%comvert to in
                testold=test;
				test=in;
				if( chgnum & ~logsonly)
					number=str_num2num(number,cm2in);
                elseif(chgnum)
                    test=testold;%change it back
				end
			elseif( strcmp(test,kg) )
				%convert to lb
                testold=test;
				test=lb;
				if( chgnum & ~logsonly)
					number=str_num2num(number,kg2lb);
                elseif(chgnum)
                    test=testold;%change it back
				end
			elseif( strcmp(test,kgm) )
				%convert to lbf
                testold=test;
				test=lbf;
				if( chgnum & ~logsonly)
					number=str_num2num(number,kgm2lbf);
                elseif(chgnum)
                    test=testold;%change it back
				end
			elseif( strcmp(test,msm) | strcmp(test,msm2) )
				%convert to msf
                testold=test;
				test=msf;
				if( chgnum & ~logsonly)
					number=str_num2num(number,msm2msf);
                elseif(chgnum)
                    test=testold;%change it back
				end
			elseif( strcmp(test,om) )
				%convert to of
                testold=test;
				test=of;
				if( chgnum & ~logsonly)
					number=str_num2num(number,om2of);
                elseif(chgnum)
                    test=testold;%change it back
				end
			end
		end
		str= [str(1:izone(1)-1) test number cmt];
		lasheader(k,:)=str;
		end
		k=k+1;
		str=lasheader(k,:);
	end