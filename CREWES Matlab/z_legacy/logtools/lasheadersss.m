function lasheader = lasheadersss(lash,start,stop,step,units)

% lasheader = lasheadersss(lasheader,start,stop,step,units)
% lasheader = lasheadersss(lasheader,start,stop,step)
%
% Given an las header in a string matrix (such as is provided by
% readlas) LASHEADERSSS change its start, stop, and step to
% the appropriate values. If units is provided, it is used to specify
% the units of start stop and step, otherwise they are unchanged.
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

if(nargin<5)
    units='';
end
[m,n]=size(lash);

for k=1:m
    tmpstr=lash(k,:);
    [t,r]=strtok(tmpstr);
    if(findstr(t,'STRT'))
        lash(k,:)=replacenum(tmpstr,start,units);
    end
    if(findstr(t,'STOP'))
        lash(k,:)=replacenum(tmpstr,stop,units);
    end
    if(findstr(t,'STEP'))
        lash(k,:)=replacenum(tmpstr,step,units);
    end
end
lasheader=lash;

function tmpstr=replacenum(instring,num,units)
        n=length(instring);
        [t,r]=strtok(instring);
        %test for units adjacent to the flag
        [tt,rr]=strtok(t,'.');
        if(isempty(rr))
            [tt,r]=strtok(r); %find units
            iu=findstr(tt,instring);
            if(~isempty(units))%replace units
                instring=[instring(1:iu(1)-1) '.' units instring(iu(1)+2:end)];
            end
            numfield=strtok(r);
        else
            iu=findstr(rr,instring);
            if(~isempty(units))%replace units
                instring=[instring(1:iu(1)-1) '.' units instring(iu(1)+2:end)];
            end
            numfield=strtok(r);
        end
        if(isempty(numfield)|strcmp(numfield(1),':'))
            %if here, then "numfield" is actually a comment and we have no start specified
            % ugh
            tmpstr=[instring(1:iu(1)+2) num2str(num) instring(iu(1)+3:end)];
        else
            ii=findstr(numfield,instring);%find the numeric field
            tmpstr=[instring(1:ii(1)-1) num2str(num) instring(ii(1)+length(numfield):end)];
            
        end
        if(length(tmpstr)<n) tmpstr=[tmpstr blanks(n-length(tmpstr))]; end
        if(length(tmpstr)>n) tmpstr=tmpstr(1:n); end