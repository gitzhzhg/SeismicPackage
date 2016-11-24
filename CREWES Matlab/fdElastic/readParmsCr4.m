function [Dt,Dxz,fractx,nWide,iPlTerm,wncvar,iZlvl,pvel,svel] = ....
            readParmsCr4(parmFile)
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

%Read parameters for design of Wave Number Correction filters (.parc)
fid = fopen(parmFile,'rt');

parmStr(1,:) = 'Dt        ';
parmStr(2,:) = 'Dxz       ';
parmStr(3,:) = 'fractx    ';
parmStr(4,:) = 'nWide     ';
parmStr(5,:) = 'iPlTerm   ';
parmStr(6,:) = 'wncvar    ';
parmStr(7,:) = 'zLvl      ';
parmStr(8,:) = 'pvel      ';
parmStr(9,:) = 'svel      ';
nStr = size(parmStr);
parms = zeros(nStr(1),1);
indChar = 6;                %Indices of parameters in text format
nChParms = length(indChar);
iZlvl = zeros(5);
pvel = zeros(5);
svel = zeros(5);
while 1
    tline = fgetl(fid);
    if ~ischar(tline); break; end
    k = strfind(tline,'%');
    if isempty(k); k = length(tline)+1; end
    if k>5
        dline = tline(1:k-1);
        [parmName, dline2] = strtok(dline);
        ind = strmatch(parmName,parmStr);
        if ind > 0                          %Found parameter
            compCh = find(indChar-ind);
            nCh = length(compCh);
            if nCh==nChParms                %Retrieve numbers
                if ind < 6
                    value = strread(dline2, '%f');
                    parms(ind) = value;
                    %disp(value)
                else
                    if ind == 7
                        zLvl = strread(dline2, '%f');
                        disp(zLvl')
                    end
                    if ind == 8
                        pvel = strread(dline2, '%f');
                        disp(pvel')
                    end
                    if ind == 9
                        svel = strread(dline2, '%f');
                        disp(svel')
                    end
                end
            else                            %Retrieve strings
                wncvar = (strread(dline2, '%c'))';
                disp(wncvar)
            end
        else
            disp([parmName',' not found'])
        end
    end
end
fclose(fid);
%stop
%disp([parms(1),parms(2),parms(3),parms(4)])
Dt          = parms(1);
Dxz         = parms(2);
fractx      = parms(3);
nWide       = parms(4);
iPlTerm     = parms(5);
iZlvl = floor(zLvl/Dxz);
% pvel        = parms(3);
% svel        = parms(4);
% p2vel       = parms(5);
% s2vel       = parms(6);
%disp(parms(1:7))
% if mvXmax>lengthX; mvXmax = lengthX; end
% if mvZmax>lengthZ; mvZmax = lengthZ; end
% itrAcq     = parms(21);
% mvTif      = parms(22);

%         for iParm = 1:nStr
%             if strcmpi(parmName,parmStr(iParm,:))
%                 %parms(iParm) = sscanf(rest,'%f');
%                 parms(iParm) = value;
%                 disp('hit')
%                 break
%             end
%         end