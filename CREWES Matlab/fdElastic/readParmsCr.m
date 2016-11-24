function [Dt,Dxz,pvel,svel,fractx,nWide,iPlTerm,wncvar] = ....
            readParmsCr(parmFile)
%Read parameters for design of a Wave Number Correction filter (.parc)
%function [parms] = readParms(parmFile)
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

%Key code 3 Read parameters from a text file *****************************
fid = fopen(parmFile,'rt');


parmStr(1,:) = 'Dt        ';
parmStr(2,:) = 'Dxz       ';
parmStr(3,:) = 'pvel      ';
parmStr(4,:) = 'svel      ';
parmStr(5,:) = 'fractx    ';
parmStr(6,:) = 'nWide     ';
parmStr(7,:) = 'iPlTerm   ';
parmStr(8,:) = 'wncvar    ';
nStr = size(parmStr);
parms = zeros(nStr(1),1);

indChar = (8);
nChParms = length(indChar);
while 1
    tline = fgetl(fid);
    if ~ischar(tline); break; end
    k = strfind(tline,'%');
    if isempty(k); k = length(tline)+1; end
    if k>5
        dline = tline(1:k-1);
        [parmName] = strread(dline, '%s',1);
        %[parmName Cstring] = sscanf(dline, '%s %s');
        %disp(parmName)
        ind = strmatch(parmName,parmStr);
        if ind > 0
            %parmsC(ind,:) = Cstring;
            compCh = find(indChar-ind);
            nCh = length(compCh);
            if nCh==nChParms
                [parmName value] = strread(dline, '%s %f');
                %[parmName value] = sscanf(dline, '%s %f');
                %[value] = strread(dline, '%f');
                parms(ind) = value;
            else
                %[valueC] = strread(dline, '%s %s,');
                %[valueC] = strread(dline, '%s,');
                %[parmName Cstring] = strread(dline, '%s');
                [Cstring] = strread(dline, '%c');
                %parms(ind) = Cstring;
%                 cmd = ([parmName '= disp('''  Cstring ')''']);
%                 disp(cmd)
%                 eval(cmd)
                %disp(Cstring)
                eval([Cstring;';'])
                %eval(Cstring)
            end
        else
            disp([parmName',' not found'])
        end
    end
end
fclose(fid);
%disp([parms(1),parms(2),parms(3),parms(4)])
Dt          = parms(1);
Dxz         = parms(2);
pvel        = parms(3);
svel        = parms(4);
fractx      = parms(5);
nWide       = parms(6);
iPlTerm     = parms(7);
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