function [Dt,Dxz,pvel,svel,p2vel,s2vel,fractx,nWide,iPlTerm,wncvar] = ....
            readParmsCr3(parmFile)
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
parmStr(3,:) = 'fractx    ';
parmStr(4,:) = 'nWide     ';
parmStr(5,:) = 'iPlTerm   ';
parmStr(6,:) = 'pvel      ';
parmStr(7,:) = 'svel      ';
parmStr(8,:) = 'z2lvl     ';
parmStr(9,:) = 'p2vel     ';
parmStr(10,:) = 's2vel     ';
parmStr(11,:) = 'z3lvl     ';
parmStr(12,:) = 'p3vel     ';
parmStr(13,:) = 's3vel     ';
parmStr(14,:) = 'z4lvl     ';
parmStr(15,:) = 'p4vel     ';
parmStr(16,:) = 's4vel     ';
parmStr(17,:) = 'z5lvl     ';
parmStr(18,:) = 'p5vel     ';
parmStr(19,:) = 's5vel     ';
parmStr(20,:) = 'wncvar    ';
nStr = size(parmStr);
parms = zeros(nStr(1),1);
parms(5) = 0;
parms(6) = 0;
indChar = 10;
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
        %disp(ind)
        if ind > 0                          %Found parameter
            %parmsC(ind,:) = Cstring;
            compCh = find(indChar-ind);
            %disp(compCh)
            nCh = length(compCh);
            %disp(nCh)
            if nCh==nChParms                %Retrieve numbers
                [parmName value] = strread(dline, '%s %f');
                %[parmName value] = sscanf(dline, '%s %f');
                %[value] = strread(dline, '%f');
                parms(ind) = value;
                %disp(value)
            else                            %Retrieve strings
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
%stop
%disp([parms(1),parms(2),parms(3),parms(4)])
Dt          = parms(1);
Dxz         = parms(2);
fractx      = parms(3);
nWide       = parms(4);
iPlTerm     = parms(5);
pvel        = parms(6);
svel        = parms(7);
p2vel       = parms(9);
s2vel       = parms(10);
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