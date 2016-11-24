function mryflag=memoryallowancecheck(szneeded,type,allowance)
% mryflag=memoryallowancecheck(szneeded,type,allowance)
%
% memoryallowancecheck returns a flag stating whenether or not there is
% enough memory availble to create an array.
%
% Inputs:
%    szneeded is a vector containing the [length width] of the size of the
%      array desired
%    type is a string indicating the type the array will be.  acceptable
%      values are: 'uint8','int8','uint16','int16','uint32','int32',
%                'char','uchar','double','single'
%    allowance is the fraction of the maximum memory the array is being 
%      compared to. allowance=1 wouldbe 100% of the maximum memory
%      available.  Please note that this will cause the operating system to
%      become sluggish.
%      **************************Default 0.25*****************************
%
% Outputs:
%    mryflag is a logical flag indicating if sufficient memory is
%    available.  1 indicates that there is enough memory and 0 indicates
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

%    that there is not


if nargin<3
    allowance=0.25;
end

[~, sysV]=memory;
mryflag=true;
% get number of bytes each number in array requires
num=1;
numty=typecast(num,type);
sznum=size(numty);sznum=8/sznum(2);

%calculate size of array required
arraysz=szneeded(1)*szneeded(2)*sznum;

%compare total memory to arraysz

if (allowance*sysV.PhysicalMemory.Total)<arraysz
    warndlg('Array requires more memory then is available',...
        'Insufficent Memory');
    mryflag=false;
    return
end

%compare avaliable memory to arraysz
if (allowance*sysV.PhysicalMemory.Available)<arraysz
    response=questdlg(char('Creating Array requires saving data to disk.',...
        'This could make the reading process slower','Would You Like To Continue'),...
        'Approaching Maximum Memory','Yes','No','No');
    if strcmp(response,'Yes')
        mryflag=True;
    else
        mryflag=false;
    end
end


end