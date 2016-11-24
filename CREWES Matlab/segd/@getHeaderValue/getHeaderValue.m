classdef getHeaderValue
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

    %Class for SEGD header definitions
    properties
      headerType;
      headerWord;
      value;
      format;
    end

    methods
    
    function obj = getHeaderValue(hdrobj,hword)
        %Constructor
        if ~isa(hdrobj,'Headers')
            me=MException('getHeaderValue:InvalidInputType',...
                'hdrobj must be a Headers object');
            throw(me)
        end

        if isempty(hdrobj.headerDefRows);
            me=MException('getHeaderValue:NonExistentField',...
                'No header word definitions have been read from file');
            throw(me)
        end

        if ~isKey(hdrobj.headerDefRows,hword)
            me=MException('getHeaderValue:NonExistentName',...
                ['Header word ' hword ...
                ' was not found in the definitions spreadsheet']);
            throw(me);
        end
        
        
        row = hdrobj.headerDefRows(hword);
        sn = str2double(hdrobj.headerDefs(row,hdrobj.headerDefCols('startNibble')));
        en = str2double(hdrobj.headerDefs(row,hdrobj.headerDefCols('endNibble')));
        obj.value = hdrobj.hexHeader(sn:en);
        obj.format = ...
            char(hdrobj.headerDefs(row,hdrobj.headerDefCols('format')));
        obj.headerType = hdrobj.headerType;
        obj.headerWord = hword;
    end
    
    function tf = isEqual(obj,code)
       tf = strcmp(obj.value,code); 
    end
    
    function val = toHexString(obj)
      val = obj.value;
    end
    
    function val = toString(obj)
       val = num2str(toNumber(obj));            
    end
    
    function val = toNumber(obj)
       switch(obj.format)
           case('bcd')
               %binary coded decimal
               val = str2double(obj.value);
           case('ubin')
               %unsigned binary
               val = hex2dec(obj.value);
           case('sbin')
               %signed binary
               signBit = getHeaderValue.getSignBit(obj.value(1));
               
               if(~signBit) %positive number
                    val = hex2dec(obj.value);
               else %negative number
                    nBits  = length(obj.value)*4;
                    maxVal = 2*2^(nBits-1);
                    val = hex2dec(obj.value)-maxVal;
               end
           case('fraction')
               %fraction; almost certain this is unsigned binary
               val = hex2dec(obj.value);
           otherwise
               %assume bcd
               val = str2double(obj.value);
       end
    end
    
    end %end methods
    
    methods (Static)
      
    function sbit = getSignBit(nibble)
        %nibble is a single hex char
        %s == 0; unsigned
        %s == 1; signed      
        sbit = uint8(hex2dec(nibble));
        sbit = bitshift(sbit,-3);
    end
    
    end %end methods (Static)
end