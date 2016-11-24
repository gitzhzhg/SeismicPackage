function obj=setheadervalue(obj,word,value,scalevalues)
% obj=setheadervalue(obj,word,value)
%
% setheadervalue will save the set of values associated with the word
%   defined in the definitions *.cvs file of a TraceHeader or
%   BinaryHeader object.
%
% This function will save one set of values associated with the word
%   input at a time.
%
% Inputs:
%  obj=  is the TraceHeader or BinaryHeader object containing the header
%        where the values will be saved.
%  word= is the name of the variable for which the values will be saved.
%        word must be in the Name column of the definitions *.cvs file
%  value= is the vector containing the values.  value must be the same
%        length as the rest of the header values.  For a TraceHeader Object
%        this is often the same size as the number of traces contained in
%        the file.  For BinaryHeader Object this should be one value per
%        header word.
%  scalevalues= is a flag that indicates whenether the values will be
%        scaled by the scaling factor before they are saved according to
%        revision 1 segystandards.  Only some variables are scaled please
%        refer to the paper on segy revision standards for which items can
%        be scaled.
%        1 indicates that the values will be scaled and 0 indicates that
%        they will not be scaled.
%        ***********************Default=1*********************************
%
% Output:
%  obj=  is the TraceHeader or BinaryHeader object with updated header
%  values
%
%
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

% If you would like to get the header values please use getheadervalue.

try
    if nargin<4
        scalevalues=1;
    end
    
    if isa(obj,'TraceHeader') || isa(obj,'BinaryHeader')
    else
        me=MException('setheadervalue:InvalidInputType',...
            'headerobj must be a TraceHeader or BinaryHeader object');
        throw(me)
    end
    
    if isempty(obj.definitions);
        me=MException('setheadervalue:NonExistantField',...
            'The object does not have any definitions defined');
        throw(me)
    end
    
    namepos=strcmpi(obj.definitions.keys(),'Name');
    possiblewords=obj.definitions.values(:,namepos);
    wordpos=strcmpi(possiblewords,word);
    
    if all(~wordpos)
        me=MException('setheadervalue:NonExistantName',...
            'word must be the name of a variable listed in the definition file');
        throw(me);
    end
    
    numoftr=size(obj.nontypecasthdr);
    if length(value)~=numoftr(2);
        me=MException('setheadervalue:InvalidSize',...
            'value must include a numeric value for all traces');
        throw(me);
    end
    
    if scalevalues
        [flag,scalepos,valfmt]=check4scaledwords(word,possiblewords);
        if flag
            obj=getscaledvalues(obj,value,flag,wordpos,scalepos,valfmt);
        else
            obj=savevalues(obj,wordpos,value);
        end
    else
        obj=savevalues(obj,wordpos,value);
    end
    
catch me
    errordlg(me.message,me.identifier);
    error(me.message);
end
end


% the purpose of this function is to test for scaled variables so that the
% getscaledvalues funtion can scale values appropriately.
function [flag,scalepos,valfmt]=check4scaledwords(word,possiblewords)
scwords1={'gelev','selev','sdepth','gdel','sdel','swdep','gwdep'};
scwords2={'sx','sy','gx','gy','cdpy','cdpx'};
scwords3={'sut','gut','sstat','gstat','tstat','laga','lagb','delrt','muts','mute'};
scwords4={'sp'};
scwords5={'sedm'};
scwords6={'fbpicks'};
valfmt=0;
if any(strcmpi(scwords1,word))
    flag=1;
    scalepos={'scalel'};
elseif any(strcmpi(scwords2,word))
    flag=2;
    scalepos={'scalco'};
    valfmt=strcmpi(possiblewords,'counit');
elseif any(strcmpi(scwords3,word))
    flag=3;
    scalepos={'scalt'};
elseif any(strcmpi(scwords4,word))
    flag=4;
    scalepos={'scalsp'};
elseif any(strcmpi(scwords5,word))
    flag=5;
    scalepos=10;
elseif any(strcmpi(scwords6,word))
    flag=6;
    scalepos={'scalfb'};
elseif any(strcmpi('scalel',word))
    flag=11;
    scalepos=scwords1;
elseif any(strcmpi('scalco',word))
    flag=12;
    scalepos=scwords2;
elseif any(strcmpi('scalt',word))
    flag=13;
    scalepos=scwords3;
elseif any(strcmpi('scalsp',word))
    flag=14;
    scalepos=scwords4;
elseif any(strcmpi('scalfb',word));
     flag=16;
    scalepos=scwords6;
else
    flag=0;
    scalepos=0;
end
end

% the purpose of this function is to scale the values appropriately
function obj=getscaledvalues(obj,value,flag,wordpos,scalepos,valfmt)

if iscell(scalepos)
    words=scalepos;
    scalepos=flag;
end

valsz=size(value);
if valsz(1)==3
    scale=double(getheadervalue(obj,words{1,1}));
    if all(scale==-100)
        e=[6, 4, 2];
    else
        e=[4, 2, 0];
    end
    
    val1=value;
    value=(val1(1,:)*(10^e(1)))+(val1(2,:)*(10^e(2)))+(val1(3,:)*(10^e(3)));
    obj=savevalues(obj,wordpos,value);
    obj=savevalues(obj,valfmt,4*ones(size(value)));
end

if all(~scalepos)
    obj=savevalues(obj,wordpos,value);
else
    if flag==5
        value=value*10;
    elseif any(flag==[11,12,13,14,16])
        for k=1:length(words)
            val=double(getheadervalue(obj,words{k}));
            value=double(value);
            if value<0
                val=val.*abs(value);
            else
                val=val./abs(value);
            end
            obj=setheadervalue(obj,words{k},val,0);
        end
        obj=savevalues(obj,wordpos,value);
    else
        scale=double(getheadervalue(obj,words{1,1}));
        if all(scale==0)
           obj=savevalues(obj,wordpos,value);
        else
            if scale<0
                value=value.*abs(scale);
            else
                value=value./abs(scale);
            end
            obj=savevalues(obj,wordpos,value);
        end
        
    end
end
end



function obj=savevalues(obj,wordpos,value)
st=str2double(obj.definitions.values(wordpos,strcmp(obj.definitions.keys,'startByte')));
ed=str2double(obj.definitions.values(wordpos,strcmp(obj.definitions.keys,'endByte')));
typ=obj.definitions.values{wordpos,strcmp(obj.definitions.keys,'Type')};
if strcmpi(typ,'ieee')||strcmpi(typ,'ibm')
    typ='single';
end

if ~isa(value,typ)
    eval(['value=',typ,'(value);']);
end

for k=1:length(value)
    val=value(k);
    val=checkforrightbyteorder(val,obj.filefmt);
    val=typecast(val,'uint8');
    obj.nontypecasthdr(st:ed,k)=val';
end


end