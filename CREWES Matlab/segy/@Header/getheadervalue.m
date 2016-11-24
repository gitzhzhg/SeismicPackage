function val=getheadervalue(obj,word,scalevalues)
% val=getheadervalue(obj,word)
% val=getheadervalue(obj,word,scalevalues)
%
% getheadervalue will return the set of values associated with the word 
%   defined in the definitions *.cvs file of a TraceHeader or 
%   BinaryHeader object.
%
% This function will return one set of values associated with the word 
%   input at a time.
%
% Inputs:
%  obj=  is the TraceHeader or BinaryHeader object containing the values
%        that the user wishes to retrive.
%  word= is the name of the variable for which the values will be retrived.
%        word must be in the Name column of the definitions *.cvs file
%  scalevalues= is a flag that indicates whenether the values will be
%        returned scaled by the scaling factor according to revision 1 segy
%        standards.  Only some variables are scaled please refer to the 
%        paper on segy revision standards for which items can be scaled.
%        1 indicates that the values will be scaled and 0 indicates that
%        they will not be scaled.
%        ***********************Default=1*********************************
%
% Output:
%  val=  is a vector containing the values stored under the variable word.
% 
% 
% If you would like to set the header values to be something else please
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

% use setheadervalue.

if nargin<3
    scalevalues=1;
end


try
    if isa(obj,'TraceHeader') || isa(obj,'BinaryHeader')
    else
        me=MException('getheadervalue:InvalidInputType',...
            'headerobj must be a TraceHeader or BinaryHeader object');
        throw(me)
    end
    
    if isempty(obj.definitions);
        me=MException('getheadervalue:NonExistantField',...
            'The object does not have any definitions defined');
        throw(me)
    end
    
    namepos=strcmpi(obj.definitions.keys(),'Name');
    possiblewords=obj.definitions.values(:,namepos);
    wordpos=strcmpi(possiblewords,word);
    if all(~wordpos)
        me=MException('getheadervalue:NonExistantName',...
            'word must be the name of a variable listed in the definition file');
        throw(me);
    end
    if scalevalues
        [flag,scalepos,valfmt]=check4scaledwords(word,possiblewords);
        if flag
            val=getscaledvalues(obj,flag,wordpos,scalepos,valfmt);
        else
            val=getvalues(obj,wordpos);
        end
    else
        val=getvalues(obj,wordpos);
    end
    
catch me
    errordlg(me.message,me.identifier);
    error(me.message);
end

end


function [flag,scalepos,valfmt]=check4scaledwords(word,possiblewords)
scwords1={'gelev','selev','sdepth','gdel','sdel','swdep','gwdep'};
scwords2={'sx','sy','gx','gy','cdpx','cdpy'};
scwords3={'sut','gut','sstat','gstat','tstat','laga','lagb','delrt','muts','mute'};
scwords4={'sp'};
scwords5={'sedm'};
scwords6={'fbpicks'};
valfmt=0;
if any(strcmpi(scwords1,word))
    flag=1;
    scalepos=strcmpi(possiblewords,'scalel');
elseif any(strcmpi(scwords2,word))
    flag=2;
    scalepos=strcmpi(possiblewords,'scalco');
    valfmt=strcmpi(possiblewords,'counit');
elseif any(strcmpi(scwords3,word))
    flag=3;
    scalepos=strcmpi(possiblewords,'scalt');
elseif any(strcmpi(scwords4,word))
    flag=4;
    scalepos=strcmpi(possiblewords,'scalsp');
elseif any(strcmpi(scwords5,word))
    flag=5;
    scalepos=10;
elseif any(strcmpi(scwords6,word))
    flag=6;
    scalepos=strcmpi(possiblewords,'scalfb');
else
    flag=0;
    scalepos=0;
end
end

function val=getscaledvalues(obj,flag,wordpos,scalepos,valfmt)
if all(~scalepos)
    val=getvalues(obj,wordpos);
else
    if flag==5
        val=double(getvalues(obj,wordpos));
        val=val./10;
    else
        scale=double(getvalues(obj,scalepos));
        val=double(getvalues(obj,wordpos));
        if all(scale==0)
            val=getvalues(obj,wordpos);
        else
            if scale>0
                val=val.*abs(scale);
            else
                val=val./abs(scale);
            end
        end
    end
    if all(valfmt==4) && ~all(scale==0)
        if all(scale==-100)
            e=[6, 4, 2];
        else
            e=[4, 2, 0];
        end
        
        val2=val;
        val1(1,:)=floor(val2/(10^e(1)));
        val1(2,:)=floor((val2-val1(1,:))/(10^e(2)));
        val1(3,:)=floor((val2-val1(1,:)-val1(2,:))/(10^e(3)));
        val={['Degrees';'Minutes';'Seconds'],val1};
    end
end

end

function val=getvalues(obj,wordpos)
numoftr=size(obj.nontypecasthdr);

st=str2double(obj.definitions.values(wordpos,strcmp(obj.definitions.keys,'startByte')));
ed=str2double(obj.definitions.values(wordpos,strcmp(obj.definitions.keys,'endByte')));
typ=obj.definitions.values{wordpos,strcmp(obj.definitions.keys,'Type')};


if strfind(typ,'ieee')
    val=uint32(zeros(1,numoftr(2)));
    for m=1:numoftr(2)
        val(1,m)=typecast(obj.nontypecasthdr(st:ed,m)','uint32');
    end
    val=checkforrightbyteorder(val,obj.filefmt);
    val=typecast(val,'float32');
    
elseif strfind(typ,'ibm')
    val=uint32(zeros(1,numoftr(2)));
    for m=1:numoftr(2)
        val(1,m)=typecast(obj.nontypecasthdr(st:ed,m)','uint32');
    end
    val=checkforrightbyteorder(val,obj.filefmt);
    val=ibm2ieee(val);
    
else
    val=eval([typ,'(zeros(1,','numoftr(2)))']);
    for m=1:numoftr(2)
        val(1,m)=typecast(obj.nontypecasthdr(st:ed,m)',typ);
    end
    val=checkforrightbyteorder(val,obj.filefmt);
end

end