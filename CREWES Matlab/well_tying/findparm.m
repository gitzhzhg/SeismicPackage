function parm=findparm(parms,name)

np=floor(length(parms)/2);
if((2*np~=length(parms))||~iscell(parms))
    error('invalid parameter array');
end

parm=[];

for k=1:np
    if(strcmp(parms{2*k-1},name))
        parm=parms{2*k};
        return;
    end
end
