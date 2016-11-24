function sn = setAssociatedSections(obj, sn)
%
% function sn = setAssociatedSections(obj,sn)
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

% reshape sn into cell array with 2 row vectors
sn = cellfun(@(X) padcell(X),sn,'UniformOutput',false);
n = length(sn);
sn = [sn{:}];
sn = reshape(sn,2,n);

% populate section index numbers
sn = vertcat(sn,num2cell(1:n));

% populate associated index numbers (all empty cells at this point)
sn = vertcat(sn,cell(1,n));

l = strncmpi('~log_data',sn(1,:),9);
if ~isequal(sum(l),0) %LAS version 3, for certain
    %get index numbers for all data sections
    b = ~cellfun(@isempty,regexpi(sn(1,:),'data'));
    dat = sn(1,b);
    def = sn(2,b);

    for i=1:numel(dat)
        dat_b = strcmp(sn(1,:),dat(i));
        def_b = strcmp(sn(1,:),['~' char(def(i))]);
        
        %update index to associated definition section
        sn(4,dat_b) = sn(3,def_b);
        
        %update index to associated data section
        sn(4,def_b) = sn(3,dat_b);
    end
    
else %LAS file does not use the ~log_data, ~log_definition style
    a1 = strncmpi('~ascii',sn(1,:),6);
    c1 = strncmpi('~curve',sn(1,:),6);
    a2 = strncmpi('~a',sn(1,:),2);
    c2 = strncmpi('~c',sn(1,:),2); 

    if isequal(sum(a1+c1),2) %found one ~ascii and one ~curve section
        a = a1;
        c = c1;       
    elseif isequal(sum(a2+c2),2) %found one ~a and one ~c section
        a = a2;
        c = c2;
    else
        disp('Code should never reach this point.')
    end
    
    %set section association for ~A or ~ASCII section
    asn = sn{1,c};
    sn(2,a) = {asn(2:numel(asn))};
    sn(4,a) = sn(3,c);
    
    %set section associtaion for ~C or ~CURVE section
    asn = sn{1,a};
    sn(2,c) = {asn(2:numel(asn))};    
    sn(4,c) = sn(3,a);
end

end %end function setAssociatedSections(obj,sn)

function ca = padcell(ca)
   if numel(ca) < 2
       ca{2} = '';
   end
end %end function padcell