function ddid
%	Display Distribution ID of SeisLab

global S4M

run_presets_if_needed

disp(['SeisLab Distribution ID: ',num2str(S4M.dd)])
