load("scenarioA_glob_matlab_outputs.mat") %% load model outputs of scenario A

%% Extract Age Depth Models of Scenario A
pos_in_strike_dir = 5; % get adm at 5th grid cell in strike dir
[scenarioA_adm_along_dip, scenarioA_time]=get_adms_along_dip(pos_in_strike_dir,glob); % extract position
scenarioA_sea_level=glob.SL; % get sea level curve of scenario A
clear glob

%%
load("scenarioB_glob_matlab_outputs.mat") %% load model outputs of scenario B

%% 
pos_in_strike_dir = 5;
[scenarioB_adm_along_dip, scenarioB_time]=get_adms_along_dip(pos_in_strike_dir,glob); % extract position
scenarioB_sea_level=glob.SL; % get sea level curve of scenario A
%%
clear glob
save('scenarioA_and_B_matlab_to_R.mat','scenarioA_adm_along_dip','scenarioB_adm_along_dip','scenarioA_time','scenarioB_time',"scenarioA_sea_level","scenarioB_sea_level")

%%
function [A,t] = get_adms_along_dip(pos_in_strike_dir,glob)
si=size(glob.thickness);
A=zeros(si(1),si(3)+1);
for i=1:si(1)
    a1=glob.thickness(i,pos_in_strike_dir,:);
    a2=squeeze(cellfun(@(x) sum(x,'all'),a1));
A(i,:)=cumsum([0;a2])';
end
t=(0:(si(3)))*glob.deltaT;
end

