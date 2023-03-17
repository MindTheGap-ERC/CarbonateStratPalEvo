load("scenarioAMatlabOutputs.mat") %% load model outputs of scenario A
%% Extract Age Depth Models of Scenario A
for i=1:10
    x=i; % position along shore
    [agemodels,timeA]=adm_transects(x,glob); % extract position
    am_spatA(i,:,:)=agemodels;
end
SL_ScenarioA=glob.SL; % get sea level curve of scenario A
%% 
clear glob stats
load("scenarioBMatlabOutputs.mat") %% load model outputs of scenario B
%% 
for i=1:10
    x=i; % position along shore
    [agemodels,timeB]=adm_transects(x,glob); % extract position
    am_spatB(i,:,:)=agemodels;
end
SL_ScenarioB=glob.SL; % get sea level curve of scenario B
%%
save('scenariosAandBMatlabToR.mat','am_spatA','am_spatB','timeA','timeB',"SL_ScenarioA","SL_ScenarioB")

%%
function [A,t] = adm_transects(x,glob)
si=size(glob.thickness);
A=zeros(si(1),si(3)+1);
for i=1:si(1)
    a1=glob.thickness(i,x,:);
    a2=squeeze(cellfun(@(x) sum(x,'all'),a1));
A(i,:)=cumsum([0;a2])';
end
t=(0:(si(3)))*glob.deltaT;
end

