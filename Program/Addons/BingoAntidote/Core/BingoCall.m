function [A1,V1,C1,TOTAL2,Report] = BingoCall(WorkVariMod,WorkVariXMap,Report,DoWePrint,UpdateText2Disp,app)

% Step 0 – Exclude melt (added 11.09.2025)
if isequal(app.BinGfDef.Melt.Activate,1)
    if isequal(app.BinGfDef.Melt.Include,0)
        [IsMelt,WhereMelt] = find(ismember(WorkVariMod.Names,app.BinGfDef.Melt.DBName));
        if isequal(IsMelt,1)
            WorkVariMod.Names(WhereMelt) = [];
            WorkVariMod.Indice = [1:length(WorkVariMod.Names)];
            WorkVariMod.COMP(WhereMelt,:) = [];
            WorkVariMod.VolFrac(WhereMelt) = [];
            WorkVariMod.VolFrac = WorkVariMod.VolFrac ./ sum(WorkVariMod.VolFrac);
            WorkVariMod.Dens(WhereMelt) = [];
            WorkVariMod.NbPhases = length(WorkVariMod.Names);
        end
    end
end

% Step 1 - Assemblage
[Evaluation.assemblage,Link,Report] = Bingo_Qasm(WorkVariMod,WorkVariXMap,Report,DoWePrint,UpdateText2Disp,app);

% Step 2 - Volume
[Evaluation,Report] = Bingo_Qvol(WorkVariMod,WorkVariXMap,Link,Evaluation,Report,DoWePrint,UpdateText2Disp,app);

% Step 3 - Compositions
[Evaluation,Report] = Bingo_Qcmp(WorkVariMod,WorkVariXMap,Link,Evaluation,Report,DoWePrint,UpdateText2Disp,app);


% eduester: new total output
A1=Evaluation.assemblage;
V1=Evaluation.Volume;
V2=Evaluation.assemblage/100*Evaluation.Volume;
C1=Evaluation.Compositions;
C2=Evaluation.assemblage/100*Evaluation.Volume/100*Evaluation.Compositions;
TOTAL= (Evaluation.assemblage+Evaluation.Volume + Evaluation.Compositions)/3;

%change edzester: *Evaluation.Volume/100
TOTAL2= (Evaluation.assemblage+Evaluation.assemblage/100*Evaluation.Volume + Evaluation.assemblage/100*Evaluation.Compositions)/3;

%TOTAL2= (Evaluation.assemblage+Evaluation.assemblage/100*Evaluation.Volume + Evaluation.assemblage/100*Evaluation.Volume/100*Evaluation.Compositions)/3;

if UpdateText2Disp
    app.Report_Bingo{end+1} = '##### CHEMICAL POTENTIAL OF COMPONENTS ##### ';
    
    for i = 1:length(WorkVariMod.ChemComp)
        app.Report_Bingo{end+1} = [WorkVariMod.ChemComp{i},' = ',num2str(round(WorkVariMod.ChemPot(i),4)),' J'];
    end
    
    app.Report_Bingo{end+1} = ' ';
    app.Text_Report.Value = app.Report_Bingo;
end

if DoWePrint
    % Print out the chemical potential of the components
    disp(' ')
    fprintf('\n%s\n','##### CHEMICAL POTENTIAL OF COMPONENTS ##### ');
    fprintf('%s\t%s\n','Oxide','mu (J)');
    
    if ~isfield(WorkVariMod,'ChemComp')
        f = errordlg({'There is something missing in your database:','CHEMICAL POTENTIAL DEFINITIONS FOR COMPONENTS'}, 'Bingo-Antidote', 'modal');
        error('Error in the database reading: The database you''re using does not include the phase definitions to calculate the chemical potential of components',' ');
    end
    
    for i = 1:length(WorkVariMod.ChemComp)
        fprintf('%s\t%.0f\n',WorkVariMod.ChemComp{i},WorkVariMod.ChemPot(i));
    end
    
    % Print out the stable assemblage
    disp(' ')
    fprintf('\n%s\n','##### STABLE ASSEMBLAGE (THERIAK) ##### ');
    fprintf('%s\t%s\t%s\n','Phase (solids)','Volume (%)','Density (kg.m^3)');
    %keyboard
    for i=1:length(WorkVariMod.Names)
        if length(WorkVariMod.Names{i}) > 7
            fprintf('%s\t%.2f\t\t%.0f\n',WorkVariMod.Names{i},WorkVariMod.VolFrac(i)*100,WorkVariMod.Dens(i)*1000);
        else
            fprintf('%s\t\t%.2f\t\t%.0f\n',WorkVariMod.Names{i},WorkVariMod.VolFrac(i)*100,WorkVariMod.Dens(i)*1000);
        end
    end 
    
    if WorkVariMod.FluidPhases
        fprintf('\n%s\t%s\t%s\t%s\n','Phase (fluid)','XH2O','XCO2','Density (kg.m^3)');
        WhereO = find(ismember(WorkVariMod.Els,'O'));
        WhereH = find(ismember(WorkVariMod.Els,'H'));
        WhereC = find(ismember(WorkVariMod.Els,'C'));
        
        for i=1:WorkVariMod.FluidPhases
            O = WorkVariMod.FluidCOMP(i,WhereO);
            if length(WhereH)
                if length(WhereC)
                    H = WorkVariMod.FluidCOMP(i,WhereH);
                    C = WorkVariMod.FluidCOMP(i,WhereC);
                    if H>0 || C>0
                        XH2O = H/2/(C+H/2);
                        XCO2 = C/(C+H/2);
                    else
                        XH2O = 0; XCO2 = 0;
                    end
                else
                    H = WorkVariMod.FluidCOMP(i,WhereH);
                    if H > 0
                        % Check for melt
                        if O > H/2   % other species are there - melt
                            XH2O = (H+H/2)/sum(WorkVariMod.FluidCOMP(i,:));
                            XCO2 = 0;
                        else
                            XH2O = 1; XCO2 = 0;
                        end
                    else
                        XH2O = 0; XCO2 = 0;
                    end
                end
            else
                XH2O = 0;
                XCO2 = 0;
            end
            if length(char(WorkVariMod.FluidNames{i})) < 7
                fprintf('%s\t\t%.3f\t%.3f\t%.0f\n',WorkVariMod.FluidNames{i},XH2O,XCO2,WorkVariMod.FluidDens(i)*1000);
            else
                fprintf('%s\t%.3f\t%.3f\t%.0f\n',WorkVariMod.FluidNames{i},XH2O,XCO2,WorkVariMod.FluidDens(i)*1000);
            end
            
        end
            
            
    end
    %fprintf('%s\n','-------------');
end
return