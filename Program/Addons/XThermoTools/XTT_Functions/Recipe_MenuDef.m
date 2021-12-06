function [AntidoteMenu] = Recipe_MenuDef(Input)
%
% Version PL (03.03.2020)


AntidoteMenu{1}  = '-------------------------------------------';
AntidoteMenu{2}  = '[Recipe 1]   Find optimal P-T(-X)';
AntidoteMenu{3}  = '[Recipe 2]   P-T map of Q factors';
AntidoteMenu{4}  = '[Recipe 3]   P-T uncertainty';
AntidoteMenu{5}  = '-------------------------------------------';
AntidoteMenu{6}  = '[Recipe 4]   Find optimal P-T (Single Phase)';
AntidoteMenu{7}  = '[Recipe 5]   P-T map (Single Phase)';
AntidoteMenu{8}  = '[Recipe 6]   P-T uncertainty (Single Phase)';
AntidoteMenu{9}  = '-------------------------------------------';
AntidoteMenu{10} = '[Recipe 7]   Bulk sensitivity';
AntidoteMenu{11} = '[Recipe 8]   P-T sensitivity ';
AntidoteMenu{12} = '[Recipe 9]   P-T-bulk sensitivity';
AntidoteMenu{13} = '-------------------------------------------';
AntidoteMenu{14} = '[Recipe 10]  Floating window (fixed P-T, variable bulk)';
AntidoteMenu{15} = '[Recipe 11]  Scanning window (find optimal P-T, variable bulk)';
AntidoteMenu{16} = '[Recipe 12]  Growing window (find optimal P-T, variable bulk)';
AntidoteMenu{17} = '[Recipe 13]  Chemical potential mapping (fixed P-T)';
AntidoteMenu{18} = '-------------------------------------------';
AntidoteMenu{19} = '[Recipe 14]  Scanning H (fixed P-T)';
AntidoteMenu{20} = '[Recipe 15]  Scanning C (fixed P-T)';
AntidoteMenu{21} = '[Recipe 16]  Scanning O (fixed P-T)';
AntidoteMenu{22} = '-------------------------------------------';
AntidoteMenu{23} = '[Recipe 17]  Advanced P-T map of Q factors';
AntidoteMenu{24} = '[Recipe 18]  Advanced Find Optimal P-T (multi-phase)';

return
