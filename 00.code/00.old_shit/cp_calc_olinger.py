import sympy as sp
from sympy import symbols, simplify, Eq
import math

Cd = .6
Cq = .001
L = 2.1*2+.9*2
rho = 1.200
A_corr = 1

def cp_calc(ratio=1, bldg_type = 'highrise', azimuth=0, room_type='1_window', cp_eq=True, zone_x=10, zone_y=10, wwr=.1, zn=0, corner_window=True):
    
    # print(ratio, bldg_type, azimuth, room_type, cp_eq, zone_x, zone_y, wwr, zn, corner_window)
    
    if room_type == '3_window':
        surfaces = ['window_0_office_coef','NA1','door_office_coef','window_side_office_coef']
        
    elif room_type == '1_window':
        surfaces = ['window_0_office_coef','window_side_office_coef','door_office_coef','NA3']
        
    else:
        surfaces = ['window_0_office_coef','NA1','door_office_coef','NA3']
        
      
    WindPressureCoefficientValues = {}

    if bldg_type == 'lowrise':

        alpha_rad = alpha*math.pi/180
        cp = 0.6*math.log(1.248 - 0.703*math.sin(alpha_rad/2) - 1.175*math.sin(alpha_rad)**2 + 
            0.131*math.sin(2*alpha_rad*math.log(ratio))**3 + 0.769*math.cos(alpha_rad/2) + 
            0.07 * math.log(ratio)**2 * math.sin(alpha_rad/2)**2 + 0.717*math.cos(alpha_rad/2)**2)

    else:
        CPHighRiseWall = [[0.60, 0.54, 0.23,  -0.25, -0.61, -0.55, -0.51, -0.55, -0.61, -0.25, 0.23,  0.54],
            [0.60, 0.48, 0.04,  -0.56, -0.56, -0.42, -0.37, -0.42, -0.56, -0.56, 0.04,  0.48],
            [0.60, 0.44, -0.26, -0.70, -0.53, -0.32, -0.22, -0.32, -0.53, -0.70, -0.26, 0.44]]
        for FacadeNum in range(4):
            WindPressureCoefficientValues[surfaces[FacadeNum]] = {              
                "airflownetwork_multizone_windpressurecoefficientarray_name": "ventos",
                "idf_max_extensible_fields": 0,
                "idf_max_fields": 14
            }
            FacadeAng = (azimuth + FacadeNum * 90)%360
            if FacadeNum == 0 or FacadeNum == 2:
                SideRatio = 1.0 / ratio
            else:
                SideRatio = ratio
            
            SideRatioFac = math.log(SideRatio)
            
            for WindDirNum in range(12):
                WindAng = WindDirNum * 30.0
                IncAng = abs(WindAng - FacadeAng)
                
                if IncAng > 180.0:
                    IncAng = 360.0 - IncAng
                
                IAng = int(IncAng / 30.0)
                DelAng = IncAng%30.0
                WtAng = 1.0 - DelAng / 30.0

                SR = min(max(SideRatio, 0.25), 4.0)
                if (SR >= 0.25 and SR < 1.0):
                    ISR = 0
                    WtSR = (1.0 - SR) / 0.75
                else:
                    ISR = 1
                    WtSR = (4.0 - SR) / 3.0
                    
                cp = WtSR * (WtAng * CPHighRiseWall[ISR][IAng] + (1.0 - WtAng) * CPHighRiseWall[ISR][IAng + 1]) + (1.0 - WtSR) * (WtAng * CPHighRiseWall[ISR + 1][IAng] + (1.0 - WtAng) * CPHighRiseWall[ISR + 1][IAng + 1])
    
                WindPressureCoefficientValues[surfaces[FacadeNum]]["wind_pressure_coefficient_value_"+str(WindDirNum+1)] = cp
        if cp_eq:
            WindPressureCoefficientValues = const_calc(room_type, WindPressureCoefficientValues, zone_x=zone_x, zone_y=zone_y, wwr=wwr, zn=zn, corner_window=corner_window)
            
    return(WindPressureCoefficientValues)

def const_calc(room_type, WindPressureCoefficientValues, zone_x, zone_y, wwr, zn, corner_window):
    
    if room_type == '3_window':
        A = (zone_x+zone_y)*wwr/2
        node1 = 'NA1'
        node3 = 'window_side_office_coef'
    elif room_type == '1_window':
        A = (zone_x+zone_y)*wwr/2
        node3 = 'NA3'
        node1 = 'window_side_office_coef'
    else:
        A = (zone_x)*wwr
        node1 = 'NA1'
        node3 = 'NA3'
    
    new_cp_values = {             
        "airflownetwork_multizone_windpressurecoefficientarray_name": "ventos",
        "idf_max_extensible_fields": 0,
        "idf_max_fields": 14
    }
    Cjan, Cjancorr, Cpor, Cp_0, Cp_1, Cp_2, Cp_3, Pout, Pd, Pcorr, P0, P1, P2, P3, P4, P5 = symbols('Cjan, Cjancorr, Cpor, Cp_0, Cp_1, Cp_2, Cp_3, Pout, Pd, Pcorr, P0, P1, P2, P3, P4, P5')

    p_2 = Eq(Cjan*(P2-Pout-Pd*Cp_3)-Cjancorr*(Pcorr-P2))
    p_3 = Eq(Cjan*(P3-Pout-Pd*Cp_1)-Cjancorr*(Pcorr-P3))
    
    if corner_window:
        p_0 = Eq(Cjan*(P0-Pout-Pd*Cp_3)-Cjancorr*(Pcorr-P0) + Cjan*(P0-Pout-Pd*Cp_2))
        p_1 = Eq(Cjan*(P1-Pout-Pd*Cp_1)-Cjancorr*(Pcorr-P1)+ Cjan*(P1-Pout-Pd*Cp_2))
        p_4 = Eq(Cjan*(P4-Pout-Pd*Cp_3)-Cjancorr*(Pcorr-P4) + Cjan*(P4-Pout-Pd*Cp_0))
        p_5 = Eq(Cjan*(P5-Pout-Pd*Cp_1)-Cjancorr*(Pcorr-P5) + Cjan*(P5-Pout-Pd*Cp_0))
    else:
        p_0 = Eq(Cjan*(P0-Pout-Pd*Cp_3)-Cjancorr*(Pcorr-P0))
        p_1 = Eq(Cjan*(P1-Pout-Pd*Cp_1)-Cjancorr*(Pcorr-P1))
        p_4 = Eq(Cjan*(P4-Pout-Pd*Cp_3)-Cjancorr*(Pcorr-P4))
        p_5 = Eq(Cjan*(P5-Pout-Pd*Cp_1)-Cjancorr*(Pcorr-P5))

    p_0 = sp.expand(sp.solve(p_0, P0)[0])
    p_1 = sp.expand(sp.solve(p_1, P1)[0])
    p_2 = sp.expand(sp.solve(p_2, P2)[0])
    p_3 = sp.expand(sp.solve(p_3, P3)[0])
    p_4 = sp.expand(sp.solve(p_4, P4)[0])
    p_5 = sp.expand(sp.solve(p_5, P5)[0])

    p_corr = Eq(Cpor*(Pcorr-p_0)+Cpor*(Pcorr-p_1)+Cpor*(Pcorr-p_2)+Cpor*(Pcorr-p_3)+Cpor*(Pcorr-p_4)+Cpor*(Pcorr-p_5)+Cjan*(2*Pcorr-Pout-Pd*(Cp_0+Cp_2)))
    p_corr = sp.expand(sp.solve(p_corr, Pcorr)[0])
    
    Cjan_value = Cd**2+A**2*2*rho
    Cjancorr_value = Cd**2+A_corr**2*2*rho
    Cpor_value = Cq**2+L**2
        
    for wind_dir in range(1,13):
        
        if zn %2 == 0:
            const = p_corr.subs({
                'Pd': 4.7,  # 2.8**2*1.2 / 2
                'Pout': 92055,
                'Cjan': Cjan_value,
                'Cjancorr': Cjancorr_value,
                'Cpor': Cpor_value,
                'Cp_0': WindPressureCoefficientValues[node1]["wind_pressure_coefficient_value_"+str(wind_dir)],
                'Cp_1': WindPressureCoefficientValues['door_office_coef']["wind_pressure_coefficient_value_"+str(wind_dir)],
                'Cp_2': WindPressureCoefficientValues[node3]["wind_pressure_coefficient_value_"+str(wind_dir)],
                'Cp_3': WindPressureCoefficientValues['window_0_office_coef']["wind_pressure_coefficient_value_"+str(wind_dir)]
            })
        else:
            const = p_corr.subs({
                'Pd': 4.7,  # 2.8**2*1.2 / 2
                'Pout': 92055,
                'Cjan': Cjan_value,
                'Cjancorr': Cjancorr_value,
                'Cpor': Cpor_value,
                'Cp_0': WindPressureCoefficientValues[node3]["wind_pressure_coefficient_value_"+str(wind_dir)],
                'Cp_1': WindPressureCoefficientValues['window_0_office_coef']["wind_pressure_coefficient_value_"+str(wind_dir)],
                'Cp_2': WindPressureCoefficientValues[node1]["wind_pressure_coefficient_value_"+str(wind_dir)],
                'Cp_3': WindPressureCoefficientValues['door_office_coef']["wind_pressure_coefficient_value_"+str(wind_dir)]
            })
        
        new_cp_values["wind_pressure_coefficient_value_"+str(wind_dir)] = float(const)
    
    WindPressureCoefficientValues['door_office_coef'] = new_cp_values
    
    return(WindPressureCoefficientValues)

'''
Cjan**2*CpN/(2*Cjan**2 + 3*Cjan*Cjancorr + 6*Cjan*Cpor + Cjancorr**2 + 5*Cjancorr*Cpor) + Cjan**2*CpS/(2*Cjan**2 + 3*Cjan*Cjancorr + 6*Cjan*Cpor + Cjancorr**2 + 5*Cjancorr*Cpor) +
3*Cjan*Cjancorr*CpN/(2*(2*Cjan**2 + 3*Cjan*Cjancorr + 6*Cjan*Cpor + Cjancorr**2 + 5*Cjancorr*Cpor)) + 3*Cjan*Cjancorr*CpS/(2*(2*Cjan**2 + 3*Cjan*Cjancorr + 6*Cjan*Cpor + Cjancorr**2 + 5*Cjancorr*Cpor)) + 
2*Cjan*CpE*Cpor/(2*Cjan**2 + 3*Cjan*Cjancorr + 6*Cjan*Cpor + Cjancorr**2 + 5*Cjancorr*Cpor) + Cjan*CpN*Cpor/(2*Cjan**2 + 3*Cjan*Cjancorr + 6*Cjan*Cpor + Cjancorr**2 + 5*Cjancorr*Cpor) + 
Cjan*CpS*Cpor/(2*Cjan**2 + 3*Cjan*Cjancorr + 6*Cjan*Cpor + Cjancorr**2 + 5*Cjancorr*Cpor) + 2*Cjan*CpW*Cpor/(2*Cjan**2 + 3*Cjan*Cjancorr + 6*Cjan*Cpor + Cjancorr**2 + 5*Cjancorr*Cpor) + 
Cjancorr**2*CpN/(2*(2*Cjan**2 + 3*Cjan*Cjancorr + 6*Cjan*Cpor + Cjancorr**2 + 5*Cjancorr*Cpor)) + Cjancorr**2*CpS/(2*(2*Cjan**2 + 3*Cjan*Cjancorr + 6*Cjan*Cpor + Cjancorr**2 + 5*Cjancorr*Cpor)) + 
3*Cjancorr*CpE*Cpor/(2*(2*Cjan**2 + 3*Cjan*Cjancorr + 6*Cjan*Cpor + Cjancorr**2 + 5*Cjancorr*Cpor)) + Cjancorr*CpN*Cpor/(2*Cjan**2 + 3*Cjan*Cjancorr + 6*Cjan*Cpor + Cjancorr**2 + 5*Cjancorr*Cpor) + 
Cjancorr*CpS*Cpor/(2*Cjan**2 + 3*Cjan*Cjancorr + 6*Cjan*Cpor + Cjancorr**2 + 5*Cjancorr*Cpor) + 3*Cjancorr*CpW*Cpor/(2*(2*Cjan**2 + 3*Cjan*Cjancorr + 6*Cjan*Cpor + Cjancorr**2 + 5*Cjancorr*Cpor))

2 janelas na extremidade:
0.294948366452572*CpE + 0.205051633547428*CpN + 0.205051633547428*CpS + 0.294948366452572*CpW

1 janela na extremidade:
0.447316104688964*CpE + 0.0526838953110364*CpN + 0.0526838953110364*CpS + 0.447316104688964*CpW

'''
