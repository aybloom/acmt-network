contains data from 2015 start to 2020 end.
contains 190 variables (kind of call 911)
e.g. when the estimate of variable "is_call911 Car Fire" is 1, it indicates a 911 call for car fire occurs at this lat/long.

911 calls: https://data.seattle.gov/Public-Safety/Seattle-Real-Time-Fire-911-Calls/kzjm-xkqj
```
    [1] "is_call911 Car Fire"                       "is_call911 Aid Response"                  
    [3] "is_call911 Unk Odor"                       "is_call911 Auto Fire Alarm"               
    [5] "is_call911 Triaged Incident"               "is_call911 Medic Response"                
    [7] "is_call911 Rescue Elevator"                "is_call911 Investigate Out Of Service"    
    [9] "is_call911 Medic Response- 7 per Rule"     "is_call911 Automatic Fire Alarm Resd"     
   [11] "is_call911 Automatic Medical Alarm"        "is_call911 AFA4 - Auto Alarm 2 + 1 + 1"   
   [13] "is_call911 Scenes Of Violence 7"           "is_call911 Aid Response Yellow"           
   [15] "is_call911 Electrical Problem"             "is_call911 Illegal Burn"                  
   [17] "is_call911 Bark Fire"                      "is_call911 Brush Fire"                    
   [19] "is_call911 Medic Response- 6 per Rule"     "is_call911 Trans to AMR"                  
   [21] "is_call911 1RED 1 Unit"                    "is_call911 Water Rescue Recon Response"   
   [23] "is_call911 MVI Freeway"                    "is_call911 Natural Gas Leak"              
   [25] "is_call911 MVI - Motor Vehicle Incident"   "is_call911 Water Job Minor"               
   [27] "is_call911 Fire in Building"               "is_call911 Natural Gas Odor"              
   [29] "is_call911 Automatic Fire Alarm False"     "is_call911 Activated CO Detector"         
   [31] "is_call911 Aid Response Freeway"           "is_call911 Dumpster Fire W/Exp."          
   [33] "is_call911 EVENT - Special Event"          "is_call911 Car Fire Freeway"              
   [35] "is_call911 Dumpster Fire"                  "is_call911 Low Acuity Response"           
   [37] "is_call911 4RED - 2 + 1 + 1"               "is_call911 Rubbish Fire"                  
   [39] "is_call911 Alarm Bell"                     "is_call911 Mutual Aid, Medic"             
   [41] "is_call911 Mutual Aid, Engine"             "is_call911 Mutual Aid, Task Force"        
   [43] "is_call911 Mutual Aid, Aid"                "is_call911 Rescue Heavy"                  
   [45] "is_call911 HAZADV - Hazmat Advised"        "is_call911 Boat Taking Water Minr/Sho"    
   [47] "is_call911 Mutual Aid, Strike Eng."        "is_call911 Mutual Aid, Ladder"            
   [49] "is_call911 Boat Under 50' Fire Shore"      "is_call911 Fire Response Freeway"         
   [51] "is_call911 Single Medic Unit"              "is_call911 Rescue Automobile"             
   [53] "is_call911 Medic Response, 6 per Rule"     "is_call911 Medic Response, 7 per Rule"    
   [55] "is_call911 Mutual Aid, Adv. Life"          "is_call911 Medic Response Freeway"        
   [57] "is_call911 Rescue Extrication"             "is_call911 Wires Down"                    
   [59] "is_call911 Chimney Fire"                   "is_call911 3RED - 1 +1 + 1"               
   [61] "is_call911 MVI Medic"                      "is_call911 FIREWATCH"                     
   [63] "is_call911 Food On The Stove"              "is_call911 Quick Dispatch Medic"          
   [65] "is_call911 Hang-Up, Aid"                   "is_call911 Quick Dispatch Medic 7"        
   [67] "is_call911 Motor Vehicle Incident"         "is_call911 Assault w/Weap 7 per Rule"     
   [69] "is_call911 Tranformer Fire"                "is_call911 Motor Vehicle Incident Freeway"
   [71] "is_call911 Rescue Lock In/Out"             "is_call911 Hang-Up, Fire"                 
   [73] "is_call911 Rescue Rope"                    "is_call911 Hazardous Mat, Spill-Leak"     
   [75] "is_call911 Food On The Stove Out"          "is_call911 Investigate In Service"        
   [77] "is_call911 Rescue Salt Water"              "is_call911 Fuel Spill"                    
   [79] "is_call911 LINK - Link Control Center"     "is_call911 Spill, Non-Hazmat"             
   [81] "is_call911 Rescue Fresh Water"             "is_call911 Fire in Single Family Res"     
   [83] "is_call911 Car Fire W/Exp."                "is_call911 COMED Poss Patient"            
   [85] "is_call911 Brush Fire Freeway"             "is_call911 Explosion Minor"               
   [87] "is_call911 Assault w/Weapons 14"           "is_call911 Aid Service"                   
   [89] "is_call911 Furnace Problem"                "is_call911 Tunnel Aid"                    
   [91] "is_call911 Aid Resp Infectious"            "is_call911 RMC Chief"                     
   [93] "is_call911 Water Job Major"                "is_call911 Monorail Fire on the beam"     
   [95] "is_call911 MVI Freeway Medic"              "is_call911 Hazardous Material w/Fire"     
   [97] "is_call911 Drill"                          "is_call911 Aircraft Crash"                
   [99] "is_call911 Multiple Medic Resp 14 Per"     "is_call911 Garage Fire"                   
  [101] "is_call911 HazMat Reduced"                 "is_call911 Vault Fire (Electrical)"       
  [103] "is_call911 Boat Under 50' Fire Water"      "is_call911 Multiple Casualty Incident"    
  [105] "is_call911 Help the Fire Fighter"          "is_call911 Mutual Aid, Hazmat"            
  [107] "is_call911 HazMat MCI"                     "is_call911 WAFA4 - Auto Alarm 2+1+1"      
  [109] "is_call911 Brush Fire W/Exp."              "is_call911 Ship Fire 50'on Shore/Pier"    
  [111] "is_call911 Pier Fire"                      "is_call911 Natural Gas Leak Major"        
  [113] "is_call911 Brush Fire Major"               "is_call911 Rescue Salt Water Maj"         
  [115] "is_call911 Shed Fire"                      "is_call911 Boat Under 50' Unknown"        
  [117] "is_call911 Rescue Fresh Water Maj"         "is_call911 Boat Taking on Water Major"    
  [119] "is_call911 Mutual Aid, Tech Res"           "is_call911 Rescue Trench"                 
  [121] "is_call911 ANTIB - Antibiotic Delivery"    "is_call911 Boat Fire In Marina"           
  [123] "is_call911 Rescue Confined Space"          "is_call911 Reduce Resp Opposite Tunnel"   
  [125] "is_call911 Rescue Standby"                 "is_call911 Rescue Standby Water"          
  [127] "is_call911 Fast Back Up"                   "is_call911 Tunnel Medic"                  
  [129] "is_call911 WAFA - Automatic Fire Alarm"    "is_call911 AFAH - Auto Alarm Hazmat"      
  [131] "is_call911 FIREWATCH - 1 Fire Unit"        "is_call911 Rescue Water"                  
  [133] "is_call911 Rescue Water Major"             "is_call911 Marine Recon Response"         
  [135] "is_call911 Marine Fire On Shore"           "is_call911 Water Rescue Recon"            
  [137] "is_call911 Vessel Sinking On Shore"        "is_call911 Vessel Sinking On Water"       
  [139] "is_call911 Marine Fire On Water"           "is_call911 Marine Service Response"       
  [141] "is_call911 Ship Fire 50' on Water"         "is_call911 Fire In Single Family Res"     
  [143] "is_call911 Water Rescue Response"          "is_call911 Water Rescue Response Major"   
  [145] "is_call911 Water Rescue Standby"           "is_call911 Explosion Major"               
  [147] "is_call911 Mutual Aid, Marine"             "is_call911 Rescue Heavy Major"            
  [149] "is_call911 Fire in a Highrise"             "is_call911 Medic Resp Infectious"         
  [151] "is_call911 Fire In A Highrise"             "is_call911 Hazardous Decon"               
  [153] "is_call911 Train Derailment wFireHzmt"     "is_call911 Mayday Incident"               
  [155] "is_call911 Referral To Agency"             "is_call911 Hang-Up- Aid"                  
  [157] "is_call911 Scenes Of Violence Aid"         "is_call911 TEST - MIS TEST"               
  [159] "is_call911 2RED - 1 + 1"                   "is_call911 Low Acuity Referral"           
  [161] "is_call911 Mutual Aid- Engine"             "is_call911 Mutual Aid- Medic"             
  [163] "is_call911 Scenes Of Violence 14"          "is_call911 Mutual Aid- Task Force"        
  [165] "is_call911 Tunnel MVI"                     "is_call911 Tunnel Standby"                
  [167] "is_call911 Mutual Aid- Ladder"             "is_call911 Hazardous Mat- Spill-Leak"     
  [169] "is_call911 Testing Only"                   "is_call911 Spill- Non-Hazmat"             
  [171] "is_call911 Mutual Aid- Aid"                "is_call911 Tanker Fire"                   
  [173] "is_call911 Hang-Up- Fire"                  "is_call911 Scenes Of Violence MCI"        
  [175] "is_call911 Hazmat Radiation"               "is_call911 Chempack Engine"               
  [177] "is_call911 Tank Farm"                      "is_call911 Mutual Aid- Tech Res"          
  [179] "is_call911 Tunnel North Ops Bldg"          "is_call911 Tunnel Car Fire"               
  [181] "is_call911 Tunnel Haz"                     "is_call911 Mutual Aid- Marine"            
  [183] "is_call911 Tunnel Rescue Standby"          "is_call911 Tunnel Full Response"          
  [185] "is_call911 Tunnel Rescue Extrication"      "is_call911 Mutual Aid, Aircraft"          
  [187] "is_call911 Rescue Ice"                     "is_call911 Aircraft Standby"              
  [189] "is_call911 Tunnel MVI Medic"               "is_call911 Tunnel Rescue"
```