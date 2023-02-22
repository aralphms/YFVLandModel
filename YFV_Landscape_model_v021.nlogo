extensions [ gis profiler ]

globals
[
  fragment-dataset                            ; the shapefile containing the forest fragments
  grid-dataset                                ; the shapefile containing the grid of the landscape

  biting-success                              ; daily mosquito biting success
  daily-survival-rate                         ; the daily survival probability for mosquitoes
  vertical-infection-rate                     ; the probability that an immature individual from the progeny of an infectious mosquito has been infected
  time-to-egg-maturation                      ; the time in days to blood fed mosquitoes digest the host blood and maturate their eggs
  eggs-by-gravid-mosquito                     ; the number of eggs a gravid mosquito will laid in the breeding site (only eggs that will become adult females)
  extrinsic-incubation-period                 ; the time in days the virus incubates in exposed mosquitoes
  latent-period                               ; the time in days the virus incubates in vertebrate hosts
  infectious-main-host-mortality              ; the mortality proportion for infectious main hosts
  main-host-viremic-period                    ; the viremic period in days for infectious main hosts
  mosquito-trans-competence                   ; the probability a infectious mosquito will infect a susceptible host during an interaction
  main-host-trans-competence                  ; the probability a infectious main host will infect a susceptible mosquito during an interaction
  alt-host-trans-competence                   ; the probability an infectious alternative host will infect a susceptible mosquito during an interaction
  alt-host-viremic-period                     ; the viremic period in days for infectious alternative hosts
  alt-host-mean-lifespan                      ; the mean lifespan for alternative hosts
  main-host-mean-lifespan                     ; the mean lifespan for howler monkeys
  minimum-development-time                    ; the minimum time needed to a immature become adult

  list-frag                                   ; list containing the number of patches represented by each node
  YFV-circ                                    ; the time in days the virus is circulating in the landscape
  maximum-viral-spread-speed                  ; the maximum value obtained for mean-viral-daily-spread-speed
  virus-invasion-speed                        ; list containing the calculated daily speeds (distance / time) of the virus to spread between two neighbor nodes (based only in the first viral emergence)
  total-main-hosts                            ; the total number of main hosts in the landscape

  speed-percolation                           ; parameter to measure the time the virus takes to travel half of the grid

;   mean-mosq-abun                             ; code to test the procedure (TestTimeEmergence)
;   count-ticks                                ; code to test the procedure (TestTimeEmergence)
;   time-to-emergence                          ; code to test the procedure (TestTimeEmergence)
;   test-link                                  ; code to test the procedure (general-test-compartment-transitions)
;   node-test                                  ; code to test the procedure (general-test-compartment-transitions)
;   node-test2                                 ; code to test the procedure (general-test-compartment-transitions)
]

;; nodes are imobile turtles containing the lists representing the populations of monkeys and hosts and transmission dynamics in a given area of the landscape
breed [nodes node ]

nodes-own
[
  my-node-id                  ; the id of the node
  my-fragment-id              ; the fragment number of the patches represented by the node
  my-landscape-unit-id        ; the id of the landscape unit where the node is located
  my-grid-size                ; the number of patches the node is representing
  my-patches                  ; agentset containing the patches that belong to the node
  mosq-list                   ; list containing the number of mosquitoes in different reproductive and epidemic states
  host-list                   ; list containing the number of hosts of diferent types and their epidemic states
  total-immatures             ; list containing the total number of immatures
  pot-infec-immatures         ; list containing potentially infected individuals from the total-immatures list
  dead-monkeys                ; number of dead monkeys in the node
  total-infec-individuals     ; total current number of exposed or infectious individuals in the node (sum of infected hosts and vectors)
  new-eggs                    ; the total number of eggs laid by gravid mosquitoes in a single day
  new-pot-infe-eggs           ; the total number of new potentially infected eggs
  active-transmission?        ; if the virus is circulating in the area the value is set to TRUE
  day-of-viral-emergence      ; the day the virus emerged in the node
  viral-introduction          ; number of new arrivals of infected mosquitoes or hosts from other nodes
  external-node?              ; if node is considered external to landscape
  prob-find-host              ; daily probability of a blood seeking mosquito finding a host
  prob-find-breed             ; daily probability of a gravid mosquito finding a breeding site

  alpha                       ; base coefficient in the equation of adult mosquitoes carrying capacity (current-K)
  beta                        ; trend coefficient in the equation of current-K
  gamma                       ; seasonal coefficient in the equation of current-K
  current-k                   ; the current carrying capacity for mosquito population in the node
  k-max                       ; the initial mosquito abundance and maximum value of current-k

  A                           ; the parameter β0 of the logistic model that controls the larval density (Eq2.4 in the ODD protocol)
  B                           ; the parameter β1 of the logistic model that controls the larval density (Eq2.3 in the ODD protocol)
]

links-own                     ; links allow the exchange of individuals and information between two nodes (individuals are represented as values in the lists)
[
 minimum-distance             ; the minimum distance between the patches represented by the linked nodes
 nodes-distance               ; the distance between the nodes of the link (the nodes are located in the geographic center of their area of influence)
 dispersal-rate-end1          ; the proportion of mosquitoes that can disperse from end1 to end2
 dispersal-rate-end2          ; the proportion of mosquitoes that can disperse from end2 to end1

 host-movement-rate-end1      ; the proportion of hosts that can move from end1 to end2 (the neighbor nodes need be part of the same fragment or adjacent fragments (<= 2 patch in distance)
 host-movement-rate-end2      ; the proportion of hosts that can move from end2 to end1 (the neighbor nodes need be part of the same fragment or adjacent fragments (<= 2 patch in distance)
 outer-link?                  ; if the link includes at least one external node
]

patches-own                   ; patches represent the spatial (discrete) units of the landscape
[
  node-id                     ; the id of the node that represent the area where the patch is located
  landscape-unit-id           ; the id of the landscape unit where the patch is located
  fragment-id                 ; the id of the fragment where the patch is located
  patch-type                  ; if the patch is forest or non-forest
]

to setup
  clear-all
  if seed?     ; if on, a seed can be chosen
  [
   random-seed my-seed
  ]
  ask patches [set patch-type "non-forest" set pcolor white]

  ;; including shapefile and configuring patches variables
  (ifelse landscape = "1"
    [
      set fragment-dataset gis:load-dataset "Layers_model/LandscapeSP1.shp"   ; the shapefile containing the landscape with the forest fragments
      set grid-dataset gis:load-dataset "Layers_model/GridLandSP1.shp"  ; the grid shapefile dividing the landscape in several areas of the same size (landscape units)
    ]
    landscape = "2" [set fragment-dataset gis:load-dataset "Layers_model/LandscapeSP2.shp" set grid-dataset gis:load-dataset "Layers_model/GridLandSP2.shp"]
    landscape = "3" [set fragment-dataset gis:load-dataset "Layers_model/LandscapeSP3.shp" set grid-dataset gis:load-dataset "Layers_model/GridLandSP3.shp"])
  gis:set-world-envelope (gis:envelope-union-of (gis:envelope-of fragment-dataset) (gis:envelope-of grid-dataset))
  gis:set-drawing-color gray
; gis:draw fragment-dataset 0.5
  gis:draw grid-dataset 0.5
  gis:set-coverage-maximum-threshold 0.01  ; applying a low threshold to avoid weighted id values for patches
  gis:apply-coverage fragment-dataset "ID" fragment-id ; assign the ID number of forest fragments to patches
  gis:apply-coverage grid-dataset "GRIDNUM" landscape-unit-id ; assign the ID number of landscape units to patches
  ask patches [if fragment-id > 0 [set pcolor green set patch-type "forest" ]] ; assign green color for forest patches

  ;; assigning parameter values
  set biting-success 0.5                  ; the biting success of blood-seeking mosquitoes (assumed be 50% during an interaction with a host)
  set daily-survival-rate 0.93            ; the fitted value for the daily survival probability of a vector mosquito (Haemagogus species in this case)
  set vertical-infection-rate 0.01        ; the assumed probability for transovarial infection of an infectious mosquito to its progeny
  set time-to-egg-maturation 5            ; the average time (in days) a blood fed mosquito will take to maturate its eggs
  set eggs-by-gravid-mosquito 15          ; the number of eggs a gravid mosquito will laid in the breeding site (only eggs that will become adult females)
  set extrinsic-incubation-period 12      ; the average time (in days) an infected mosquito will take to be able to transmit the virus to a host
  set latent-period 3                     ; the average time (in days) an infected host will take before become viremic
  set infectious-main-host-mortality 0.9  ; the assumed probability of a main host (howler monkeys in this case) die after be infected by the virus
  set main-host-viremic-period 5          ; the average period of infectiousness for the main hosts
  set alt-host-viremic-period 5           ; the average period of infectiousness for the alternative hosts
  set mosquito-trans-competence 0.25      ; the assumed probability that an infectious mosquito will transmit the virus to a susceptible host after a successful interaction
  set main-host-trans-competence 0.6      ; the assumed probability that an infectious main host will transmit the virus to a susceptible mosquito after a successful interaction
  set alt-host-trans-competence 0.2       ; the assumed probability that an infectious alternative host will transmit the virus to a susceptible mosquito after a successful interaction
  set alt-host-mean-lifespan 3650         ; the assumed average time of life (in days) for an alternative host
  set main-host-mean-lifespan 5475        ; the average time of life (in days) for a main host
  set minimum-development-time 12         ; the minimum time needed to a immature become adult

  create-the-nodes                        ; creating the nodes that will hold the lists
  create-node-links                       ; creating the links between nodes

  define-external-area                    ; defining the area and nodes that will be external to the landscape
  create-mosq-lists                       ; creating the list for mosquitoes
  create-host-lists                       ; creating the list for vertebrate hosts
  create-breeding-site-lists              ; creating the lists with immature information
  larval-density-parameters               ; assign the larval density parameters

  set total-main-hosts sum [item 0 item 0 host-list] of nodes with [external-node? = false]  ; the total number of main hosts in the landscape (accounting only internal nodes)
  set YFV-circ 0                 ; the time (in days) the virus is circulating in the landscape
  set virus-invasion-speed []    ; the list containing information about the daily emergence speed of the virus between the linked nodes
  current-k-parameters           ; the parameters to obtaining the current-k (referential value representing a seasonal carry capacity for mosquito density)

  set speed-percolation 0        ; parameter to measure the time the virus takes to travel half of the grid

;  set time-to-emergence []  ; code to test the procedure (TestTimeEmergence)
;  set mean-mosq-abun []     ; code to test the procedure (TestTimeEmergence)
;  test-compartment-transitions  ; code to test the procedures (general-test-compartment-transitions)

  reset-ticks
end

to create-the-nodes
  with-local-randomness [ random-seed 1  ; since the function to merge nodes can generate different patterns on each run, it is applied a local seed to avoid these variations in the nodes layouts
  let maxgr max [landscape-unit-id] of patches    ; the maximum id of the landscape-unit shapefile
  let list-grid n-values maxgr [x -> x + 1] ; creating a list of grid id's
  let nd 1 ; local variable for the node number whitin a grid cell
;  let list-for-x [] ; code to test the procedure
;  let list-for-mylist [] ; code to test the procedure
;  let list-for-nd []   ; code to test the procedure
  foreach list-grid [x -> let mylist [] let my-ptch patches with [patch-type = "forest" and landscape-unit-id = x]  ; creating an agentset for forest patches inside a landscape unit
    ask my-ptch [let num fragment-id set mylist fput num mylist] set mylist remove-duplicates mylist  ; indentifying different fragments in the same grid area
    foreach mylist [y -> ask my-ptch with [fragment-id = y] [set node-id nd] ; assign the number of the node the patch will represent in the landscape
;      set list-for-x lput x list-for-x set list-for-mylist lput mylist list-for-mylist set list-for-nd lput nd list-for-nd ; code to test the procedure
      set nd nd + 1] ]

  let maxnd max [node-id] of patches with [patch-type = "forest"]    ; How many nodes the landscape will need
  set list-frag n-values maxnd [x -> x + 1]  ; list containing the nodes number
;  let list-for-frag-id [] foreach list-frag [x -> let id-fg [fragment-id] of one-of patches with [node-id = x] set list-for-frag-id lput id-fg list-for-frag-id] ; code-test
  ;; create a node for each item of the list-frag
  foreach list-frag [x -> let xmean mean [pxcor] of patches with [node-id = x] let ymean mean [pycor] of patches with [node-id = x] ;calculate the local geographical center
    let id-fg [fragment-id] of one-of patches with [node-id = x]  ; obtaining the id of the fragment where the patches with my-node = x are located
    create-nodes 1 ;create a node and assign it state variable values
    [
      setxy xmean ymean
      set shape "circle"
      set color black
      set my-node-id x
      ;set label x
      set label-color black
      set my-fragment-id id-fg
      set my-patches patches with [node-id = x]
      set active-transmission? false
      set external-node? false
      set day-of-viral-emergence "no"
      set viral-introduction 0
      set prob-find-host 1
      set prob-find-breed 1  ; assuming abundant and well distributed breedings in the fragment
    ]]

  set list-frag map [x -> count patches with [node-id = x]] list-frag  ; modify list-frag to show how many patches are represented by each node
  ; assign for each node the size (in patches) it represents
  ask nodes [set my-grid-size item (my-node-id - 1) list-frag]

  ; to reduce the number of nodes in the landscape and thus the computational demand,
  ; the next function will select nodes representing a small area and join it with some neighboring node of the same fragment (if any)
  let small-size-cl nodes with [my-grid-size <= maximum-size-to-merge]   ; the procedure will be applied for nodes with less than maximum-size-to-merge patches
;  let list-of-merged-nodes [] ; code to test the procedure
  if any? small-size-cl
  [
    ask small-size-cl
    [
      let val1 my-node-id   ; the id of the node
      let val2 my-fragment-id       ; the fragment-id of the patches represented by the node
      let val3 my-grid-size  ; the size (in patches) represented by the node
      let my-neig 0  ; local variable that will store an agentset of patches
      let val4 0     ; local variable that will store the number of a node
      let list-targ-patch [] ; the list that will store selected patches
      ask my-patches
      [
        set my-neig neighbors with [node-id != val1 and fragment-id = val2] ; the neighbor need to be of the same fragment but represent a different node
        if any? my-neig [let one-neig one-of my-neig set list-targ-patch fput one-neig list-targ-patch] ; each patch that found at least one patch with the above criteria store the information in the list-targ-patch
      ]
      if not empty? list-targ-patch
      [
        let targ-patch first list-targ-patch ; only one item of the list-targ-patch is necessary, then we can chose the first
        ask targ-patch [let num-nd node-id set val4 num-nd] ; this selected patch will inform the number of the node it represent
        ask my-patches [set node-id val4]
        let val5 val4 - 1
        ask node val5 ; accessing the new node (since netlogo turtle numbers starts at 0 and nodes id starts at 1, the correct number to call is always node - 1)
        [
          set my-grid-size my-grid-size + val3       ; adding the new patches to the size area represented by the node
          set my-patches patches with [node-id = val4]  ; recreate my-patches agentset containing the new patches added
          let xmean mean [pxcor] of my-patches let ymean mean [pycor] of my-patches setxy xmean ymean ; move the node to the new geographical center of the area it represents
        ]
;        let merged-nodes [] set merged-nodes lput (val1 - 1) merged-nodes set merged-nodes lput val5 merged-nodes ; code to test the procedure
;        set list-of-merged-nodes lput merged-nodes list-of-merged-nodes set list-of-merged-nodes lput [my-grid-size] of node val5 list-of-merged-nodes  ; code to test the procedure
        die   ;now that the small node's patches have joined to its neighbor node, the node will die.
      ]
    ]
  ]
    ask nodes [set my-landscape-unit-id [landscape-unit-id] of patch-here]]  ; assigning the landscape-unit-id for the nodes
;; outputs to procedure verification
;  ;file-delete "code-test_create-the-nodes.txt"
;  file-open "code-test_create-the-nodes.txt"
;  file-type "maxgr " file-print maxgr
;  file-type "list-grid " file-print list-grid
;  file-type "list-for-x " file-print list-for-x
;  file-type "list-for-mylist " file-print list-for-mylist
;  file-type "list-for-nd " file-print list-for-nd
;  file-type "maxnd " file-print maxnd
;  file-type "list-for-who-id " file-print list-for-who-id
;  file-type "list-for-frag-id-of-nd " file-print list-for-frag-id
;  file-type "list-frag-size-nd " file-print list-frag
;  file-type "maximum-size-to-merge " file-print maximum-size-to-merge
;  file-type "list-of-merged-nodes " file-print list-of-merged-nodes
;  file-close
end

to define-external-area
  ask patches with [pxcor = min-pxcor or pxcor = max-pxcor or pycor = min-pycor or pycor = max-pycor] ; calling all the patches in the limits of the grid
  [
    if external-area > 0.5 ;avoiding runtime error message
    [
      ; since the distance is calculated from the center of the patch, the value 0.5 need to be subtracted
      ask nodes in-radius (external-area - 0.5) [set external-node? true set color blue] ;external nodes are identified with the color blue in the grid
    ]
  ]
end

to create-mosq-lists
  ;;Mosq-list -> item 0 = susceptible; item 1 = exposed; item 2 = infectious
  ;;Elements -> 0 = blood-seeking; 1 = blood-fed; 2 = gravid; 3 = newly-emerged
  ask nodes
  [
    ;multiply the number of mosquitoes per patch (each patch representing 1 ha) per number of patches represented by the node
    let n-mosq-list map [x -> round ((initial-mosquitoes-per-ha * my-grid-size) / 4)] [0 0 0 0] ; mosquitoes are shared equally in the susceptible compartment (item 0)
    set mosq-list [[] [0 0 0] [0 0 0 0]]
    set mosq-list replace-item 0 mosq-list n-mosq-list
    set k-max sum item 0 mosq-list
  ]
end

to create-host-lists
  ;;Lists -> item 0 = susceptible; item 1 = exposed; item 2 = infectious; item 3 = recovered and immune or dead-end
  ;;Elements -> 0 = main host; 1 = alternative host; 2 = dead-end host
  ask nodes
  [
    set host-list [[0 0] [0 0] [0 0] [0 0 0]]
    if my-grid-size >= 10  ; only nodes with ten or more hectares in size will support a population of the main host (howler monkeys)
    [
      set host-list (replace-item 0 host-list
                  (replace-item 0 (item 0 host-list) (round (main-host-per-ha * my-grid-size)))) ;the number of hosts per patch is multiplied by number of patches represented by the node
    ]
    set host-list (replace-item 0 host-list
                  (replace-item 1 (item 0 host-list) (round (alternative-host-per-ha * my-grid-size))))
    set host-list (replace-item 3 host-list
                  (replace-item 2 (item 3 host-list) (round (dead-end-host-per-ha * my-grid-size))))
  ]
end

to create-breeding-site-lists
  ;; including the initial number of immatures in the breeding site
  ;; each item of the list represent a day of the immature development in increasing order
  ;; the number 7.5 represents the assumed initial number of immatures per adult mosquitoes. This number also represents the density where the survival probability is 50% in the breeding site
  ;; the immature-list starts with 12 items (=minimum-development-time) representing the minimum time required for the immature development from egg to adult
  ask nodes
  [
    ; each element of total-immatures list represents a day of immatures development, the list can increase but never will have less than minimum-development-time elements
    set total-immatures n-values minimum-development-time [x -> 0]
    set pot-infec-immatures n-values minimum-development-time [x -> 0] ; this list reflects the total immature list, but only counts immature that can be infected
    ; we assume an initial number of immature 7.5 times fold the adult mosquitoes, the total is divided for each item of the immature lists
    let init-larv-num round ((7.5 * k-max) / minimum-development-time)
    set total-immatures map [x -> init-larv-num] total-immatures
  ]
end

to create-node-links
  ;;each node will create a link with nodes situated in its dispersion radius
;  let neigh-nodes-and-links [] ; code to test the procedure
  ask nodes
  [
    create-links-with other nodes in-radius maximum-distance-link
;    set neigh-nodes-and-links lput who neigh-nodes-and-links ; code to test the procedure
;    set neigh-nodes-and-links lput [who] of other nodes in-radius maximum-distance-link neigh-nodes-and-links ; code to test the procedure
;    set neigh-nodes-and-links lput my-links neigh-nodes-and-links ; code to test the procedure
  ]

;   calculating the minimum distance between two linked areas
  ask links
  [
    let dist-list []    ; creating a list to store the information about minimum distances provided by patches
    let ref-patches 0
    let id1 [my-node-id] of end1
    let id2 [my-node-id] of end2
    ask end1
    [
      set ref-patches patches with [node-id = id2] in-radius maximum-distance-link ; end1 detect the patches of end2 and create an agentset called ref-patches
      ask ref-patches ; each ref-patches will calculate the minimum distance to end1 and store this information in the dist-list
      [
        let px pxcor
        let py pycor
        let tg [distancexy px py] of (min-one-of patches with [node-id = id1] [distance myself])
        set dist-list lput tg dist-list
      ]
    ]
     set minimum-distance min dist-list   ; the minimum distance is the minimum value stored in the dist-list
  ]

  ; creating the link variables
  ask links
  [
    let target end1
    let dist 0
    ask end2 [set dist distance target] ; getting the distance between nodes
    set nodes-distance dist ; store dist in the link variable 'nodes-distance'

    let size-end1 [my-grid-size] of end1
    let size-end2 [my-grid-size] of end2

    let size-landscape-grid (count patches with [landscape-unit-id = 1]) ; the total number of patches per landscape unit (1 km2) - all landscape grids have the same size
    let prop-size1 size-end1 / size-landscape-grid ; the proportional size of end 1 in relation to the landscape grid
    if prop-size1 > 1 [set prop-size1 1]           ; in some situations merged nodes may have large areas than their landscape grid
    let prop-size2 size-end2 / size-landscape-grid
    if prop-size2 > 1 [set prop-size2 1]

    ;; The dispersion rate is modelled here as function of the inverse of the minimum-distance, the parameter for maximum dispersal/movement and the proportional size of the node
    set dispersal-rate-end1 (1 / minimum-distance) * max-prop-mosq-dispersal * prop-size2 ; Eq. 14 in the ODD protocol
    set dispersal-rate-end2 (1 / minimum-distance) * max-prop-mosq-dispersal * prop-size1 ; Eq. 15 in the ODD protocol

    if minimum-distance <= 2 ; only linked nodes with minimum-distance less than 2 patches will allow movement of hosts
    [
      set host-movement-rate-end1 (1 / minimum-distance) * host-move-proportion * prop-size2
      set host-movement-rate-end2 (1 / minimum-distance) * host-move-proportion * prop-size1
    ]
    ifelse any? both-ends with [external-node?] [set outer-link? true][set outer-link? false] ; defining if the link is external or internal
;    ;; outputs to check the procedure
;    file-open "code-test_create-node-links.txt"
;    file-type "who-end1 " file-print [who] of end1
;    file-type "size-end1 " file-print size-end1
;    file-type "prop-size1 " file-print prop-size1
;    file-type "who-end2 " file-print [who] of end2
;    file-type "size-end2 " file-print size-end2
;    file-type "prop-size2 " file-print prop-size2
;    file-type "minimum-distance " file-print minimum-distance
;    file-type "dispersion-rate-end1 " file-print dispersion-rate-end1
;    file-type "dispersion-rate-end2 " file-print dispersion-rate-end2
;    file-close
  ]

;  ;; outputs to check the procedure
;  ;file-delete "code-test_create-node-links.txt"
;  file-open "code-test_create-node-links.txt"
;  file-type "neigh-nodes-and-links " file-print neigh-nodes-and-links
;  file-close
end

 ;by assign parameter values considering individual breeding sites it is calculated the larval density parametes for the 'super-breeding-site' (that represents all individual breeding sites in the area)
 ;the larval density is controlled by a logistic function
to larval-density-parameters
  ask nodes
  [
    let sv1 0.99     ; probability of 0.99
    let sv2 0.5      ; probability of 0.5

    let max-ab-cap k-max if max-ab-cap = 0 [set max-ab-cap 1] ; create a local variable to avoid possible division by zero when calculate the parameters
    let lv1 1 * max-ab-cap  ; the number of immatures whose probability of survival is 99%
    let lv2 7.5 * max-ab-cap   ; the number of immatures whose probability of survival is 50%

    let D ln (sv1 / (1 - sv1))      ; equation 2.1 in the ODD
    let C ln (sv2 / (1 - sv2))      ; equation 2.2 in the ODD
    set B (D - C) / (lv1 - lv2)     ; the β1 of the logistic model that controls larval density  (equation 2.3 in the ODD)
    set A D - B * lv1               ; the β0 of the logistic model that controls larval density  (equation 2.4 in the ODD)

    ; outputs to check the procedure (test for larval density)
;    if who = min [who] of nodes                   ;code to test the procedure (test for larval density)
;    [
;      ;file-delete "test_for_larval_density.txt"
;      file-open "test_for_larval_density.txt"
;      file-type "k-max " file-print max-ab-cap
;      file-type "lv1 " file-print lv1
;      file-type "lv2 " file-print lv2
;      file-type "Eq2.1 " file-print Eq2.1
;      file-type "Eq2.2 " file-print Eq2.2
;      file-type "Eq2.3 " file-print Eq2.3
;      file-type "Eq2.4 " file-print Eq2.4
;      file-close
;    ]
  ]

end

; obtaining the parameters to simulate the seasonal variance in the environmnet by limiting the abundance of mosquitoes
to current-k-parameters
  ask nodes
  [
    set current-k k-max

    let k-min (k-max / 6)  ;minimum value of current-K  (six time lower than the maximum value) -> assumed the lower abundance limit of mosquitoes during the year
    set alpha (k-max + k-min) / 2 ; base coefficient
    set beta (-(k-max - k-min) / (365 / 2)) ; trend coefficient
    set gamma (alpha - k-max) ; seasonal coefficient
  ]
end


;; initial configuration for a general check for compartment transitions
;; the code to test each transition is out commented in the respective procedures
;to test-compartment-transitions  ; code to test the procedure (general-test-compartment-transitions)
;  set test-link one-of links
;  ask test-link [set color red]
;  ask links with [color != red] [die]
;  set node-test [end1] of test-link
;  set node-test2 [end2] of test-link
;  set start-YFV-circulation -1
;  ask node-test [set mosq-list replace-item 1 mosq-list [100 100 100] set mosq-list replace-item 2 mosq-list [100 100 100 0]
;    set host-list replace-item 1 host-list [10 10] set host-list replace-item 2 host-list [10 10] set active-transmission? true
;    set day-of-viral-emergence 0 set viral-introduction viral-introduction + 1 ask my-patches [set pcolor red]]
;  file-open "general-test-compartment-transitions.txt"
;  file-print "General Test for Compartment Transitions"
;  file-print " " if seed? [file-type "Seed " file-print my-seed]
;  file-type "node-test who " file-print [who] of node-test
;  file-type "node-test2 who " file-print [who] of node-test2
;  file-type "mosq-list node-test " file-print [mosq-list] of node-test
;  file-type "host-list node-test " file-print [host-list] of node-test
;  file-type "immature-list node-test " file-print [total-immatures] of node-test
;  file-type "pot-infec-immatures node-test " file-print [pot-infec-immatures] of node-test
;  file-close
;end

;####################################################################################################################################################
;##################################################################################################################################################
; GO PROCEDURES #################################################################################################################################

to go
  tick
  update-the-current-mosquito-capacity      ;updates the variable that controls the seasonal density of adult mosquitoes
  check-larval-density                      ;updates the daily survival parameter and the number of immatures in the breeding site
  eliminate-adult-mosquitoes                ;applies a mortality rate for each compartment of the mosquito adult population in the node
  from-newly-emerged-to-blood-seeking       ;adult mosquitoes in the newly-emerged compartment change to the blood-seeking compartment
  mosquito-adult-emergence                  ;the newly-emerged compartment receives new adults emerging from the immature stage
  from-gravid-to-blood-seeking              ;gravid/pregnant mosquitoes lay their eggs and change from gravid to the blood-seeking compartment
  from-blood-fed-to-gravid                  ;a number of blood-fed mosquitoes change to the gravid compartment
  from-exposed-to-infectious                ;the exposed mosquitoes that reach the infectious stage change to the infectious compartment.

  vector-host-interaction                   ;blood-seeking mosquitoes that performed a successful bite on the hosts will change to the blood-fed compartment.
                                            ;The susceptible mosquitoes or hosts that become infected by the virus change to the exposed compartment

  mosquitoes-dispersal                      ;the exchange between mosquitoes of linked nodes is performed, simulating the dispersal of mosquitoes
  host-movement                             ;the exchange between hosts from linked nodes of a same (or close) fragment is performed, simulating host movement
  host-recovery-or-death                    ;checks the infectious compartments of hosts and selects a number of them to be removed (die) or move to the immune compartment
  population-dynamics-of-host               ;applies natural mortality and new births for the hosts
  insert-the-virus                          ;at a given moment infectious mosquitoes are inserted in one or more external nodes to start the circulation of the virus in the landscape
  calc-the-prob-of-finding-a-host           ;calculates the probability of blood-seeking mosquitoes finding a host in the next time step
  check-viral-circulation                   ;check if the virus is circulating in the mosquito or host populations
  if ticks = 1 [ask links [hide-link]]

;  set mean-mosq-abun fput total-mosquitoes mean-mosq-abun    ; code to test the procedure (TestTimeEmergence)
;  set count-ticks count-ticks + 1         ; code to test the procedure (TestTimeEmergence)
;  if ticks = 1
;  [
;    file-open "TestTimeEmergence.txt"       ; code to test the procedure (TestTimeEmergence)
;    file-type "mean-abund,"                ; code to test the procedure (TestTimeEmergence)
;    file-type "mean-emergence-time,"       ; code to test the procedure (TestTimeEmergence)
;    file-type "sd-emergence-time,"         ; code to test the procedure (TestTimeEmergence)
;    file-print "max-emergence-time,"       ; code to test the procedure (TestTimeEmergence)
;    file-close                              ; code to test the procedure (TestTimeEmergence)
;  ]
;  if count-ticks = 30 [test-time-egg-to-adult]     ; code to test the procedure (TestTimeEmergence)
;  if ticks = 720 [stop]                            ; code to test the procedure (TestTimeEmergence)

   ;; the code used to follow the progress during simulations with NLRX
;  if ticks > start-YFV-circulation and all? nodes [active-transmission? = false] [file-open "progress.txt" file-print my_status_simulation file-close stop]
;  if ticks = 1825 [file-open "progress.txt" file-print my_status_simulation file-close stop]

;  ;; stop conditions
  if ticks > start-YFV-circulation and all? nodes [active-transmission? = false] [stop]
  if ticks = 1825 [stop]
end

; procedure to test and calibrate time from egg to adult in mosquitoes
;to test-time-egg-to-adult                             ; code to test the procedure (TestTimeEmergence)
;  file-open "TestTimeEmergence.txt"
;  file-type (word mean-abund ",")
;  file-type (word mean-emergence-time ",")
;  file-type (word sd-emergence-time ",")
;  file-print max-emergence-time
;  file-close
;  set count-ticks 0
;  set mean-mosq-abun []
;  set time-to-emergence []
;end


;;Updating the current-mosquito-capacity to controlling the abundance of adult mosquitoes
;;this procedure simulates the annual seasonal changes of the environment
to update-the-current-mosquito-capacity
  ask nodes
  [
    let time 0.5 + (ticks / 365) ; to make the current-K start in the maximum value we add 0.5 to time
    ;the mosquito carrying capacity will vary according to the time creating seasonal cycles
    ; Eq. 1 in the ODD protocol
    set current-k  (alpha + beta * sin (2 * pi * time * (180 / pi)) + gamma * cos (2 * pi * time * (180 / pi)))
  ]
end

;;updates the daily survival parameter and the number of immatures in the breeding site
to check-larval-density
  ask nodes with [not empty? total-immatures and sum total-immatures != 0]
  [
    let tot-immat sum total-immatures
    let Z exp (A + (B * tot-immat))   ; the odds of the logistic model (Equation 2.5 in the ODD)

;    let pre-tot-immat total-immatures          ;code to test the procedure (test for larval density)
;    let pre-pot-inf-immat pot-infec-immatures  ;code to test the procedure (test for larval density)

    ; calculating the daily survival probability for immatures
    let P (Z / (1 + Z))     ; Eq. 2.6 in the ODD protocol
    set total-immatures map [x -> round (x * P)] total-immatures ; creating the new list of total-immatures
    set pot-infec-immatures map [x -> round (x * P)] pot-infec-immatures ; creating the new list of possible infected immatures

;    ; code to check the procedure (test for larval density)
;    if who = min [who] of nodes                   ;code to test the procedure (test for larval density)
;    [
;      ;file-delete "test_for_larval_density.txt"
;      file-open "test_for_larval_density.txt"
;      file-type "time-step " file-print ticks
;      file-type "pre-list total-immatures " file-print pre-tot-immat
;      file-type "pre-list pot-infec-immatures " file-print pre-pot-inf-immat
;      file-type "tot-immat " file-print tot-immat
;      file-type "Z " file-print Z
;      file-type "P " file-print P
;      file-type "pos-list total-immatures " file-print total-immatures
;      file-type "pos-list pot-infec-immatures " file-print pot-infec-immatures
;      file-close
;    ]

  ]
end

to eliminate-adult-mosquitoes
  ; calculating the finite mortality rate from the daily survival rate
  let mosquito-mortality-rate (1 - daily-survival-rate)  ; Eq. 3 in the ODD protocol (S = daily-survival-rate and µ = mosquito-mortality-rate)

;  ;code to test the procedure (general-test-compartment-transitions)
;  ask node-test ;code to test the procedure (general-test-compartment-transitions)
;  [
;    file-open "general-test-compartment-transitions.txt"
;    file-print " " file-print "#procedure eliminate-adult-mosquitoes: " file-type "#time-step " file-print ticks
;    file-type "mosquito-mortality-rate " file-print mosquito-mortality-rate
;    file-type "pre-mosq-list " file-print mosq-list
;    file-close
;  ]

  ask nodes
  [
    let mort-mosq-list []  ; create a local list
    ;calculating the number of mosquitoes to be eliminated from each item and store in the mort-mosq-list
    set mort-mosq-list (map [[x] -> (map [[y] -> y * mosquito-mortality-rate] x)] mosq-list)
    ;apllying a subtraction operation between lists to eliminate mosquitoes from the mosq-list. The constant -0.5 ensure the compartment goes to 0 when all adult mosquitoes die.
    set mosq-list (map [[x1 x2] -> (map [[y1 y2] -> round (y1 - y2 - 0.5)] x1 x2)] mosq-list mort-mosq-list)

;    if who = [who] of node-test  ;code to test the procedure (general-test-compartment-transitions)
;    [
;      file-open "general-test-compartment-transitions.txt"
;      file-type "mort-mosq-list " file-print mort-mosq-list
;      file-type "new-mosq-list " file-print mosq-list
;      file-close
;    ]
  ]
end

;It is assume that all newly-emerged mosquitoes from the previous time step are ready to move to blood-seeking status in the current time step
;(parameter time to start blood-seeking = 1 -> see table 3 ODD)
to from-newly-emerged-to-blood-seeking
  ask nodes
  [
    let susc-newly item 3 item 0 mosq-list ;; counting current number of mosquitoes in susceptible and newly emerged status
    let infe-newly item 3 item 2 mosq-list ;; counting current number of mosquitoes in infectious and newly emerged status

    let susc-bs item 0 item 0 mosq-list  ;; counting current number of mosquitoes in susceptible and blood-seeking status
    let infe-bs item 0 item 2 mosq-list  ;; counting current number of mosquitoes in infectious and blood-seeking status
    ; transition 1 (see ODD protocol)
    let new-susc-newly susc-newly - susc-newly
    let new-susc-bs susc-bs + susc-newly
    ;; attributing the new values for the susceptible compartments of mosquito list
    set mosq-list (replace-item 0 mosq-list
                  (replace-item 3 (item 0 mosq-list) new-susc-newly))
    set mosq-list (replace-item 0 mosq-list
                  (replace-item 0 (item 0 mosq-list) new-susc-bs))
    ; transition 2 (see ODD protocol)
    let new-infe-newly infe-newly - infe-newly
    let new-infe-bs infe-bs + infe-newly
    ;; attributing the new values for the infectious compartments of mosquito list
    set mosq-list (replace-item 2 mosq-list
                  (replace-item 3 (item 2 mosq-list) new-infe-newly))
    set mosq-list (replace-item 2 mosq-list
                  (replace-item 0 (item 2 mosq-list) new-infe-bs))

;    if who = [who] of node-test  ; code to test the procedure (general-test-compartment-transitions)
;    [
;      file-open "general-test-compartment-transitions.txt"
;      file-print " " file-print "#procedure from-newly-emerged-to-blood-seeking: "
;      file-type "#time-step " file-print ticks file-print " "
;      file-print "Transition 1"
;      file-type "susceptible newly-emerged " file-print susc-newly
;      file-type "new susceptible blood-seeking " file-print new-susc-bs
;      file-print " " file-print "Transition 2"
;      file-type "infectious newly-emerged " file-print infe-newly
;      file-type "new infectious blood-seeking " file-print new-infe-bs
;      file-print " " file-type "mosq-list " file-print mosq-list
;      file-close
;    ]
  ]
end

to mosquito-adult-emergence
  ask nodes
  [
    let tot-mosq sum item 0 mosq-list + sum item 1 mosq-list + sum item 2 mosq-list  ; obtaining the current abundance of adult mosquitoes
    if tot-mosq < current-k            ; check if abundance is lower than current-K. If so, new adults can emerge from the super-breeding-site
    [
      let needed-mosq round (current-k - tot-mosq)  ; calculating how many individuals are needed to reach the carrying capacity
      let num-emerged-mosq 0                        ; local variable to store the number of newly emerged
      let num-emerged-infec-mosq 0                  ; local variable to store the number of potentially infectious newly emerged

;      if who = [who] of node-test  ; code to test the procedure (general-test-compartment-transitions)
;      [
;        file-open "general-test-compartment-transitions.txt"
;        file-print " " file-print "#procedure mosquito-adult-emergence: "
;        file-type "#time-step " file-print ticks
;        file-type "pre mosquito list " file-print mosq-list
;        file-type "total mosquitoes " file-print tot-mosq
;        file-type "current-k " file-print current-k
;        file-type "needed adult mosquitoes " file-print needed-mosq
;        file-type "pre total immatures list " file-print total-immatures
;        ;file-close
;       ]

      while [num-emerged-mosq < needed-mosq and length total-immatures >= minimum-development-time] ; repeat the procedure as long as the conditions are true
      [
        set num-emerged-mosq num-emerged-mosq + last total-immatures ; add the value of the last item of total-immatures list
        set num-emerged-infec-mosq num-emerged-infec-mosq + last pot-infec-immatures ; add the value of the last item of pot-infec-immatures list

;        set time-to-emergence fput length total-immatures time-to-emergence  ; code to test the procedure (TestTimeEmergence)

        set total-immatures but-last total-immatures                 ; remove the last item of total-immatures list
        set pot-infec-immatures but-last pot-infec-immatures   ; remove the last item of pot-infec-immatures
      ]
      ; applying the parameter vertical-infection-rate to obtain the number of infectious newly-emerged mosquitoes
      let new-emerged-infe random-poisson (num-emerged-infec-mosq * vertical-infection-rate) ; Eq. 4 in the ODD protocol
      ;if the random number obtained is greater, make it equal to num-emerged-infec-mosq.
      if new-emerged-infe > num-emerged-infec-mosq [set new-emerged-infe num-emerged-infec-mosq]
      let new-emerged-susc num-emerged-mosq - new-emerged-infe ; obtaining the number of susceptible newly-emerged by subtract the number of infectious ne from the total ne
      ; including the new values in the newly-emerged compartments
      ; transition 3 (see ODD protocol)
      set mosq-list (replace-item 0 mosq-list
                    (replace-item 3 (item 0 mosq-list) new-emerged-susc))
      ; transition 4 (see ODD protocol)
      set mosq-list (replace-item 2 mosq-list
                    (replace-item 3 (item 2 mosq-list) new-emerged-infe))

;      if who = [who] of node-test  ; code to test the procedure (general-test-compartment-transitions)
;      [
;        set tot-mosq sum item 0 mosq-list + sum item 1 mosq-list + sum item 2 mosq-list  ; obtaining the new abundance of adult mosquitoes
;       ; file-open "general-test-compartment-transitions.txt"
;        file-print " "
;        file-print "Transition 3"
;        file-type "new-emerged-susc " file-print new-emerged-susc
;        file-print " " file-print "Transition 4"
;        file-type "new-emerged-infe " file-print new-emerged-infe file-print " "
;        file-type "total immatures list " file-print total-immatures
;        file-type "mosquito list " file-print mosq-list
;        file-type "total mosquitoes " file-print tot-mosq
;        file-close
;       ]
    ]
  ]
end

to from-gravid-to-blood-seeking
  ask nodes
  [
    let susc-gr item 2 item 0 mosq-list  ;; counting current number of mosquitoes in susceptible and gravid status
    let expo-gr item 2 item 1 mosq-list  ;; counting current number of mosquitoes in exposed and gravid status
    let infe-gr item 2 item 2 mosq-list  ;; counting current number of mosquitoes in infectious and gravid status

    let susc-bs item 0 item 0 mosq-list  ;; counting current number of mosquitoes in susceptible and blood-seeking status
    let expo-bs item 0 item 1 mosq-list  ;; counting current number of mosquitoes in exposed and blood-seeking status
    let infe-bs item 0 item 2 mosq-list  ;; counting current number of mosquitoes in infectious and blood-seeking status

;    if who = [who] of node-test  ; code to test the procedure (general-test-compartment-transitions)
;    [
;      file-open "general-test-compartment-transitions.txt"
;      file-print " " file-print "#procedure from-gravid-to-blood-seeking: "
;      file-type "#time-step " file-print ticks
;      file-type "pre mosquito list " file-print mosq-list
;      file-type "pre total immatures list " file-print total-immatures
;      file-type "prob-find-breed " file-print prob-find-breed
;      ;file-close
;    ]

    ;; for susceptible-gravid mosquitoes
    let susc-gr-to-susc-bs random-poisson (susc-gr * prob-find-breed) ; Eq. 5 in the ODD protocol
    if susc-gr-to-susc-bs > susc-gr [set susc-gr-to-susc-bs susc-gr]
    ; transition 5 (see ODD protocol)
    let new-susc-gr susc-gr - susc-gr-to-susc-bs
    let new-susc-bs susc-bs + susc-gr-to-susc-bs
    ;; attributing the new values for mosquito list
    set mosq-list (replace-item 0 mosq-list
                  (replace-item 2 (item 0 mosq-list) new-susc-gr))
    set mosq-list (replace-item 0 mosq-list
                  (replace-item 0 (item 0 mosq-list) new-susc-bs))

    ;; for exposed-gravid mosquitoes
    let expo-gr-to-expo-bs random-poisson (expo-gr * prob-find-breed) ; Eq. 5 in the ODD protocol
    if expo-gr-to-expo-bs > expo-gr [set expo-gr-to-expo-bs expo-gr]
    ; transition 6 (see ODD protocol)
    let new-expo-gr expo-gr - expo-gr-to-expo-bs
    let new-expo-bs expo-bs + expo-gr-to-expo-bs
    ;; attributing the new values for mosquito list
    set mosq-list (replace-item 1 mosq-list
                  (replace-item 2 (item 1 mosq-list) new-expo-gr))
    set mosq-list (replace-item 1 mosq-list
                  (replace-item 0 (item 1 mosq-list) new-expo-bs))

    ;; for infectious-gravid mosquitoes
    let infe-gr-to-infe-bs random-poisson (infe-gr * prob-find-breed) ; Eq. 5 in the ODD protocol
    if infe-gr-to-infe-bs > infe-gr [set infe-gr-to-infe-bs infe-gr]
    ; transition 7 (see ODD protocol)
    let new-infe-gr infe-gr - infe-gr-to-infe-bs
    let new-infe-bs infe-bs + infe-gr-to-infe-bs
    ;; attributing the new values for mosquito list
    set mosq-list (replace-item 2 mosq-list
                  (replace-item 2 (item 2 mosq-list) new-infe-gr))
    set mosq-list (replace-item 2 mosq-list
                  (replace-item 0 (item 2 mosq-list) new-infe-bs))

    ;; Obtaining the total of eggs laid in the time step
    let total-ovipositions (susc-gr-to-susc-bs + expo-gr-to-expo-bs + infe-gr-to-infe-bs) ;; number of mosquitoes that laid eggs
    set new-eggs total-ovipositions * eggs-by-gravid-mosquito ;; total number of eggs laid in the time step
    set new-pot-infe-eggs infe-gr-to-infe-bs * eggs-by-gravid-mosquito ;; total number of eggs laid by infectious mosquitoes in the time step

    ;; including the new eggs in the breeding site lists
    ; transition 8 (see ODD protocol)
    set total-immatures fput new-eggs total-immatures
    set pot-infec-immatures fput new-pot-infe-eggs pot-infec-immatures

;    ; outputs to check the procedure (test for larval density)
;    if who = min [who] of nodes                   ;code to test the procedure (test for larval density)
;    [
;      file-open "test_for_larval_density.txt"
;      file-type "new eggs " file-print new-eggs
;      file-type "new-pot-infec-eggs " file-print new-pot-infe-eggs
;      file-close
;    ]

;    if who = [who] of node-test  ; code to test the procedure (general-test-compartment-transitions)
;    [
;      ; file-open "general-test-compartment-transitions.txt"
;      file-print " " file-print "Transition 5"
;      file-type "susc-gravid-to-susc-blood-seeking " file-print susc-gr-to-susc-bs
;      file-type "new-susc-blood-seeking " file-print new-susc-bs
;      file-print " " file-print "Transition 6"
;      file-type "expo-gravid-to-expo-blood-seeking " file-print expo-gr-to-expo-bs
;      file-type "new-expo-blood-seeking " file-print new-expo-bs
;      file-print " " file-print "Transition 7"
;      file-type "infe-gravid-to-infe-blood-seeking " file-print infe-gr-to-infe-bs
;      file-type "new-infe-blood-seeking " file-print new-infe-bs file-print " "
;      file-type "mosquito list " file-print mosq-list file-print " "
;      file-print "Transition 8"
;      file-type "new eggs " file-print new-eggs
;      file-type "new-pot-infec-eggs " file-print new-pot-infe-eggs
;      file-type "total immatures list " file-print total-immatures
;      file-type "potentially-infected-immatures " file-print pot-infec-immatures
;      file-close
;    ]
  ]
end

to from-blood-fed-to-gravid
  ask nodes
  [
    let susc-bf item 1 item 0 mosq-list  ;; counting current number of mosquitoes in susceptible and blood-fed status
    let expo-bf item 1 item 1 mosq-list  ;; counting current number of mosquitoes in exposed and blood-fed status
    let infe-bf item 1 item 2 mosq-list  ;; counting current number of mosquitoes in infectious and blood-fed status

    let susc-gr item 2 item 0 mosq-list  ;; counting current number of mosquitoes in susceptible and gravid status
    let expo-gr item 2 item 1 mosq-list  ;; counting current number of mosquitoes in exposed and gravid status
    let infe-gr item 2 item 2 mosq-list  ;; counting current number of mosquitoes in infectious and gravid status

;    if who = [who] of node-test  ; code to test the procedure (general-test-compartment-transitions)
;    [
;      file-open "general-test-compartment-transitions.txt"
;      file-print " " file-print "#procedure from-blood-fed-to-gravid: "
;      file-type "#time-step " file-print ticks
;      file-type "pre mosquito list " file-print mosq-list
;      file-type "prob (1 / time-to-egg-maturation)  " file-print 1 / time-to-egg-maturation
;      ;file-close
;    ]

    ;; susceptible mosquitoes
    ;;obtaining the number of blood-fed individuals that will change to gravid status
    let susc-bf-to-gr-num random-poisson (susc-bf * (1 / time-to-egg-maturation)) ; Eq. 6 in the ODD protocol
    if susc-bf-to-gr-num > susc-bf [set susc-bf-to-gr-num susc-bf] ;if the number obtained is greater than the total, then the susc-bf-to-gr-num is set to total
    ; transition 9 (see ODD protocol)
    let new-susc-bf susc-bf - susc-bf-to-gr-num  ; creating a local variable to update the number of susceptible blood-fed mosquitoes
    let new-susc-gr susc-gr + susc-bf-to-gr-num  ; creating a local variable to update the number of susceptible gravid mosquitoes
    ;; attributing the new values for mosquito list
    set mosq-list (replace-item 0 mosq-list
                  (replace-item 1 (item 0 mosq-list) new-susc-bf))
    set mosq-list (replace-item 0 mosq-list
                  (replace-item 2 (item 0 mosq-list) new-susc-gr))

    ;; exposed mosquitoes
    let expo-bf-to-gr-num random-poisson (expo-bf * (1 / time-to-egg-maturation)) ; Eq. 6 in the ODD protocol
    if expo-bf-to-gr-num > expo-bf [set expo-bf-to-gr-num expo-bf]
    ; transition 10 (see ODD protocol)
    let new-expo-bf expo-bf - expo-bf-to-gr-num
    let new-expo-gr expo-gr + expo-bf-to-gr-num
    ;; attributing the new values for mosquito list
    set mosq-list (replace-item 1 mosq-list
                  (replace-item 1 (item 1 mosq-list) new-expo-bf))
    set mosq-list (replace-item 1 mosq-list
                  (replace-item 2 (item 1 mosq-list) new-expo-gr))

    ;; for infectious mosquitoes
    let infe-bf-to-gr-num random-poisson (infe-bf * (1 / time-to-egg-maturation)) ; Eq. 6 in the ODD protocol
    if infe-bf-to-gr-num > infe-bf [set infe-bf-to-gr-num infe-bf]
    ; transition 11 (see ODD protocol)
    let new-infe-bf infe-bf - infe-bf-to-gr-num
    let new-infe-gr infe-gr + infe-bf-to-gr-num
    ;; attributing the new values for mosquito list
    set mosq-list (replace-item 2 mosq-list
                  (replace-item 1 (item 2 mosq-list) new-infe-bf))
    set mosq-list (replace-item 2 mosq-list
                  (replace-item 2 (item 2 mosq-list) new-infe-gr))

;    if who = [who] of node-test  ; code to test the procedure (general-test-compartment-transitions)
;    [
;      ; file-open "general-test-compartment-transitions.txt"
;      file-print " "
;      file-print "Transition 9"
;      file-type "susc-blood-fed-to-gravid-num " file-print susc-bf-to-gr-num
;      file-type "new-susc-gravid " file-print new-susc-gr
;      file-print " " file-print "Transition 10"
;      file-type "expo-blood-fed-to-gravid-num " file-print expo-bf-to-gr-num
;      file-type "new-expo-gravid " file-print new-expo-gr
;      file-print " " file-print "Transition 11"
;      file-type "infe-blood-fed-to-gravid-num " file-print infe-bf-to-gr-num
;      file-type "new-infe-gravid " file-print new-infe-gr
;      file-print " " file-type "mosquito list " file-print mosq-list
;      file-close
;    ]

  ]
end

to from-exposed-to-infectious
  ask nodes
  [
    let expo-bs item 0 item 1 mosq-list  ;; counting current number of mosquitoes in exposed and blood-seeking status
    let expo-bf item 1 item 1 mosq-list  ;; counting current number of mosquitoes in exposed and blood-fed status
    let expo-gr item 2 item 1 mosq-list  ;; counting current number of mosquitoes in exposed and gravid status

    let infe-bs item 0 item 2 mosq-list  ;; counting current number of mosquitoes in infectious and blood-seeking status
    let infe-bf item 1 item 2 mosq-list  ;; counting current number of mosquitoes in infectious and blood-fed status
    let infe-gr item 2 item 2 mosq-list  ;; counting current number of mosquitoes in infectious and gravid status

    let expo-main-host item 0 item 1 host-list ;; counting the current number of exposed main hosts
    let expo-alt-host item 1 item 1 host-list ;; counting the current number of exposed alternative hosts

    let infe-main-host item 0 item 2 host-list ;; counting the current number of infectious main hosts
    let infe-alt-host item 1 item 2 host-list ;; counting the current number of infectious alternative hosts

;    if who = [who] of node-test  ; code to test the procedure (general-test-compartment-transitions)
;    [
;      file-open "general-test-compartment-transitions.txt"
;      file-print " " file-print "#procedure from-exposed-to-infectious: "
;      file-type "#time-step " file-print ticks
;      file-type "pre mosquito list " file-print mosq-list
;      file-type "pre host list " file-print host-list
;      file-type "prob mosquitoes (1 / extrinsic-incubation-period) " file-print 1 / extrinsic-incubation-period
;      file-type "prob hosts (1 / latent-period) " file-print 1 / latent-period
;      ;file-close
;    ]

    ;; exposed-blood-seeking mosquitoes
    ;; obtaining the new infectious blood-seeking mosquitoes
    let expo-bs-to-infe-bs-num random-poisson (expo-bs * (1 / extrinsic-incubation-period)) ; Eq. 7 in the ODD protocol
    if expo-bs-to-infe-bs-num > expo-bs [set expo-bs-to-infe-bs-num expo-bs] ; the new value must be <= expo-bs
    ; transition 12 (see ODD protocol)
    let new-expo-bs expo-bs - expo-bs-to-infe-bs-num  ; calculating the new value for the exposed blood-seeking compartment
    let new-infe-bs infe-bs + expo-bs-to-infe-bs-num  ; calculating the new value for the infectious blood-seeking compartment
    ;; attributing the new values for mosquito list
    set mosq-list (replace-item 1 mosq-list
                  (replace-item 0 (item 1 mosq-list) new-expo-bs))
    set mosq-list (replace-item 2 mosq-list
                  (replace-item 0 (item 2 mosq-list) new-infe-bs))

    ;; exposed-blood-fed mosquitoes
    ;; obtaining the new infectious blood-fed mosquitoes
    let expo-bf-to-infe-bf-num random-poisson (expo-bf * (1 / extrinsic-incubation-period)) ; Eq. 7 in the ODD protocol
    if expo-bf-to-infe-bf-num > expo-bf [set expo-bf-to-infe-bf-num expo-bf] ; the new value must be <= expo-bf
    ; transition 13 (see ODD protocol)
    let new-expo-bf expo-bf - expo-bf-to-infe-bf-num ; calculating the new value for the exposed blood-fed compartment
    let new-infe-bf infe-bf + expo-bf-to-infe-bf-num ; calculating the new value for the infectious blood-fed compartment
    ;; attributing the new values for mosquito list
    set mosq-list (replace-item 1 mosq-list
                  (replace-item 1 (item 1 mosq-list) new-expo-bf))
    set mosq-list (replace-item 2 mosq-list
                  (replace-item 1 (item 2 mosq-list) new-infe-bf))

    ;; exposed-gravid mosquitoes
    ;; obtaining the new infectious gravid mosquitoes
    let expo-gr-to-infe-gr-num random-poisson (expo-gr * (1 / extrinsic-incubation-period)) ; Eq. 7 in the ODD protocol
    if expo-gr-to-infe-gr-num > expo-gr [set expo-gr-to-infe-gr-num expo-gr] ; the new value must be <= expo-gr
    ; transition 14 (see ODD protocol)
    let new-expo-gr expo-gr - expo-gr-to-infe-gr-num ; calculating the new value for the exposed gravid compartment
    let new-infe-gr infe-gr + expo-gr-to-infe-gr-num ; calculating the new value for the infectious gravid compartment
    ;; attributing the new values for mosquito list
    set mosq-list (replace-item 1 mosq-list
                  (replace-item 2 (item 1 mosq-list) new-expo-gr))
    set mosq-list (replace-item 2 mosq-list
                  (replace-item 2 (item 2 mosq-list) new-infe-gr))

    ;; exposed main hosts
    ;; obtaining the new infectious main hosts
    let expo-mh-to-infe-mh-num random-poisson (expo-main-host * (1 / latent-period)) ; Eq. 8 in the ODD protocol
    if expo-mh-to-infe-mh-num > expo-main-host [set expo-mh-to-infe-mh-num expo-main-host] ; the value must be <= expo-main-host
    ; transition 15 (see ODD protocol)
    let new-expo-main-host  expo-main-host - expo-mh-to-infe-mh-num ; calculating the new value for the exposed compartment
    let new-infe-main-host  infe-main-host + expo-mh-to-infe-mh-num ; calculating the new value for the infectious compartment
    ;; attributing the new values for host list
    set host-list (replace-item 1 host-list
                  (replace-item 0 (item 1 host-list) new-expo-main-host))
    set host-list (replace-item 2 host-list
                  (replace-item 0 (item 2 host-list) new-infe-main-host))

    ;; exposed alternative hosts
    ;; obtaining the new infectious alternative hosts
    let expo-ah-to-infe-ah-num random-poisson (expo-alt-host * (1 / latent-period)) ; Eq. 8 in the ODD protocol
    if expo-ah-to-infe-ah-num > expo-alt-host [set expo-ah-to-infe-ah-num expo-alt-host]  ; the value must be <= expo-alt-host
    ; transition 16 (see ODD protocol)
    let new-expo-alt-host  expo-alt-host - expo-ah-to-infe-ah-num ; calculating the new value for the exposed compartment
    let new-infe-alt-host  infe-alt-host + expo-ah-to-infe-ah-num ; calculating the new value for the infectious compartment
    ;; attributing the new values for host list
    set host-list (replace-item 1 host-list
                  (replace-item 1 (item 1 host-list) new-expo-alt-host))
    set host-list (replace-item 2 host-list
                  (replace-item 1 (item 2 host-list) new-infe-alt-host))

;    if who = [who] of node-test  ; code to test the procedure (general-test-compartment-transitions)
;    [
;      ; file-open "general-test-compartment-transitions.txt"
;      file-print " " file-print "Transition 12"
;      file-type "expo-blood-seeking-to-infe-blood-seeking-num " file-print expo-bs-to-infe-bs-num
;      file-type "new-infectious-blood-seeking " file-print new-infe-bs
;      file-print " " file-print "Transition 13"
;      file-type "expo-blood-fed-to-infe-blood-fed-num " file-print expo-bf-to-infe-bf-num
;      file-type "new-infectious-blood-fed " file-print new-infe-bf
;      file-print " " file-print "Transition 14"
;      file-type "expo-gravid-to-infe-gravid-num " file-print expo-gr-to-infe-gr-num
;      file-type "new-infectious-gravid " file-print new-infe-gr
;      file-print " " file-print "Transition 15"
;      file-type "expo-main-host-to-infe-main-host-num " file-print expo-mh-to-infe-mh-num
;      file-type "new-infe-main-host " file-print new-infe-main-host
;      file-print " " file-print "Transition 16"
;      file-type "expo-alt-host-to-infe-alt-host-num " file-print expo-ah-to-infe-ah-num
;      file-type "new-infe-alt-host " file-print new-infe-alt-host
;      file-print " " file-type "mosquito list " file-print mosq-list file-type "host list " file-print host-list
;      file-close
;    ]
  ]
end


to vector-host-interaction
  ask nodes
  [
    let susc-bs item 0 item 0 mosq-list  ;; counting current number of mosquitoes in susceptible and blood-seeking status
    let expo-bs item 0 item 1 mosq-list  ;; counting current number of mosquitoes in exposed and blood-seeking status
    let infe-bs item 0 item 2 mosq-list  ;; counting current number of mosquitoes in infectious and blood-seeking status

    let susc-bf item 1 item 0 mosq-list  ;; counting current number of mosquitoes in susceptible and blood-fed status
    let expo-bf item 1 item 1 mosq-list  ;; counting current number of mosquitoes in exposed and blood-fed status
    let infe-bf item 1 item 2 mosq-list  ;; counting current number of mosquitoes in infectious and blood-fed status

    let bs-mosq (susc-bs + expo-bs + infe-bs) ;; counting current total of blood-seeking mosquitoes
    ;; calculating the proportion of infectious individuals among blood-seeking mosquitoes (avoiding division by 0)
    let prop-bs-infe 0
    ifelse bs-mosq > 0 [set prop-bs-infe infe-bs / bs-mosq] [set prop-bs-infe prop-bs-infe]

    let susc-mosq sum item 0 mosq-list   ;; counting current total number of susceptible mosquitoes
    let expo-mosq sum item 1 mosq-list   ;; counting current total number of exposed-infected mosquitoes
    let infe-mosq sum item 2 mosq-list   ;; counting current total number of infectious mosquitoes

    ;; calculating the probability of interactions between blood-seeking vectors and hosts
    let interactions prob-find-host * biting-success ; Eq. 9 in the ODD protocol

    ;; counting the current total number of hosts
    let tot-host sum item 0 host-list + sum item 1 host-list + sum item 2 host-list + sum item 3 host-list

    ;; counting the current total number of main hosts
    let current-main-host-num (item 0 item 0 host-list) + (item 0 item 1 host-list) + (item 0 item 2 host-list) + (item 0 item 3 host-list)
    ;; calculating the proportion of main hosts based on the total number of hosts (avoiding division by 0)
    let prop-main-host 0
    ifelse tot-host > 0 [set prop-main-host (current-main-host-num / tot-host)][set prop-main-host prop-main-host]

    let infe-main-host item 0 item 2 host-list ;; counting the current number of infectious main hosts
    ;; calculating the proportion of infectious main hosts (avoiding division by 0)
    let prop-infe-main-host 0
    ifelse current-main-host-num > 0 [set prop-infe-main-host (infe-main-host / current-main-host-num)][set prop-infe-main-host prop-infe-main-host]

    let susc-main-host item 0 item 0 host-list ;; counting the current number of susceptible main hosts
    let expo-main-host item 0 item 1 host-list ;; counting the current number of exposed main hosts

    ;; counting the total number of alternative hosts (for model simplicity this number remains constant)
    let current-alt-host-num (item 1 item 0 host-list) + (item 1 item 1 host-list) + (item 1 item 2 host-list) + (item 1 item 3 host-list)
    ;; calculating the proportion of alternative hosts based on the total number of hosts (avoiding division by 0)
    let prop-alt-host 0
    ifelse tot-host > 0 [set prop-alt-host (current-alt-host-num / tot-host)][set prop-alt-host prop-alt-host]

    let infe-alt-host item 1 item 2 host-list ;; counting the current number of infectious alt hosts
    ;; calculating the proportion of infectious alternative hosts (avoiding division by 0)
    let prop-infe-alt-host 0
    ifelse current-alt-host-num > 0 [set prop-infe-alt-host (infe-alt-host / current-alt-host-num)] [set prop-infe-alt-host prop-infe-alt-host]

    let susc-alt-host item 1 item 0 host-list ;; counting the current number of susceptible alternative hosts
    let expo-alt-host item 1 item 1 host-list ;; counting the current number of exposed alternative hosts

    ;; obtaining the daily number of interactions for each blood-seeking compartment of mosquitoes
    ;; the random-poisson function gives some stochasticity for the daily interactions
    let susc-int random-poisson (susc-bs * interactions) ; Eq. 10 in the ODD protocol
    if susc-int > susc-bs [set susc-int susc-bs]
    let expo-int random-poisson (expo-bs * interactions) ; Eq. 10 in the ODD protocol
    if expo-int > expo-bs [set expo-int expo-bs]
    let infe-int random-poisson (infe-bs * interactions) ; Eq. 10 in the ODD protocol
    if infe-int > infe-bs [set infe-int infe-bs]

    ;; calculating the average daily number of bites per host (avoiding division by 0)
    let tot-interac susc-int + expo-int + infe-int
    let num-bites-per-host 0
    ifelse bs-mosq > 0 and tot-host > 0
    [
      set num-bites-per-host tot-interac / tot-host ; Eq. 13 in the ODD protocol
    ]
    [set num-bites-per-host num-bites-per-host]

    ;; calculating the reference daily number of newly exposed/infected main hosts
    let num-infected-main-host (susc-main-host * num-bites-per-host * prop-bs-infe * mosquito-trans-competence) ; Eq. 12 in the ODD protocol

    ;; calculating the reference daily number of newly exposed/infected alternative hosts
    let num-infected-alt-host (susc-alt-host * num-bites-per-host * prop-bs-infe * mosquito-trans-competence) ; Eq. 12 in the ODD protocol

    ;; calculating the reference daily number of susceptible mosquitoes that were infected from infectious main hosts
    let infec-from-main-host (susc-int * prop-main-host * prop-infe-main-host * main-host-trans-competence) ; Eq. 11 in the ODD protocol
    ;; calculating the reference daily number of susceptible mosquitoes that were infected from infectious alternative hosts
    let infec-from-alt-host (susc-int * prop-alt-host * prop-infe-alt-host * alt-host-trans-competence) ; Eq. 11 in the ODD protocol
    ;; the total reference daily number of newly exposed/infected mosquitoes
    let num-infected-mosq infec-from-main-host + infec-from-alt-host ; Eq. 11 in the ODD protocol

;    if who = [who] of node-test  ; code to test the procedure (general-test-compartment-transitions)
;    [
;      file-open "general-test-compartment-transitions.txt"
;      file-print " " file-print "#procedure vector-host-interaction: " file-type "#time-step " file-print ticks
;      file-type "pre mosquito list " file-print mosq-list file-type "pre host list " file-print host-list
;      file-type "total blood-seeking mosquitoes " file-print bs-mosq file-type "prop-blood-seeking-infe " file-print prop-bs-infe
;      file-type "susceptible-mosq " file-print susc-mosq file-type "exposed-mosq " file-print expo-mosq file-type "infectious-mosq " file-print infe-mosq
;      file-type "prob-find-host " file-print prob-find-host file-type "biting-success " file-print biting-success file-type "interactions " file-print interactions
;      file-type "total-hosts " file-print tot-host file-type "current-main-host-number " file-print current-main-host-num file-type "proportion-main-host " file-print prop-main-host
;      file-type "prop-infectious-main-host " file-print prop-infe-main-host file-type "current-alt-host-number " file-print current-alt-host-num
;      file-type "proportion-alt-host " file-print prop-alt-host file-type "prop-infectious-alt-host " file-print prop-infe-alt-host
;      file-type "susceptible-interactions " file-print susc-int file-type "exposed-interactions " file-print expo-int file-type "infectious-interactions " file-print infe-int
;      file-type "total-interactions " file-print tot-interac file-type "number-of-bites-per-host " file-print num-bites-per-host file-print " "
;      file-type "referential daily number of new infected main hosts " file-print num-infected-main-host
;      file-type "referential daily number of new infected alternative hosts " file-print num-infected-alt-host
;      file-type "infected(mosq)-from-main-host " file-print infec-from-main-host file-type "infected(mosq)-from-alt-host " file-print infec-from-alt-host
;      file-type "referential daily number of new infected mosquitoes " file-print num-infected-mosq
;      ;file-close
;    ]

    ;; for susceptible/blood-seeking mosquitoes
    let new-infected-mosq random-poisson num-infected-mosq ; generating an (random) real number of new infected mosquitoes based on the referential daily number
    if new-infected-mosq > susc-int [set new-infected-mosq susc-int] ; The new-infected-mosq must not be higher than susc-int
    ; transition 17 (see ODD protocol)
    let new-susc-bs susc-bs - susc-int  ;; calculating the new value for the susceptible/blood-seeking compartment
    let new-susc-bf susc-bf + (susc-int - new-infected-mosq) ;; calculating the new value for the susceptible/blood-fed compartment (subtract the new infected mosquitoes)
    ;;atributing the new values for the mosquito list
    set mosq-list (replace-item 0 mosq-list
                  (replace-item 0 (item 0 mosq-list) new-susc-bs))
    set mosq-list (replace-item 0 mosq-list
                  (replace-item 1 (item 0 mosq-list) new-susc-bf))

    ;; for exposed/blood-seeking mosquitoes
    ; transitions 18 and 19 (see ODD protocol)
    let new-expo-bs expo-bs - expo-int  ;; calculating the new value for the exposed/blood-seeking compartment
    let new-expo-bf expo-bf + expo-int + new-infected-mosq ;; calculating the new value for the exposed/blood-fed compartment (including the new infected mosquitoes)
    ;;atributing the new values for the mosquito list
    set mosq-list (replace-item 1 mosq-list
                  (replace-item 0 (item 1 mosq-list) new-expo-bs))
    set mosq-list (replace-item 1 mosq-list
                  (replace-item 1 (item 1 mosq-list) new-expo-bf))

    ;; for infectious/blood-seeking mosquitoes
    ; transition 20 (see ODD protocol)
    let new-infe-bs infe-bs - infe-int  ;; calculating the new value for the infectious/blood-seeking compartment
    let new-infe-bf infe-bf + infe-int  ;; calculating the new value for the infectious/blood-fed compartment
    ;;atributing the new values for the mosquito list
    set mosq-list (replace-item 2 mosq-list
                  (replace-item 0 (item 2 mosq-list) new-infe-bs))
    set mosq-list (replace-item 2 mosq-list
                  (replace-item 1 (item 2 mosq-list) new-infe-bf))

    ;; Main hosts
    let new-infected-main-host random-poisson num-infected-main-host ; obtaining the (random) real number of new exposed/infected main hosts
    if new-infected-main-host > susc-main-host [set new-infected-main-host susc-main-host] ; The new-infected-main-host must not be higher than susc-main-host
    ; transition 21 (see ODD protocol)
    let new-susc-main-host susc-main-host - new-infected-main-host ;; calculating the new value for the susceptible/main host compartment
    let new-expo-main-host expo-main-host + new-infected-main-host ;; calculating the new value for the exposed/main host compartment
    ;;atributing the new values for the host list
    set host-list (replace-item 0 host-list
                  (replace-item 0 (item 0 host-list) new-susc-main-host))
    set host-list (replace-item 1 host-list
                  (replace-item 0 (item 1 host-list) new-expo-main-host))

    ;; Alternative hosts
    let new-infected-alt-host random-poisson num-infected-alt-host ; obtaining the (random) real number of new exposed/infected alternative hosts
    if new-infected-alt-host > susc-alt-host [set new-infected-alt-host susc-alt-host] ; The new-infected-alt-host must not be higher than susc-alt-host
    ; transition 22 (see ODD protocol)
    let new-susc-alt-host susc-alt-host - new-infected-alt-host ;; calculating the new value for the susceptible/alternative host compartment
    let new-expo-alt-host expo-alt-host + new-infected-alt-host ;; calculating the new value for the exposed/alternative host compartment
    ;;atributing the new values for the host list
    set host-list (replace-item 0 host-list
                  (replace-item 1 (item 0 host-list) new-susc-alt-host))
    set host-list (replace-item 1 host-list
                  (replace-item 1 (item 1 host-list) new-expo-alt-host))

;    if who = [who] of node-test  ; code to test the procedure (general-test-compartment-transitions)
;    [
;      ; file-open "general-test-compartment-transitions.txt"
;      file-print " " file-print "Transition 17"
;      file-type "new-infected-mosq " file-print new-infected-mosq
;      file-type "new-susceptible-blood-fed (susceptible-blood-fed + susceptible-interactions - new-infected-mosq) " file-print new-susc-bf
;      file-print " " file-print "Transition 18"
;      file-type "exposed-blood-fed + new-infected-mosq " file-print expo-bf + new-infected-mosq
;      file-print " " file-print "Transition 19"
;      file-type "new-exposed-blood-fed (exposed-blood-fed + exposed-interactions + new-infected-mosq) " file-print new-expo-bf
;      file-print " " file-print "Transition 20"
;      file-type "new-infectious-blood-fed  (infectious-blood-fed + infectious-interactions) " file-print new-infe-bf
;      file-print " " file-print "Transition 21"
;      file-type "new-infected-main-host " file-print new-infected-main-host
;      file-type "new-exposed-main-host (exposed-main-host + new-infected-main-host) " file-print new-expo-main-host
;      file-print " " file-print "Transition 22"
;      file-type "new-infected-alt-host " file-print new-infected-alt-host
;      file-type "new-exposed-alt-host (exposed-alt-host + new-infected-alt-host) " file-print new-expo-alt-host
;      file-print " " file-type "mosquito list " file-print mosq-list file-type "host list " file-print host-list
;      file-close
;    ]

  ]
end


to mosquitoes-dispersal
  ask links
  [
    let disp-rate-1 dispersal-rate-end1  ; store the dispersion rate from end1 to end2 in a local variable to use when ask end1
    let disp-rate-2 dispersal-rate-end2  ; store the dispersion rate from end2 to end1 in a local variable to use when ask end2
    let ext-link outer-link? ; local boolean variable to indentify if the link is external or not
    let mov-list-end1 []  ; list to store the number of mosquitoes from each compartment that will move from end1 to end2
    let mov-list-end2 []  ; list to store the number of mosquitoes from each compartment that will move from end2 to end1
    ; creating a binary list that will be multiplied by mov-list-end1 and mov-list-end2, resulting new mov-lists where the newly-emerged compartments = 0
    ; it is assumed that newly-emerged mosquitoes will not disperse between nodes
    let binary-list [[1 1 1 0][1 1 1][1 1 1 0]]
    ; transition 23 (see ODD protocol)
    ask end1
    [
      set mov-list-end1 (map [[x] -> (map [[y] -> round (y * disp-rate-1)] x) ] mosq-list) ; applying disp-rate-1 for each item of the mosq-list and storing the result in the mov-list-end1
      set mov-list-end1 (map [[x1 x2] -> (map [[y1 y2] -> y1 * y2] x1 x2)] mov-list-end1 binary-list) ; multiplying the mov-list-end1 by the binary-list
      set mosq-list (map [[x1 x2] -> (map [[y1 y2] -> y1 - y2] x1 x2)] mosq-list mov-list-end1) ; update the mosq list by subtracting those individuals that will leave the node
    ]
    ask end2
    [
      set mov-list-end2 (map [[x] -> (map [[y] -> round (y * disp-rate-2)] x) ] mosq-list) ; applying disp-rate-2 for each item of the mosq-list and storing the result in the mov-list-end2
      set mov-list-end2 (map [[x1 x2] -> (map [[y1 y2] -> y1 * y2] x1 x2)] mov-list-end2 binary-list) ; multiplying the mov-list-end2 by the binary-list
      set mosq-list (map [[x1 x2] -> (map [[y1 y2] -> y1 - y2] x1 x2)] mosq-list mov-list-end2) ; update the mosq list by subtracting those individuals that will leave the node
    ]

;    ; code to test the procedure (general-test-compartment-transitions)
;    file-open "general-test-compartment-transitions.txt"
;    file-print " " file-print "#procedure mosquito-dispersion: " file-type "#time-step " file-print ticks
;    file-type "pre mosquito list test-node " file-print [mosq-list] of end1
;    file-type "pre mosquito list test-node2 " file-print [mosq-list] of end2
;    file-type "dispersal rate test-node " file-print disp-rate-1
;    file-type "dispersal rate test-node2 " file-print disp-rate-2
;    ;file-close stop

    ask end1 [set mosq-list (map [[x1 x2] -> (map [[y1 y2] -> y1 + y2] x1 x2)] mosq-list mov-list-end2)] ; update the mosq list by add arrival individuals from end2
    ask end2 [set mosq-list (map [[x1 x2] -> (map [[y1 y2] -> y1 + y2] x1 x2)] mosq-list mov-list-end1)] ; update the mosq list by add arrival individuals from end1


;    ; code to test the procedure (general-test-compartment-transitions)
;    ;file-open "general-test-compartment-transitions.txt"
;    file-print " " file-print "Transition 23"
;    file-type "list of mosquitoes dispersal from test-node to test-node2 " file-print mov-list-end1
;    file-type "list of mosquitoes dispersal from test-node2 to test-node " file-print mov-list-end2
;    file-print " " file-type "mosquito list test-node " file-print [mosq-list] of end1
;    file-type "mosquito list test-node2 " file-print [mosq-list] of end2
;    file-close


    ;; if exposed or infectious mosquitoes migrate between nodes
    if sum item 1 mov-list-end1 > 0 or sum item 2 mov-list-end1 > 0
    [
      ask [my-patches] of end2 [set pcolor red] ; update the patches status (only for visual purpose)
      let viral-emerg1 [day-of-viral-emergence] of end1 ; a local variable to store the day of viral emergence in end1 (this information will be accessed by end2)
      let dist nodes-distance ; store the distance between the linked nodes to be accessed by end2
      ask end2
      [
        set active-transmission? true
        set viral-introduction viral-introduction + 1 ; adding the new event of virus migration to the node and storing this information in the variable viral-introduction
        if viral-introduction = 1 and day-of-viral-emergence = "no" ; if that is the first viral introduction in the node, the invasion spreading speed will be measured
        [
          set day-of-viral-emergence YFV-persistence-days ; the node will store the information about how many days it took for the virus to arrive after being introduced into the landscape
          if ext-link = false  ; applying the function only for internal links
          [
            let time-days (day-of-viral-emergence - viral-emerg1) ; the difference in days from the viral arrival in end1 to its arrival in end2
            if time-days = 0 [set time-days 0.5]  ; if the difference in days is 0, it is assumed 0.5 (half a day) to avoid division by 0
            ;calculating the spreading speed of the virus from end1 to end2
            let speed (dist / time-days) ; Eq. 16 in the ODD protocol
            set virus-invasion-speed fput speed virus-invasion-speed ; storing the speed in the viral-spread-speed list for calculation of the mean viral spreading speed in the landscape
          ]
        ]
      ]
    ]
    ; the same procedure above is applied for end1
    if sum item 1 mov-list-end2 > 0 or sum item 2 mov-list-end2 > 0
    [
      ask [my-patches] of end1 [set pcolor red]
      let viral-emerg2 [day-of-viral-emergence] of end2
      let dist nodes-distance
      ask end1
      [
        set active-transmission? true
        set viral-introduction viral-introduction + 1
        if viral-introduction = 1 and day-of-viral-emergence = "no"
        [
          set day-of-viral-emergence YFV-persistence-days
          if ext-link = false
          [
            let time-days (day-of-viral-emergence - viral-emerg2)
            if time-days = 0 [set time-days 0.5]
            let speed (dist / time-days) ; Eq. 16 in the ODD protocol
            set virus-invasion-speed fput speed virus-invasion-speed
         ]
      ]]]
  ]
end

to host-movement
  ask links with [minimum-distance <= 2] ; it is assumed that hosts will not move between areas separated by more than 2 patches away (> 200m)
  [
    let mov-list-end1 [] ; list to store the number of hosts from each compartment that will move from end1 to end2
    let mov-list-end2 [] ; list to store the number of hosts from each compartment that will move from end2 to end1
    let move-rate-1 host-movement-rate-end1 ; store the movement rate from end1 to end2 in a local variable to use when ask end1
    let move-rate-2 host-movement-rate-end2 ; store the movement rate from end2 to end1 in a local variable to use when ask end2
    let ext-link outer-link? ; local boolean variable to indentify if the link is external or not
    ; creating a binary list that will be multiplied by mov-list-end1 and mov-list-end2, resulting new mov-lists where the main host infectious compartments = 0
    ; sick howler monkeys tend to stop moving in the forest
    let binary-list [[1 1][1 1][0 1][1 1 1]]
    ; transition 24 (see ODD protocol)
    ask end1
    [
      set mov-list-end1 (map [[x] -> (map [[y] -> round (y * move-rate-1)] x) ] host-list) ; applying move-rate-1 for each item of the host-list and storing the result in the mov-list-end1
      set mov-list-end1 (map [[x1 x2] -> (map [[y1 y2] -> y1 * y2] x1 x2)] mov-list-end1 binary-list) ;multiplying the mov-list-end1 by the binary-list
      set host-list (map [[x1 x2] -> (map [[y1 y2] -> y1 - y2] x1 x2)] host-list mov-list-end1) ; update the host-list by subtracting the individuals that will leave the node
    ]
    ask end2
    [
      set mov-list-end2 (map [[x] -> (map [[y] -> round (y * move-rate-2)] x) ] host-list)
      set mov-list-end2 (map [[x1 x2] -> (map [[y1 y2] -> y1 * y2] x1 x2)] mov-list-end2 binary-list)
      set host-list (map [[x1 x2] -> (map [[y1 y2] -> y1 - y2] x1 x2)] host-list mov-list-end2)
    ]

;    ; code to test the procedure (general-test-compartment-transitions)
;    file-open "general-test-compartment-transitions.txt"
;    file-print " " file-print "#procedure host-movement: " file-type "#time-step " file-print ticks
;    file-type "pre host list test-node " file-print [host-list] of end1
;    file-type "pre host list test-node2 " file-print [host-list] of end2
;    file-type "movement rate test-node " file-print move-rate-1
;    file-type "movement rate test-node2 " file-print move-rate-2
;    ;file-close stop

    ask end1 [set host-list (map [[x1 x2] -> (map [[y1 y2] -> y1 + y2] x1 x2)] host-list mov-list-end2)] ; update the host-list by add arrival individuals from end2
    ask end2 [set host-list (map [[x1 x2] -> (map [[y1 y2] -> y1 + y2] x1 x2)] host-list mov-list-end1)] ; update the host-list by add arrival individuals from end1


;    ; code to test the procedure (general-test-compartment-transitions)
;    ;file-open "general-test-compartment-transitions.txt"
;    file-print " " file-print "Transition 24"
;    file-type "list of host movement from test-node to test-node2 " file-print mov-list-end1
;    file-type "list of host movement from test-node2 to test-node " file-print mov-list-end2
;    file-print " " file-type "host list test-node " file-print [host-list] of end1
;    file-type "host list test-node2 " file-print [host-list] of end2
;    file-close

    ;; if exposed or infectious hosts migrate between nodes
    if sum item 1 mov-list-end1 > 0 or sum item 2 mov-list-end1 > 0
    [
      ask [my-patches] of end2 [set pcolor red] ; update the patches status (only for visual purpose)
      let viral-emerg1 [day-of-viral-emergence] of end1     ; a local variable to store the day of viral emergence in end1 (will be accessed by end2)
      let dist nodes-distance         ; a local variable to store the distance between the linked nodes (will be accessed by end2)
      ask end2
      [
        set active-transmission? true
        set viral-introduction viral-introduction + 1  ; adding the new event of virus migration to the node and storing this information in the variable viral-introduction
        if viral-introduction = 1 and day-of-viral-emergence = "no" ; if that is the first viral introduction in the node, the spreading speed will be measured
        [
          set day-of-viral-emergence YFV-persistence-days ; the node will store the information about how many days it took for the virus to arrive after being introduced into the landscape
          if ext-link = false  ; applying the function only for internal links
          [
            let time-days (day-of-viral-emergence - viral-emerg1) ; the difference in days from the viral arrival in end1 to its arrival in end2
            if time-days = 0 [set time-days 0.5]  ; if the difference in days is 0, it is assumed 0.5 (half a day) to avoid division by 0
            let speed (dist / time-days)    ;calculating the spreading speed of the virus from end1 to end2
            set virus-invasion-speed fput speed virus-invasion-speed  ;storing the speed in the viral-spread-speed list for calculation of the mean viral spreading speed in the landscape
          ]
        ]
      ]
    ]
    ; the same procedure above is applied for end1
    if sum item 1 mov-list-end2 > 0 or sum item 2 mov-list-end2 > 0
    [
      ask [my-patches] of end1 [set pcolor red]
      let viral-emerg2 [day-of-viral-emergence] of end2
      let dist nodes-distance
      ask end1
      [
        set active-transmission? true
        set viral-introduction viral-introduction + 1
        if viral-introduction = 1 and day-of-viral-emergence = "no"
        [
          set day-of-viral-emergence YFV-persistence-days
          if ext-link = false
          [
            let time-days (day-of-viral-emergence - viral-emerg2)
            if time-days = 0 [set time-days 0.5]
            let speed (dist / time-days)
            set virus-invasion-speed fput speed virus-invasion-speed
          ]
        ]
      ]
    ]
  ]

;  ; code to test the procedure (general-test-compartment-transitions)
;  if [minimum-distance] of test-link > 2
;  [
;    file-open "general-test-compartment-transitions.txt"
;    file-print " " file-print "#procedure host-movement: " file-type "#time-step " file-print ticks
;    file-print " " file-print "Transition 24"
;    file-type "No host movement between the two nodes."
;    file-close
;  ]

end

to host-recovery-or-death
 ask nodes
  [
    let infe-main-host item 0 item 2 host-list ;; counting the current number of infectious main hosts
    let infe-alt-host item 1 item 2 host-list ;; counting the current number of infectious alternative hosts

    let immu-main-host item 0 item 3 host-list ;; counting the current number of immune main hosts
    let immu-alt-host item 1 item 3 host-list ;; counting the current number of immune main hosts

;    if who = [who] of node-test  ; code to test the procedure (general-test-compartment-transitions)
;    [
;      file-open "general-test-compartment-transitions.txt"
;      file-print " " file-print "#procedure host-recovery-or-death: " file-type "#time-step " file-print ticks
;      file-type "pre host list " file-print host-list
;      file-type "prob alt host (1 / alt-host-viremic-period) " file-print 1 / alt-host-viremic-period
;      file-type "prob main host (1 / main-host-viremic-period) " file-print 1 / main-host-viremic-period
;      ;file-close
;    ]

    ;; for alternative hosts
    ;; obtaining the number of new immune individuals
    let infe-ah-to-immu-ah random-poisson (infe-alt-host * (1 / alt-host-viremic-period)) ; Eq. 18 in the ODD protocol
    if infe-ah-to-immu-ah > infe-alt-host [set infe-ah-to-immu-ah infe-alt-host]          ; infe-ah-to-rec-die must not be > infe-alt-host
    ; transition 25 (see ODD protocol)
    let new-infe-alt-host  infe-alt-host - infe-ah-to-immu-ah  ; subtracting the new number of immunes to obtaining the new value for infectious compartment
    let new-immu-alt-host  immu-alt-host + infe-ah-to-immu-ah  ; adding the new number of immunes to obtaining the new value for immune compartment
    ;; attributing the new values for host list
    set host-list (replace-item 2 host-list
                  (replace-item 1 (item 2 host-list) new-infe-alt-host))
    set host-list (replace-item 3 host-list
                  (replace-item 1 (item 3 host-list) new-immu-alt-host))

    ;; for main hosts (mortality due infection is applied here)
    ;; obtaining the number of new immune or dead individuals
    let infe-mh-to-rec-death random-poisson (infe-main-host * (1 / main-host-viremic-period))  ; Eq. 17 in the ODD protocol
    if infe-mh-to-rec-death > infe-main-host [set infe-mh-to-rec-death infe-main-host]         ; infe-mh-to-rec-die must not be > infe-main-host

    ;; second local variable to define how many will survive
;    let rec-die-list n-values infe-mh-to-rec-death [x -> random-float 1] ; assigning a random number between 0 and 1 to each individual represented in infe-mh-to-rec-die
;    let dead-mh length filter [i -> i < infectious-main-host-mortality] rec-die-list ; if the random number is < infectious-main-host-mortality the individual will die
;    let survivors infe-mh-to-rec-death - dead-mh ; calculate the survivors (now also immune for the virus)

    let survivors infe-mh-to-rec-death - random-poisson (infe-mh-to-rec-death * infectious-main-host-mortality)
    if survivors < 0 [set survivors 0]
    set dead-monkeys dead-monkeys + (infe-mh-to-rec-death - survivors)
;    set dead-monkeys dead-monkeys + dead-mh

    ; transitions 26 and 27 (see ODD protocol)
    let new-infe-main-host  infe-main-host - infe-mh-to-rec-death ; subtracting the new number of survivors or dead individuals to obtaining the new value for infectious compartment
    let new-immu-main-host  immu-main-host + survivors ; adding the new number of survivors individuals to obtaining the new value for immune compartment
    ;; attributing the new values for host list
    set host-list (replace-item 2 host-list
                  (replace-item 0 (item 2 host-list) new-infe-main-host))
    set host-list (replace-item 3 host-list
                  (replace-item 0 (item 3 host-list) new-immu-main-host))

;    if who = [who] of node-test  ; code to test the procedure (general-test-compartment-transitions)
;    [
;      ;file-open "general-test-compartment-transitions.txt"
;      file-print " " file-print "Transition 25"
;      file-type "infected-alt-host-to-immune-alt-host " file-print infe-ah-to-immu-ah
;      file-type "new-immune-alt-hosts " file-print new-immu-alt-host
;      file-print " " file-print "Transition 26"
;      file-type "infectious-main-host-to-recovery-or-death " file-print infe-mh-to-rec-death
;      file-type "dead-main-hosts " file-print dead-mh
;      file-print " " file-print "Transition 27"
;      file-type "survivors " file-print survivors
;      file-type "new-immune-main-hosts " file-print new-immu-main-host
;      file-print " " file-type "host list " file-print host-list
;      file-close
;    ]
  ]
end

to population-dynamics-of-host
  ;; applying natural mortality (not associated to the viral infection) and new births for main hosts and alternative hosts (dead-ends are disregarded)
  let main-host-prob-die (1 / main-host-mean-lifespan) ; daily probability of natural mortality for main hosts
  let alt-host-prob-die (1 / alt-host-mean-lifespan) ; daily probability of natural mortality for alternative hosts
  let dead-list-mh []
  let dead-list-ah []
  let binary-list-mh [[1 0][1 0][1 0][1 0 0]] ; binary list to multiply and keep only main hosts results
  let binary-list-ah [[0 1][0 1][0 1][0 1 0]] ; binary list to multiply and keep only alternative hosts results

  ask nodes
  [

;    if who = [who] of node-test  ; code to test the procedure (general-test-compartment-transitions)
;    [
;      file-open "general-test-compartment-transitions.txt"
;      file-print " " file-print "#procedure population-dynamics-of-host: " file-type "#time-step " file-print ticks
;      file-type "pre host list " file-print host-list
;      file-type "probability of main host dying " file-print main-host-prob-die
;      file-type "probability of alternative host dying " file-print alt-host-prob-die
;      ;file-close
;    ]
    ;; obtaining dead main hosts and storing the result in the dead-list-mh
    set dead-list-mh (map [[x] -> (map [[y] -> random-poisson (y * main-host-prob-die)] x) ] host-list) ; Eq. 19 in the ODD protocol
    set dead-list-mh (map [[x1 x2] -> (map [[y1 y2] -> y1 * y2] x1 x2)] dead-list-mh binary-list-mh) ;multiplying the dead-list-mh by the binary-list to keep only results for main hosts
    set host-list (map [[x1 x2] -> (map [[y1 y2] -> y1 - y2] x1 x2)] host-list dead-list-mh) ; update the host-list by subtracting the individuals that died
    ; For simplicity it is assumed that for each individual that die a new individual will born in the population
    ; In this case all births will be included in the susceptible compartment
    let tot-dead-mh sum item 0 dead-list-mh + sum item 1 dead-list-mh + sum item 2 dead-list-mh + sum item 3 dead-list-mh ; the total number of deaths
    let susc-mh item 0 item 0 host-list  ; the current number of susceptible main hosts
    let new-susc-mh susc-mh + tot-dead-mh  ; the new number of susceptible main hosts (births + current number)
    ; including the new number in the susceptible compartment
    set host-list (replace-item 0 host-list
                  (replace-item 0 (item 0 host-list) new-susc-mh))

    ;;The same procedure is applied for alternative hosts
    set dead-list-ah (map [[x] -> (map [[y] -> random-poisson (y * alt-host-prob-die)] x) ] host-list) ; Eq. 19 in the ODD protocol
    set dead-list-ah (map [[x1 x2] -> (map [[y1 y2] -> y1 * y2] x1 x2)] dead-list-ah binary-list-ah)
    set host-list (map [[x1 x2] -> (map [[y1 y2] -> y1 - y2] x1 x2)] host-list dead-list-ah)
    let tot-dead-ah sum item 0 dead-list-ah + sum item 1 dead-list-ah + sum item 2 dead-list-ah + sum item 3 dead-list-ah
    let susc-ah item 1 item 0 host-list
    let new-susc-ah susc-ah + tot-dead-ah
    set host-list (replace-item 0 host-list
                  (replace-item 1 (item 0 host-list) new-susc-ah))

;    if who = [who] of node-test  ; code to test the procedure (general-test-compartment-transitions)
;    [
;      ;file-open "general-test-compartment-transitions.txt"
;      file-type "dead-list-main-hosts " file-print dead-list-mh
;      file-type "tot-dead-main-hosts " file-print tot-dead-mh
;      file-type "new-susceptible-main-hosts (susceptibles + new births) " file-print new-susc-mh
;      file-type "dead-list-alt-hosts " file-print dead-list-ah
;      file-type "tot-dead-alt-hosts " file-print tot-dead-ah
;      file-type "new-susceptible-alt-hosts (susceptibles + new births) " file-print new-susc-ah
;      file-type "host list " file-print host-list
;      file-close
;    ]

  ]
end

;;Insert virus procedure
;;The external nodes located in the top of the grid will be those that intiate the viral dispersion (from the South to the North)
;;A number of infectious mosquitoes defined by the parameter "YFV-initial-burden" will be "released" in five selected nodes to simulate the experiment
to insert-the-virus
  ;mosquitoes will released in the intial external nodes
  if ticks = start-YFV-circulation ;
  [
    let num-ext-nodes count nodes with [external-node? and ycor < (max-pycor) and ycor > (max-pycor - external-area) ]
    if num-ext-nodes >= 5 ;ensuring there are a minimum number of external nodes
    [
      ask n-of 5 nodes with [external-node? and ycor < (max-pycor) and ycor > (max-pycor - external-area) ] ; selecting five external nodes that will receive the infected mosquitoes
      [
        let tot-mosq sum item 0 mosq-list ; at this point all mosquitoes in the node are susceptible
        if  tot-mosq * YFV-initial-burden >= 1
        [
          let target-list item 0 mosq-list ; creating a list of the susceptibles sublist from mosq-list
          set target-list map [x -> round (x * YFV-initial-burden)] target-list ; multiplying each item of target-list by YFV-initial-burden to define the number of infectious will be introduced in the node
          let new-susc-list item 0 mosq-list ; creating a local list to store the new number of susceptibles in the node
          set new-susc-list (map - new-susc-list target-list) ; calculating the new number of susceptibles
          set mosq-list replace-item 2 mosq-list target-list  ; change mosq-list to include te new number of infectious mosquitoes
          set mosq-list replace-item 0 mosq-list new-susc-list ; change mosq-list to include the new number of susceptible mosquitoes (after subtract the new infectious)
          set active-transmission? true
          set day-of-viral-emergence YFV-persistence-days
          set viral-introduction viral-introduction + 1 ; update the number of time steps the node received at least one infected individual
          ask my-patches [set pcolor red]
        ]
      ]
    ]
  ]
end


to calc-the-prob-of-finding-a-host
  ask nodes
  [
    let tot-host sum item 0 host-list + sum item 1 host-list + sum item 2 host-list + sum item 3 host-list ; calculating the total number of hosts
  ;; apllying parameters calculated from a Michaelis-Menten fit based on the probability of find a host in the YFV ABM model (Medeiros-Sousa et al., 2022)
    set prob-find-host 2.38232 * (tot-host / (107.125183 + tot-host)) ; Eq. 20 in the ODD protocol
    if prob-find-host > 1 [set prob-find-host 1] ; the maximum value admitted is 1 (100%)
  ]
end

to check-viral-circulation
  ; nodes calculate their total number of infected individuals
  ask nodes [set total-infec-individuals sum item 1 mosq-list + sum item 2 mosq-list + sum item 1 host-list + sum item 2 host-list]
  ; nodes with status true for active transmission will check if the virus is no longer circulating in the area
  ; for this they will check the presence of infected mosquitoes and hosts and the presence of possible infected immatures
  ask nodes with [active-transmission?]
  [
    let pot-infe-immatures-sum sum pot-infec-immatures
    ; if no virus presence the active transmission is set to false
    if total-infec-individuals = 0 and pot-infe-immatures-sum = 0
    [
      set active-transmission? false
      ask my-patches [set pcolor yellow]
    ]
  ]
end


;#######################################################################################################################
;; Model reports

to-report total-mosquitoes
  let ab-tot sum [sum item 0 mosq-list + sum item 1 mosq-list + sum item 2 mosq-list] of nodes
  report ab-tot
end

; report how many days the virus is circulating in the landscape
to-report YFV-persistence-days
  if ticks >= start-YFV-circulation [set YFV-circ ticks - start-YFV-circulation]
  report YFV-circ
end

;; report the proportion of monkeys that died of the virus infection
to-report dead-monkeys-prop
  let num-dead sum [dead-monkeys] of nodes with [external-node? = false] ; summing the number of dead monkeys from all internal nodes
  let prop-dead 0
  ; since the current version of the model assumes a constant monkey population (natural deaths = births),
  ; the initial total number of main hosts will keep constant when excluded the virus effect.
  ; For this reason, total-main-hosts can be used as denominator to calculate the proportion of dead-monkeys due to viral disease.
  if total-main-hosts > 0 [set prop-dead (num-dead / total-main-hosts)]
  report prop-dead
end

; report the proportion of internal nodes with at least one event of viral introduction
to-report prop-infected-nodes
  let infe-node count nodes with [external-node? = false and viral-introduction > 0] / count nodes with [external-node? = false]
  report infe-node
end

; report the average invasion speed of the virus based on the values reported by internal nodes
; The speed tends to decrease as the virus spread to more nodes
to-report mean-virus-invasion-speed
  let mean-speed 0
  ifelse empty? virus-invasion-speed ;accessing the viral-invasion-speed list
  [set mean-speed mean-speed]
  [set mean-speed mean virus-invasion-speed] ; if the virus-invasion-speed is not empty, calculate the mean
  report mean-speed / 10   ;since the linear distance is in patches and the distance between the center of two patches represents 100m, the value is divided by ten to convert in km
end

; this is a second way of measuring the spread speed of the virus and is based on the time it takes
; for the virus travelling half of the grid (from top to bottom) after being introduced into the landscape
to-report speed-in-the-middle-grid
  ifelse speed-percolation = 0 and any? nodes with [active-transmission? and ycor < 0]
  [set speed-percolation 6 / (ticks - start-YFV-circulation)] ; it is assumed 6km were travelled
  [set speed-percolation speed-percolation]
  report speed-percolation
end
@#$#@#$#@
GRAPHICS-WINDOW
465
10
957
503
-1
-1
4.0
1
10
1
1
1
0
0
0
1
-60
60
-60
60
0
0
1
days
30.0

BUTTON
18
31
81
64
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
16
93
216
126
initial-mosquitoes-per-ha
initial-mosquitoes-per-ha
0
3000
500.0
1
1
NIL
HORIZONTAL

SLIDER
16
138
217
171
main-host-per-ha
main-host-per-ha
0
3
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
15
181
216
214
alternative-host-per-ha
alternative-host-per-ha
0
3
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
16
224
216
257
dead-end-host-per-ha
dead-end-host-per-ha
0
3
0.5
0.05
1
NIL
HORIZONTAL

BUTTON
94
31
157
64
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
169
32
232
65
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
229
94
437
127
start-YFV-circulation
start-YFV-circulation
1
730
50.0
1
1
NIL
HORIZONTAL

SLIDER
229
136
438
169
YFV-initial-burden
YFV-initial-burden
0
0.2
0.02
0.001
1
NIL
HORIZONTAL

PLOT
463
543
782
704
Mosquito-abundance
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"capacity" 1.0 0 -16777216 true "" "let sum-cur-k sum [current-k] of nodes\nplot sum-cur-k"
"abundance" 1.0 0 -13345367 true "" "plot total-mosquitoes"

MONITOR
1000
18
1137
63
NIL
YFV-persistence-days
0
1
11

SWITCH
230
286
339
319
seed?
seed?
1
1
-1000

INPUTBOX
230
328
339
388
my-seed
1.0
1
0
Number

INPUTBOX
22
670
241
730
my_status_simulation
NIL
1
0
String

SLIDER
14
280
217
313
max-prop-mosq-dispersal
max-prop-mosq-dispersal
0
1
0.2
0.01
1
NIL
HORIZONTAL

BUTTON
241
33
316
66
Clear all
clear-all
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

OUTPUT
21
469
429
624
11

BUTTON
325
33
415
66
speed test
profiler:start\nrepeat 1 [setup]\n;repeat 1 [go]\nprofiler:stop\noutput-print profiler:report\nprofiler:reset
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
14
322
217
355
host-move-proportion
host-move-proportion
0
1
0.3
0.01
1
NIL
HORIZONTAL

MONITOR
1042
131
1227
176
mean-YFV-invasion-speed (km/day)
mean-virus-invasion-speed
3
1
11

MONITOR
1149
17
1288
62
NIL
dead-monkeys-prop
3
1
11

MONITOR
1066
75
1206
120
NIL
prop-infected-nodes
3
1
11

PLOT
795
543
1006
704
Proportion of dead monkeys
Days
Proportion
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot dead-monkeys-prop"

SLIDER
229
180
437
213
external-area
external-area
0
50
10.0
1
1
NIL
HORIZONTAL

SLIDER
13
365
217
398
maximum-distance-link
maximum-distance-link
0
30
15.0
0.5
1
NIL
HORIZONTAL

TEXTBOX
26
455
176
473
Speed test output
11
0.0
1

SLIDER
229
224
437
257
maximum-size-to-merge
maximum-size-to-merge
0
30
20.0
1
1
NIL
HORIZONTAL

CHOOSER
349
286
441
331
landscape
landscape
"1" "2" "3"
1

MONITOR
1036
186
1237
231
speed-in-the-middle-grid (km/day)
speed-in-the-middle-grid
3
1
11

TEXTBOX
26
653
176
671
Input from NLRX
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
