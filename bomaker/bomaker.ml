type ticks = int (* 10 ticks per second *)

type unit_id = int

type unit_type =
    | Scv
    | Command_center
    | Refinery
    | Supply_depot
    | Barracks
    | Orbital_command
    | Reactor
    | Techlab
    | Reaper

let minerals_cost = function
    | Scv -> 50
    | Command_center -> 400
    | Refinery -> 75
    | Supply_depot -> 100
    | Barracks -> 150
    | Orbital_command -> 150
    | Reactor -> 50
    | Techlab -> 50
    | Reaper -> 50

let gas_cost = function
    | Scv | Command_center | Refinery |
    Supply_depot | Barracks | Orbital_command -> 0
    | Reactor -> 50
    | Techlab -> 25
    | Reaper -> 50

let build_time = function
    | Scv -> 120
    | Command_center -> 710
    | Refinery -> 210
    | Supply_depot -> 210
    | Barracks -> 460
    | Orbital_command -> 25
    | Reactor -> 360
    | Techlab -> 180
    | Reaper -> 32

let supply = function
    | Scv -> -1
    | Command_center -> 15
    | Refinery -> 0
    | Supply_depot -> 8
    | Barracks | Reactor | Techlab -> 0
    | Orbital_command -> 15
    | Reaper -> -1

type ability =
    | Build_unit of unit_type
    | Morph of unit_type
    | Gather_minerals
    | Gather_gas

let ability_duration = function
    | Build_unit unit_type -> build_time unit_type
    | Morph unit_type -> build_time unit_type
    | Gather_minerals -> 56
    | Gather_gas -> 42

type unit_state =
    | Idling
    | UsingAbility of ability * ticks

let use_ability ability =
    UsingAbility (ability, ability_duration ability)

type unit_store = (unit_id * unit_type * unit_state) list

let (empty_unit_store : unit_store) = []

let lookup_unit unit_id =
    List.find (function (id, _, _) -> id = unit_id)

type unit_pattern = unit_id option * unit_type option * unit_state option

let match_unit (uid_pat, ut_pat, state_pat) (uid, ut, state) =
    let uid_match = match uid_pat with
        | None -> true
        | Some uid_ -> uid = uid_ in
    let ut_match = match ut_pat with
        | None -> true
        | Some ut_ -> ut = ut_ in
    let state_match = match (state_pat, state) with
        | (None, _) -> true
        | (Some Idling, Idling) -> true
        | (Some (UsingAbility (ability, _)), UsingAbility (ability_, _)) ->
            ability = ability_
        | _ -> false in
    uid_match && ut_match && state_match

type global_state = {
    minerals : int;
    gas : int;
    all_units : unit_store;
    game_clock : int;
}

let initial_global_state = {
    minerals = 50;
    gas = 0;
    all_units = [
        (0, Command_center, Idling);
        (1, Scv, UsingAbility (Gather_minerals, ability_duration Gather_minerals + 0));
        (2, Scv, UsingAbility (Gather_minerals, ability_duration Gather_minerals + 1));
        (3, Scv, UsingAbility (Gather_minerals, ability_duration Gather_minerals + 2));
        (4, Scv, UsingAbility (Gather_minerals, ability_duration Gather_minerals + 3));
        (5, Scv, UsingAbility (Gather_minerals, ability_duration Gather_minerals + 4));
        (6, Scv, UsingAbility (Gather_minerals, ability_duration Gather_minerals + 5));
        (7, Scv, UsingAbility (Gather_minerals, ability_duration Gather_minerals + 6));
        (8, Scv, UsingAbility (Gather_minerals, ability_duration Gather_minerals + 7));
        (9, Scv, UsingAbility (Gather_minerals, ability_duration Gather_minerals + 8));
        (10, Scv, UsingAbility (Gather_minerals, ability_duration Gather_minerals + 9));
        (11, Scv, UsingAbility (Gather_minerals, ability_duration Gather_minerals + 10));
        (12, Scv, UsingAbility (Gather_minerals, ability_duration Gather_minerals + 11));
    ];
    game_clock = 0;
}

let supply_available global_state =
    let state_supply = function
        | UsingAbility (Build_unit unit_type, _) -> min 0 (supply unit_type)
        | _ -> 0 in
    global_state.all_units
    |> List.map (function (_, unit_type, state) ->
        supply unit_type + state_supply state)
    |> List.fold_left (+) 0

let supply_cap global_state =
    global_state.all_units
    |> List.map (function (_, unit_type, state) -> max 0 (supply unit_type))
    |> List.fold_left (+) 0
    |> min 200

let count_units unit_type global_state =
    global_state.all_units
    |> List.filter (function (_, ut, _) -> ut = unit_type)
    |> List.length

let count_units_by f global_state =
    global_state.all_units
    |> List.filter f
    |> List.length

let next_tick global_state =
    let minerals_diff = ref 0 in
    let gas_diff = ref 0 in
    let new_units = ref empty_unit_store in
    let new_id () =
        (global_state.all_units @ !new_units)
        |> List.map (function (id, _, _) -> id)
        |> List.fold_left max 0
        |> (+) 1 in
    let step_unit (unit_id, unit_type, unit_state) =
        match unit_state with
        | Idling -> (unit_id, unit_type, unit_state)
        | UsingAbility (ability, eta) when eta > 0 ->
            (unit_id, unit_type, UsingAbility (ability, eta - 1))
        | UsingAbility (Build_unit ut_to_build, 0) ->
            new_units := (new_id (), ut_to_build, Idling) :: !new_units;
            (unit_id, unit_type, Idling)
        | UsingAbility (Morph ut_to_morph, 0) ->
            (unit_id, ut_to_morph, Idling)
        | UsingAbility (Gather_minerals, 0) ->
            minerals_diff := !minerals_diff + 5;
            (unit_id, unit_type, use_ability Gather_minerals)
        | UsingAbility (Gather_gas, 0) ->
            gas_diff := !gas_diff + 4;
            (unit_id, unit_type, use_ability Gather_gas)
        | _ -> raise (Invalid_argument "Invalid unit state.") in
    {
        minerals = global_state.minerals + !minerals_diff;
        gas = global_state.gas + !gas_diff;
        all_units = begin
            let next = List.map step_unit global_state.all_units
            in next @ !new_units
        end;
        game_clock = global_state.game_clock + 1;
    }

let recruit_workers global_state =
    let is_gathering_minerals =
        match_unit (None, Some Scv, Some (use_ability Gather_minerals)) in
    let is_gathering_gas =
        match_unit (None, Some Scv, Some (use_ability Gather_gas)) in
    let is_base unit =
        match_unit (None, Some Command_center, None) unit
        || match_unit (None, Some Orbital_command, None) unit in
    let is_refinery =
        match_unit (None, Some Refinery, None) in
    let minerals_vacancy = ref (
            count_units_by is_base global_state * 16
            -
            count_units_by is_gathering_minerals global_state
    ) in
    let gas_vacancy = ref (
            count_units_by is_refinery global_state * 3
            -
            count_units_by is_gathering_gas global_state
    ) in
    let f ((unit_id, _, _) as unit) =
        if not (match_unit (None, Some Scv, Some Idling) unit) then
            unit
        else if !gas_vacancy > 0 then begin
            gas_vacancy := !gas_vacancy - 1;
            (unit_id, Scv, use_ability Gather_gas)
        end
        else if !minerals_vacancy > 0 then begin
            minerals_vacancy := !minerals_vacancy - 1;
            (unit_id, Scv, use_ability Gather_minerals)
        end
        else unit in
    if !minerals_vacancy < 0 || !gas_vacancy < 0 then begin
        Printf.printf "mv=%d;gv=%d\n" !minerals_vacancy !gas_vacancy;
        raise (Invalid_argument "Too many workers.")
    end
    else
        { global_state with all_units = List.map f global_state.all_units }

let step global_state =
    global_state
    |> next_tick
    |> recruit_workers

type command = unit_pattern * unit_state

let try_execute (pat, state) global_state =
    let is_done = ref false in
    let minerals_diff = ref 0 in
    let gas_diff = ref 0 in
    let (state_minerals_cost, state_gas_cost, state_supply_cost) =
        match state with
        | UsingAbility (Build_unit unit_type, _) ->
            (minerals_cost unit_type, gas_cost unit_type, - supply unit_type)
        | _ -> (0, 0, 0) in
    let modify_unit ((uid, unit_type, old_state) as unit) =
        let (old_state_minerals_income, old_state_gas_income, old_state_supply_income) =
            match old_state with
            | UsingAbility (Build_unit ut, _) ->
                (minerals_cost ut * 3 / 4, gas_cost ut * 3 / 4, - supply ut)
            | _ -> (0, 0, 0) in
        let md = old_state_minerals_income - state_minerals_cost in
        let gd = old_state_gas_income - state_gas_cost in
        let sd = old_state_supply_income - state_supply_cost in
        if global_state.minerals + md >= 0
            && global_state.gas + gd >= 0
            && supply_available global_state + sd >= 0 then begin
            minerals_diff := md;
            gas_diff := gd;
            is_done := true;
            (uid, unit_type, state)
        end
        else unit in
    let f unit =
        if not !is_done && match_unit pat unit then begin
            modify_unit unit
        end
        else unit in
    let new_units =
        List.map f global_state.all_units in
    (
        !is_done,
        {
            global_state with
            all_units = new_units;
            minerals = global_state.minerals + !minerals_diff;
            gas = global_state.gas + !gas_diff;
        }
    )

type build_order = command list

let unit_type_to_string = function
    | Scv -> "SCV"
    | Command_center -> "Command Center"
    | Refinery -> "Refinery"
    | Supply_depot -> "Supply Depot"
    | Barracks -> "Barracks"
    | Orbital_command -> "Orbital Command"
    | Reactor -> "Reactor"
    | Techlab -> "Tech Lab"
    | Reaper -> "Reaper"

let game_clock_to_string game_clock =
    let secs = game_clock / 10 in
    Printf.sprintf "%d:%02d" (secs / 60) (secs mod 60)

let print_gs ({ game_clock; minerals; gas; all_units } as gs) =
    Printf.printf "time=%d;minerals=%d;gas=%d;sup=%d\nunits:\n" game_clock minerals gas (supply_available gs);
    let ability_to_string = function
        | Build_unit ut -> "Building " ^ unit_type_to_string ut
        | Morph ut -> "Upgrading to " ^ unit_type_to_string ut
        | Gather_minerals -> "Gathering minerals"
        | Gather_gas -> "Gathering gas" in
    let print_unit_state = function
        | Idling -> print_string "Idling"
        | UsingAbility (ability, eta) -> Printf.printf "%s [ETA: %d ticks]"
            (ability_to_string ability) eta in
    let print_unit (uid, ut, state) =
        Printf.printf "[#%d] %s: " uid (unit_type_to_string ut);
        print_unit_state state; print_newline () in
    List.iter print_unit all_units

let rec execute_build_order global_state = function
    | [] -> ()
    | ((_, state) as command) :: rest ->
        let before_exec =
            try step global_state
            with Invalid_argument str ->
                print_gs global_state;
                raise (Invalid_argument str) in
        let (exec, next_gs) = try_execute command before_exec in
        if exec then begin
            let cap = supply_cap before_exec in
            let avail = supply_available before_exec in
            let action = match state with
                | UsingAbility (Build_unit unit_type, _) ->
                    unit_type_to_string unit_type
                | UsingAbility (Morph unit_type, _) ->
                    unit_type_to_string unit_type
                | _ -> "@" in
            Printf.printf "%s %d/%d %d/%d %s\n"
                (game_clock_to_string before_exec.game_clock)
                (cap - avail) cap
                before_exec.minerals before_exec.gas
                action;
            execute_build_order next_gs rest
        end
        else execute_build_order next_gs (command :: rest)

let rec iterate n f init =
    if n <= 0 then []
    else init :: iterate (n - 1) f (f init)

let rec times n f x =
    if n = 0 then x
    else times (n - 1) f (f x)

let () =
    execute_build_order initial_global_state [
        ((None, Some Command_center, Some Idling), use_ability (Build_unit Scv));
        ((None, Some Command_center, Some Idling), use_ability (Build_unit Scv));
        ((None, Some Scv, Some (use_ability Gather_minerals)), use_ability (Build_unit Supply_depot));
        ((None, Some Command_center, Some Idling), use_ability (Build_unit Scv));
        ((None, Some Command_center, Some Idling), use_ability (Build_unit Scv));
        ((None, Some Scv, Some (use_ability Gather_minerals)), use_ability (Build_unit Barracks));
        ((None, Some Scv, Some (use_ability Gather_minerals)), use_ability (Build_unit Refinery));
        ((None, Some Command_center, Some Idling), use_ability (Build_unit Scv));
        ((None, Some Command_center, Some Idling), use_ability (Build_unit Scv));
        ((None, Some Command_center, Some Idling), use_ability (Build_unit Scv));
        ((None, Some Command_center, Some Idling), use_ability (Morph Orbital_command));
        ((None, Some Barracks, Some Idling), use_ability (Build_unit Reaper));
        ((None, Some Scv, Some (use_ability Gather_minerals)), use_ability (Build_unit Command_center));
        ((None, Some Orbital_command, Some Idling), use_ability (Build_unit Scv));
        ((None, Some Orbital_command, Some Idling), use_ability (Build_unit Scv));
    ]