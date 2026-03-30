{-# LANGUAGE OverloadedStrings
           , LambdaCase
           , OverloadedRecordDot
           , NoFieldSelectors
           , DuplicateRecordFields
           , BlockArguments
           #-}

import Data.Text qualified as Text
import Data.IntMap.Strict qualified as Map
import Control.Monad.State (gets, modify, runStateT, MonadState, StateT(StateT))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))

type Ticks = Int -- 10 ticks per second

type UnitID = Int

data UnitType
  = Scv
  | CommandCenter
  | OrbitalCommand
  | Refinery
  | SupplyDepot
  | Barracks
  | Reactor
  | TechLab
  | Reaper
  deriving (Eq, Ord)

data UnitDesc = UnitDesc
  { name :: Text.Text
  , mineralsCost :: Int
  , gasCost :: Int
  , buildTime :: Int
  , supply :: Int
  }

unitDesc :: UnitType -> UnitDesc
unitDesc = \case
  CommandCenter -> UnitDesc "Command Center" 400 0 710 15
  OrbitalCommand -> UnitDesc "Orbital Command" 150 0 250 15
  Refinery -> UnitDesc "Refinery" 75 0 210 0
  Barracks -> UnitDesc "Barracks" 150 0 460 0
  Reactor -> UnitDesc "Reactor" 50 50 360 0
  TechLab -> UnitDesc "Tech Lab" 50 25 180 0

  Scv -> UnitDesc "SCV" 50 0 120 (-1)
  Reaper -> UnitDesc "Reaper" 50 50 320 (-1)

data Ability
  = Build UnitType
  | Morph UnitType
  | GatherMinerals
  | GatherGas
  deriving Eq

data AbilityDesc = AbilityDesc
  { name :: Text.Text
  , effect :: Effect
  , delay :: Int
  , coolDown :: Int
  }

data Effect
  = CreateUnit UnitType
  | MorphUnit UnitType
  | AddMinerals Int
  | AddGas Int

abilityDesc :: Ability -> AbilityDesc
abilityDesc = \case
  Build ut -> AbilityDesc ("Build " <> (unitDesc ut).name) (CreateUnit ut) (unitDesc ut).buildTime 0
  Morph ut -> AbilityDesc ("Morph to " <> (unitDesc ut).name) (MorphUnit ut) (unitDesc ut).buildTime 0
  GatherMinerals -> AbilityDesc "Gather minerals" (AddMinerals 5) 56 0
  GatherGas -> AbilityDesc "Gather gas" (AddGas 4) 42 0

data UnitState = UnitState
  { ty :: UnitType
  , abilState :: AbilityState
  }
  deriving Eq

data AbilityState
  = Idling
  | Casting Ability AbilityStage
  deriving Eq

data AbilityStage
  = Delay Int
  | CoolDown Int
  deriving Eq

initAbilState :: Ability -> AbilityState
initAbilState ab = Casting ab (Delay (abilityDesc ab).delay)

data Unit = Unit
  { id :: UnitID
  , state :: UnitState
  }

data UnitPattern
  = (:&) UnitPattern UnitPattern
  | (:|) UnitPattern UnitPattern
  | UPType UnitType
  | UPAbilState AbilityState

matchUnit :: UnitPattern -> UnitState -> Bool
matchUnit (a :& b) u =  matchUnit a u && matchUnit b u
matchUnit (a :| b) u =  matchUnit a u || matchUnit b u
matchUnit (UPType t) u = t == u.ty
matchUnit (UPAbilState s) u = s == u.abilState

data GameState = GameState
  { minerals :: Int
  , gas :: Int
  , units :: Map.IntMap UnitState
  , clock :: Int
  }

newtype Game a = Game (StateT GameState IO a)
  deriving (Functor, Applicative, Monad, MonadState GameState, MonadIO)

runGame :: Game a -> GameState -> IO (a, GameState)
runGame (Game g) s = runStateT g s

openingState :: GameState
openingState = GameState 50 0 units 0
  where units = Map.fromList
          [ (0, UnitState CommandCenter Idling)
          , (1, UnitState Scv (initAbilState GatherMinerals))
          , (2, UnitState Scv (initAbilState GatherMinerals))
          , (3, UnitState Scv (initAbilState GatherMinerals))
          , (4, UnitState Scv (initAbilState GatherMinerals))
          , (5, UnitState Scv (initAbilState GatherMinerals))
          , (6, UnitState Scv (initAbilState GatherMinerals))
          , (7, UnitState Scv (initAbilState GatherMinerals))
          , (8, UnitState Scv (initAbilState GatherMinerals))
          , (9, UnitState Scv (initAbilState GatherMinerals))
          , (10, UnitState Scv (initAbilState GatherMinerals))
          , (11, UnitState Scv (initAbilState GatherMinerals))
          , (12, UnitState Scv (initAbilState GatherMinerals))
          ]

supplyAvailable :: Game Int
supplyAvailable = sum . map toSupply . Map.toList <$> gets (.units)
  where
  toSupply (_, u) = (unitDesc u.ty).supply + stateSupply
    where
    stateSupply = case u.abilState of
      Casting (Build ty') (Delay _) -> (unitDesc ty').supply
      _ -> 0

supplyCap :: Game Int
supplyCap = sum . filter (> 0) . map toSupply . Map.toList <$> gets (.units)
  where
  toSupply (_, u) = (unitDesc u.ty).supply

findUnits :: UnitPattern -> Game [UnitID]
findUnits p = map fst . filter (matchUnit p . snd) . Map.toList <$> gets (.units)

countUnits :: UnitPattern -> Game Int
countUnits = fmap length <$> findUnits

nextUnitID :: Game Int
nextUnitID = maybe 0 ((+ 1) . fst) . Map.lookupMax <$> gets (.units)

modifyUnit :: UnitID -> (UnitState -> (a, UnitState)) -> Game a
modifyUnit i f = do
  old <- gets (.units)
  let (r, u) = f (old Map.! i)
  modify \s -> s { units = Map.insert i u old }
  pure r

modifyUnit' :: UnitID -> (UnitState -> UnitState) -> Game ()
modifyUnit' i f = modifyUnit i (((), ) <$> f)

addUnit :: UnitType -> Game UnitID
addUnit ty = do
  i <- nextUnitID
  modify \s -> s { units = Map.insert i (UnitState ty Idling) s.units }
  pure i

getUnit :: UnitID -> Game UnitState
getUnit i = (Map.! i) <$> gets (.units)

setUnit :: UnitID -> UnitState -> Game ()
setUnit i s = modifyUnit i (const ((), s))

setAbilState :: UnitID -> AbilityState -> Game ()
setAbilState uid s = modifyUnit' uid (\u -> u { abilState = s })

tick :: Game ()
tick = do
  ids <- map fst . Map.toList <$> gets (.units)
  mapM_ tickUnit ids
  modify (\s -> s { clock = s.clock + 1 })
  where
    tickUnit :: UnitID -> Game ()
    tickUnit uid = (.abilState) <$> getUnit uid >>= \case
      Idling -> pure ()
      Casting ab (Delay n) | n > 0 ->
        setAbilState uid (Casting ab (Delay (n - 1)))
      Casting ab (Delay 0) -> do
        let cd = (abilityDesc ab).coolDown
        if cd == 0
          then setAbilState uid Idling
          else setAbilState uid (Casting ab (CoolDown (abilityDesc ab).coolDown))
        case (abilityDesc ab).effect of
          CreateUnit t -> void $ addUnit t
          MorphUnit t -> setUnit uid (UnitState t Idling)
          AddMinerals m -> modify (\s -> s { minerals = s.minerals + m })
          AddGas g -> modify (\s -> s { gas = s.gas + g })
      Casting ab (CoolDown n) | n > 0 ->
        setAbilState uid (Casting ab (CoolDown (n - 1)))
      Casting ab (CoolDown 0) ->
        setAbilState uid Idling

data AIStepResult a where
  Wait :: Game Bool -> AI a -> AIStepResult a
  Halt :: AIStepResult a
  Pure :: a -> AIStepResult a
  deriving Functor

newtype AI a = AI { unAI :: Game (AIStepResult a) }

instance Functor AI where
  fmap f = AI . fmap f' . (.unAI)
    where
    f' = \case
      Wait c r -> Wait c (fmap f r)
      Halt -> Halt
      Pure a -> Pure (f a)

instance Applicative AI where
  pure = AI . pure . Pure
  f <*> a = AI $ f.unAI >>= \case
    Wait c r -> pure $ Wait c do
      f' <- r
      fmap f' a
    Halt -> a.unAI >> pure Halt
    Pure f' -> (fmap f' a).unAI

instance Monad AI where
  m >>= f = AI $ m.unAI >>= \case
    Wait c r -> pure . Wait c $ r >>= f
    Halt -> pure Halt
    Pure a -> (f a).unAI

runAIStep :: AI a -> Game (AIStepResult a)
runAIStep = (.unAI)

game :: Game a -> AI a
game = AI . fmap Pure

wait :: Game Bool -> AI ()
wait c = AI . pure . Wait c $ pure ()

halt :: AI a
halt = AI . pure $ Halt

data AIManager = AIManager
  { waiters :: [(Game Bool, AI ())]
  }

stepAIs :: AIManager -> Game AIManager
stepAIs mgr = AIManager . concat <$> mapM step mgr.waiters
  where
  step (c, k) = c >>= \case
    True -> runAIStep k >>= \case
      Halt -> pure []
      Wait c' k' -> pure [(c', k')]
      Pure _ -> pure []
    False -> pure [(c, k)]

scvAI :: UnitID -> AI ()
scvAI uid = do
  wait do
    (.abilState) <$> getUnit uid >>= \case
      Idling -> pure True
      _ -> pure False
  game $ setAbilState uid (initAbilState GatherMinerals)
  scvAI uid

main = runGame (loop (AIManager [(pure True, scvAI 1)])) $ openingState
  where
  loop ai = do
    ai' <- stepAIs ai
    tick
    liftIO . print =<< gets (.minerals)
    loop ai'

{-

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
        match_unit (Pat_state (use_ability Gather_minerals)) in
    let is_gathering_gas =
        match_unit (Pat_state (use_ability Gather_gas)) in
    let is_base unit =
        match_unit (Pat_type Command_center) unit
        || match_unit (Pat_type Orbital_command) unit in
    let is_refinery =
        match_unit (Pat_type Refinery) in
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
        if not (match_unit (Pat_and (Pat_type Scv, Pat_state Idling)) unit) then
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
        else (execute_build_order next_gs (command :: rest); print_string "wait for next tick.\n")

let rec iterate n f init =
    if n <= 0 then []
    else init :: iterate (n - 1) f (f init)

let rec times n f x =
    if n = 0 then x
    else times (n - 1) f (f x)

let () =
    execute_build_order initial_global_state [
        (Pat_and (Pat_type Command_center, Pat_state Idling), use_ability (Build_unit Scv));
        (Pat_and (Pat_type Command_center, Pat_state Idling), use_ability (Build_unit Scv));
        (Pat_and (Pat_type Scv, Pat_state (use_ability Gather_minerals)), use_ability (Build_unit Supply_depot));
        (Pat_and (Pat_type Command_center, Pat_state Idling), use_ability (Build_unit Scv));
        (Pat_and (Pat_type Command_center, Pat_state Idling), use_ability (Build_unit Scv));
        (Pat_and (Pat_type Scv, Pat_state (use_ability Gather_minerals)), use_ability (Build_unit Barracks));
        (Pat_and (Pat_type Scv, Pat_state (use_ability Gather_minerals)), use_ability (Build_unit Refinery));
        (Pat_and (Pat_type Command_center, Pat_state Idling), use_ability (Build_unit Scv));
        (Pat_and (Pat_type Command_center, Pat_state Idling), use_ability (Build_unit Scv));
        (Pat_and (Pat_type Command_center, Pat_state Idling), use_ability (Build_unit Scv));
        (Pat_and (Pat_type Command_center, Pat_state Idling), use_ability (Morph Orbital_command));
        (Pat_and (Pat_type Barracks, Pat_state Idling), use_ability (Build_unit Reaper));
        (Pat_and (Pat_type Scv, Pat_state (use_ability Gather_minerals)), use_ability (Build_unit Command_center));
        (Pat_and (Pat_type Orbital_command, Pat_state Idling), use_ability (Build_unit Scv));
        (Pat_and (Pat_type Orbital_command, Pat_state Idling), use_ability (Build_unit Scv));
    ]

-}
