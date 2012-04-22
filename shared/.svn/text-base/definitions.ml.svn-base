type timer = float
type score = int
type color = Red | Blue
type game_result = Winner of color | Tie

type unit_id = int
type building_id = int
type health = int
type resource_count = int

type vector = float * float
type tile = int * int (* (row, column) *)
type position = vector

type food_count = resource_count
type wood_count = resource_count

module Unit_map = Map.Make(struct type t = unit_id let compare = compare end)
module Building_map =
  Map.Make(struct type t = building_id let compare = compare end)

type attackable_object = Building of building_id | Unit of unit_id

type unit_type = Villager | Knight | Archer | Pikeman | EliteKnight | EliteArcher | ElitePikeman

type building_type = TownCenter | Barracks

type resource_type = Food | Wood

type age = DarkAge | ImperialAge

type upgrade_type = AgeUpgrade | UnitUpgrade of unit_type

(* Graphics updates *)
type update =
    AddBuilding of building_id * building_type * tile * health *  color |
    AddUnit of unit_id * unit_type * position * health * color |
    Countdown of int |
    DisplayString of color * string |
    DoAttack of unit_id |
    DoBuild of tile * color| (* TODO: remove this update? I think its never used *)
    DoCollect of unit_id * color * resource_type * int |
    GameOver of game_result |
    InitGraphics |
    InitFood of tile list |
    InitWood of tile list |
    MoveUnit of unit_id * position list * color |
    RemoveUnit of unit_id |
    RemoveBuilding of building_id |
    RemoveResource of tile |
		StopUnit of unit_id * color * position |
    UpdateScore of color * score |
		UpgradeAge of color |
    UpdateBuilding of building_id * health |
    UpdateResource of tile * resource_count |
    UpdateUnit of unit_id * health |
    UpgradeUnit of unit_type * color

(* Control messages *)
type control =
    GameStart |
    GameRequest |
    Team of color |
    GameEnd

(* Actions for AI to do *)
(* subject to change *)
type action =
    QueueAttack of unit_id * attackable_object |
    QueueCollect of unit_id |
    QueueMove of unit_id * vector |
    QueueBuild of unit_id * building_type |
    QueueSpawn of building_id * unit_type |
    ClearAttack  of unit_id |
		ClearMove of unit_id |
    Upgrade of upgrade_type |
    Talk of string

type upgrades = bool * bool * bool (*ElitePikeman * EliteArcher * EliteKnight*)

type unit_data = unit_id * unit_type * health * position
type building_data = building_id * building_type * health * tile
type team_data = score * unit_data list * building_data list * age * food_count * wood_count * upgrades
type game_data = team_data * team_data * timer
type resource_data = tile * resource_type * int

type result = Success | Failed

(* type for clients to request information *)
type status =
    UnitStatus of unit_id |
    BuildingStatus of building_id |
    TeamStatus of color |
		ResourceStatus |
    GameStatus

(* types that are returned to clients after status request *)
type data =
    UnitData of unit_data |
    BuildingData of building_data |
    TeamData of team_data |
    GameData of game_data |
		ResourceData of resource_data list |
    NoData

type command =
    Control of control |
    Action of action |
    Status of status |
    Data of data |
    Result of result |
    Error of string
