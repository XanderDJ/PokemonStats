module Pokemon.Replays.Types where

import Data.Text (Text)

data ReplayMessage
  = RMJ Join
  | RML Leave
  | RMChat Chat
  | RMHTML HTML
  | RMRaw Raw
  | RMPL Player
  | RMTS TeamSize
  | RMGT GameType
  | RMG Gen
  | RMT Tier
  | RMR Rated
  | RMRule Rule
  | RMCP ClearPokemon
  | RMTP TeamPreview
  | RMPoke Poke
  | RMS Start
  | RMD Delimiter
  | RMInactive Inactive
  | RMInactiveOff InactiveOff
  | RMU Upkeep
  | RMTurn Turn
  | RMW Win
  | RMTie Tie
  | RMTimeStamp TimeStamp
  | RMMove Move
  | RMSwitch Switch
  | RMDrag Drag
  | RMDC DetailsChange
  | RMFC FormeChange
  | RMReplace Replace
  | RMSwap Swap
  | RMCant Cant
  | RMF Faint
  | RMFailed Failed
  | RMB Block
  | RMNT NoTarget
  | RMMiss Miss
  | RMDmg Damage
  | RMH Heal
  | RMSHP SetHP
  | RMStatus Status
  | RMCS CureStatus
  | RMCT CureTeam
  | RMBoost Boost
  | RMUnboost Unboost
  | RMSetBoost SetBoost
  | RMSwapBoost SwapBoost
  | RMInvertBoost InvertBoost
  | RMCB ClearBoost
  | RMCAB ClearAllBoosts
  | RMCPB ClearPositiveBoost
  | RMCNB ClearNegativeBoost
  | RMCopyBoost CopyBoost
  | RMWeather Weather
  | RMFS FieldStart
  | RMFA FieldActivate
  | RMFE FieldEnd
  | RMSS SideStart
  | RMSE SideEnd
  | RMVS VolatileStart
  | RMVE VolatileEnd
  | RMC Crit
  | RMSuperEffective SuperEffective
  | RMResist Resisted
  | RMImmune Immune
  | RMI Item
  | RMEI EndItem
  | RMA Ability
  | RMEA EndAbility
  | RMTransform Transform
  | RMMega Mega
  | RMPrimal Primal
  | RMBurst Burst
  | RMZM ZMove
  | RMZB ZBroken
  | RMActivate Activate
  | RMHint Hint
  | RMCenter Center
  | RMMessage Message
  | RMCombine Combine
  | RMWaiting Waiting
  | RMPrepare Prepare
  | RMMustRecharge MustRecharge
  | RMNothing Nothing
  | RMHC HitCount
  | RMSM SingleMove
  | RMST SingleTurn
  | RMName Name
  | RMAnim Anim
  | RMUnsupported Unsupported
  deriving (Show, Eq)

-- | Join player
newtype Join = Join Text deriving (Show, Eq)

-- | Leave player
newtype Leave = Leave Text deriving (Show, Eq)

-- | n Player
newtype Name = N Text deriving (Show, Eq)

-- | Chat player message
data Chat = Chat Text Text deriving (Show, Eq)

-- | HTML html
newtype HTML = HTML Text deriving (Show, Eq)

-- | Raw html
newtype Raw = Raw Text deriving (Show, Eq)

-- | Position Player
data Position = P1 | P2 | P3 | P4 deriving (Show, Eq)

-- | P position name avatar ranking
data Player = Pl Position (Maybe Text) (Maybe Text) (Maybe Int) deriving (Show, Eq)

-- | TS playerPosition size
data TeamSize = TS Text Int deriving (Show, Eq)

-- | Gametype of the replay. Needed sometimes to determine position, but not really
data GameType = SINGLES | DOUBLES | TRIPLES | MULTI | FFA deriving (Show, Eq)

-- | Gen generation
newtype Gen = Gen Int deriving (Show, Eq)

-- | Tier tierFormat
newtype Tier = Tier Text deriving (Show, Eq)

-- | Rated (maybe official tournament)
newtype Rated = RATED (Maybe Text) deriving (Show, Eq)

-- | Rule (name of rule + description)
newtype Rule = Rule Text deriving (Show, Eq)

-- | Start of preview
data ClearPokemon = ClearPoke deriving (Show, Eq)

-- | Teampreview
data TeamPreview = TeamPreview deriving (Show, Eq)

-- | Poke (player position) (name) (maybe item)
data Poke = Poke Position Text (Maybe Text) deriving (Show, Eq)

-- | Start of match
data Start = START deriving (Show, Eq)

-- | Separates messages, shows boundaries
data Delimiter = DELIMITER deriving (Show, Eq)

-- | Inactive message. This means the timer is on and a timer related message is shown
newtype Inactive = Inactive Text deriving (Show, Eq)

-- | InactiveOff message. This means timer is off
newtype InactiveOff = InactiveOff Text deriving (Show, Eq)

-- | Signals upkeep phase where turns left for field conditions is updated
data Upkeep = UPKEEP deriving (Show, Eq)

-- | Turn number
newtype Turn = Turn Int deriving (Show, Eq)

-- | Win player
newtype Win = Win Text deriving (Show, Eq)

-- | Game ended in a tie
data Tie = TIE deriving (Show, Eq)

newtype TimeStamp = TimeStamp Int deriving (Show, Eq)

-- | Tag sometimes added to an effect in replays
data Tag = Of Text | From Text | MISS | STILL | NOTARGET | ANIM Text | SILENT | MSG | UK | EAT | WISHER Text | IDENTIFY | WEAKEN | DAMAGE | PARTIALLYTRAPPED | CONSUMED deriving (Show, Eq)

-- | Move pokemon movename target potential tags added
data Move = Move Text Text Text [Tag] deriving (Show, Eq)

-- | Switch nickname name currentHp tags
data Switch = Switch Text Text Int [Tag] deriving (Show, Eq)

-- | Drag nickname name currentHp tags
data Drag = Drag Text Text Int [Tag] deriving (Show, Eq)

-- | DetailsChange nickname name hp tags
data DetailsChange = DetailsChange Text Text [Tag] deriving (Show, Eq)

-- | FormeChange nickname name tags
data FormeChange = FormeChange Text Text [Tag] deriving (Show, Eq)

-- | Replace nickname name tags
data Replace = Replace Text Text [Tag] deriving (Show, Eq)

-- | Swap nickname position tags
data Swap = Swap Text Text [Tag] deriving (Show, Eq)

-- | Cant nickname reason (maybe move)
data Cant = Cant Text Text (Maybe Text) deriving (Show, Eq)

-- | Faint nickname
newtype Faint = Faint Text deriving (Show, Eq)

-- | Failed nickname move
data Failed = Failed Text (Maybe Text) deriving (Show, Eq)

-- | Block nickname effect move attacker TODO: check if this uses tags instead
data Block = Block Text Text Text Text [Tag] deriving (Show, Eq)

-- | NoTarget nickname
newtype NoTarget = NoTarget Text deriving (Show, Eq)

-- | Miss source (maybe action)
data Miss = Miss Text (Maybe Text) deriving (Show, Eq)

-- | Damage nickname currentHp tags
data Damage = Damage Text Int [Tag] deriving (Show, Eq)

-- | Heal nickname currentHp tags
data Heal = Heal Text Int [Tag] deriving (Show, Eq)

-- | SetHP nickname hp
data SetHP = SetHP Text Int [Tag] deriving (Show, Eq)

-- | Status nickname (status abbreviation)
data Status = Status Text Text deriving (Show, Eq)

-- | CureStatus nickname status
data CureStatus = CureStatus Text Text deriving (Show, Eq)

-- | CureTeam nickname
newtype CureTeam = CureTeam Text deriving (Show, Eq)

-- | Boost nickname stat amount
data Boost = Boost Text Text Int deriving (Show, Eq)

-- | Unboost nickname stat amount
data Unboost = Unboost Text Text Int deriving (Show, Eq)

-- | SetBoost nickname stat amount
data SetBoost = SetBoost Text Text Int deriving (Show, Eq)

-- | SwapBoost source target tags
data SwapBoost = SwapBoost Text Text [Text] [Tag] deriving (Show, Eq)

-- | InvertBoost nickname tags
data InvertBoost = InvertBoost Text [Tag] deriving (Show, Eq)

-- | ClearBoost nickname
newtype ClearBoost = ClearBoost Text  deriving (Show, Eq)

-- | ClearAllBoosts
data ClearAllBoosts = CLEARALLBOOSTS deriving (Show, Eq)

-- | ClearPositiveBoost target source effect
data ClearPositiveBoost = ClearPositiveBoost Text Text Text deriving (Show, Eq)

-- | ClearNegativeBoost target source effect
newtype ClearNegativeBoost = ClearNegativeBoost Text deriving (Show, Eq)

-- | CopyBoost Source Target tags
data CopyBoost = CopyBoost Text Text [Tag] deriving (Show, Eq)

-- | Weather effect tags, Upkeep means weather was already in effect
data Weather = Weather Text [Tag] deriving (Show, Eq)

-- | FieldStart condition tags
data FieldStart = FieldStart Text [Tag] deriving (Show, Eq)

-- | FieldActivate Effect
newtype FieldActivate = FieldActivate Text deriving (Show, Eq)

-- | FieldEnd condition tags
data FieldEnd = FieldEnd Text [Tag] deriving (Show, Eq)

-- | SideStart side condition tags
data SideStart = SideStart Text Text deriving (Show, Eq)

-- | SideEnd side condition tags
data SideEnd = SideEnd Text Text deriving (Show, Eq)

-- | VolatileStart nickname effect tags. Used for volatile status
data VolatileStart = VolatileStart Text Text deriving (Show, Eq)

-- | VolatileEnd nickname effect tags. Used for volatile status (happens when the effect depends on the pokemon being there)
data VolatileEnd = VolatileEnd Text Text deriving (Show, Eq)

-- | Crit nickname
newtype Crit = Crit Text deriving (Show, Eq)

-- | SuperEffective nickname
newtype SuperEffective = SuperEffective Text deriving (Show, Eq)

-- | Resisted nickname
newtype Resisted = Resisted Text deriving (Show, Eq)

-- | Immune nickname
newtype Immune = Immune Text deriving (Show, Eq)

-- | Item pokemon item tags
data Item = Item Text Text [Tag] deriving (Show, Eq)

-- | EndItem pokemon item tags
data EndItem = EndItem Text Text [Tag] deriving (Show, Eq)

-- | Ability pokemon ability tags
data Ability = Ability Text Text [Tag] deriving (Show, Eq)

-- | Endability Pokemon tags
data EndAbility = EndAbility Text [Tag] deriving (Show, Eq)

-- | Transform pokemon newForm
data Transform = Transform Text Text [Tag] deriving (Show, Eq)

-- | Mega pokemon megaStone
data Mega = Mega Text Text deriving (Show, Eq)

-- | Primal pokemon
newtype Primal = Primal Text deriving (Show, Eq)

-- | Burst pokemon species item
data Burst = Burst Text Text Text deriving (Show, Eq)

-- | ZMove pokemon tags
data ZMove = ZMove Text [Tag] deriving (Show, Eq)

-- | ZBroken pokemon
newtype ZBroken = ZBroken Text deriving (Show, Eq)

-- | Activate effect tags
data Activate = Activate Text Text [Tag] deriving (Show, Eq)

newtype Hint = Hint Text deriving (Show, Eq)

data Center = CENTER deriving (Show, Eq)

newtype Message = Message Text deriving (Show, Eq)

data Combine = COMBINE deriving (Show, Eq)

-- | Waiting target source
data Waiting = Waiting Text Text deriving (Show, Eq)

-- | Prepare attacker move (maybe defender)
data Prepare = Prepare Text Text (Maybe Text) deriving (Show, Eq)

-- | MustRecharge pokemon
newtype MustRecharge = MustRecharge Text deriving (Show, Eq)

data Nothing = NOTHING deriving (Show, Eq)

-- | Hitcount pokemon count
data HitCount = HitCount Text Int deriving (Show, Eq)

-- | SingleMove pokemon move
data SingleMove = SingleMove Text Text deriving (Show, Eq)

-- | SingleTurn pokemon move
data SingleTurn = SingleTurn Text Text deriving (Show, Eq)

-- | Anim Nick move nick
data Anim = Anim Text Text Text deriving (Show, Eq) 

newtype Unsupported = Unsupported Text deriving (Show, Eq)