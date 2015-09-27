module Embot.Commands.Dice where

import           ClassyPrelude
import           Control.Lens (view)
import           Control.Lens.TH (makeLenses)
import           Data.Attoparsec.Text (Parser, decimal, endOfInput, skipSpace)
import           Data.Monoid (Any(Any, getAny))
import           System.Random (randomRIO)
import           TextShow (TextShow(showb), showt)

import           Embot.Action (Action(SendMessage))
import           Embot.Command (Command, commandChat, commandData, actionCommand)
import           Embot.Commands.Help (helpFor)
import           Embot.Slack (RtmStartRp)
import           Embot.Types (EmbotLogic, MinimalEmbotMonad)

data RollSpec = RollSpec
  { _rollQuantity  :: Int
  , _rollSides     :: Int
  , _rollConst     :: Int
  , _rollMinTarget :: Maybe Int
  , _rollMaxTarget :: Maybe Int
  , _rollThreshold :: Maybe Int }

makeLenses ''RollSpec

instance TextShow RollSpec where
  showb (RollSpec { .. }) = showb _rollQuantity <> "d" <> showb _rollSides
                         <> maybe "" ((">" <>) . showb) _rollMinTarget
                         <> maybe "" (("<" <>) . showb) _rollMaxTarget
                         <> maybe "" (("?" <>) . showb) _rollThreshold

trigger :: Parser ()
trigger = "roll" *> (skipSpace <|> endOfInput)

parser :: Parser RollSpec
parser = "roll" *> skipSpace *> rollSpec <* skipSpace <* endOfInput
  where
    rollSpec :: Parser RollSpec
    rollSpec = RollSpec <$> decimal
                        <*> ("d" *> decimal)
                        <*> ("+" *> decimal <|> pure 0)
                        <*> (">" *> (Just <$> decimal) <|> pure Nothing)
                        <*> ("<" *> (Just <$> decimal) <|> pure Nothing)
                        <*> ("?" *> (Just <$> decimal) <|> pure Nothing)

roll :: forall m. MinimalEmbotMonad m => RtmStartRp -> EmbotLogic m
roll rtmStartRp = actionCommand rtmStartRp trigger parser respond
  where
    respond :: Command RollSpec -> m (Seq Action)
    respond cmd = do
      let spec = view commandData cmd
      dice <- mapM (const . liftIO $ randomRIO (1, view rollSides spec)) [1 .. view rollQuantity spec]
      let subtotal :: Int
          subtotal = sum dice

          total :: Int
          total = subtotal + view rollConst spec

          totalSuccess :: Maybe Bool
          totalSuccess = getAny <$> foldMap (Any <$>) [ (total >=) <$> view rollMinTarget spec
                                                      , (total <=) <$> view rollMaxTarget spec ]

          successes :: Maybe [Int]
          successes = (\ threshold -> filter (>= threshold) dice) <$> view rollThreshold spec

          diceText :: Text
          diceText = "Rolled: " <> intercalate " " (map showt dice)

          successesText :: Maybe Text
          successesText = (\ rolls -> showt (length rolls) <> " successes: " <> intercalate " " (map showt rolls)) <$> successes

          totalText :: Text
          totalText = "Total: " <> showt subtotal
                   <> case view rollConst spec of 0 -> ""; c -> "+" <> showt c <> "=" <> showt total
                   <> maybe "" (\ b -> ", a " <> if b then "success" else "failure") totalSuccess

      pure . singleton . SendMessage (view commandChat cmd) . intercalate "\n" . catMaybes $ [Just diceText, successesText, Just totalText]

help :: MinimalEmbotMonad m => RtmStartRp -> EmbotLogic m
help rtmStartRp = helpFor rtmStartRp "roll" "Roll some dice."
         $ [ "Simple roll: @embot roll 4d6"
           , "Success threshold (per die): @embot roll 4d6?3"
           , "Roll, total, add something, and check if it's at least a minimum: @embot roll 3d6+2>10" ]

effects :: MinimalEmbotMonad m => RtmStartRp -> EmbotLogic m
effects rtmStartRp = foldMap ($ rtmStartRp) [roll, help]
