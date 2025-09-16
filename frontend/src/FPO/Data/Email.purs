module FPO.Data.Email where

import Data.Either (Either(..))
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)

-- | Stricter email validation that requires a proper domain with TLD.
-- | This validates that the email has:
-- | - A local part (before @)
-- | - A domain name (after @)
-- | - A top-level domain (at least 2 characters after the last dot)
isValidEmailStrict :: String -> Boolean
isValidEmailStrict email =
  case regex "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" noFlags of
    Right r -> test r email
    Left _ -> false
