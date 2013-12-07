-- Copyright © 2012 Iain Nicol

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | In a fairly general way, this module provides details about this
-- specific package.  It is a bit analogous to config.h in Autotools
-- projects.  Additionally, it provides the means to determine the paths
-- of our data resources.
module Bscc.ThisPackage.Info (bugsUrl, copyright, homepage, version) where

-- We cannot import anything not in base.  This is because we are a
-- dependency of Bscc.HelpAndVersion.

-- | @bug-reports@ field from our .cabal file.
bugsUrl :: String
bugsUrl = "http://www.brainsick.cc/bugs/"

-- | @copyright@ field from our .cabal file.
copyright :: String
copyright = "© 2012, 2013 Iain Nicol"

-- | @homepage@ field from our .cabal file.
homepage :: String
homepage = "http://www.brainsick.cc/"

-- | @version@ field from our .cabal file.
version :: String
version = "0.2.0"
