# Copyright © 2017 Iain Nicol

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

PREFIX ?= $$HOME/.local

.PHONY: all
all:
	stack build

.PHONY: test
test:
	stack test

.PHONY: install
install:
	@SRC="$$(find .stack-work/install -type f -executable \
	                                          -wholename "*/bin/bscc" \
	                                          -print0 \
	                | xargs -0 stat --format="%Y %n" \
	                | sort -nr \
	                | cut -d' ' -f2- \
	                | head -n1)" && \
	    DEST="${PREFIX}/bin/bscc" && \
	    if [ -n "$$SRC" ]; then \
	        echo mkdir -p "${PREFIX}/bin"; \
	        mkdir -p "${PREFIX}/bin"; \
	        echo cp "$$SRC" "$$DEST"; \
	        cp "$$SRC" "$$DEST"; \
	    else \
	        echo "Cannot find built bscc to install"; \
	        false; \
	    fi
	mkdir -p "${PREFIX}/share/brainsick"
	cp --parents libbsccts/startup.ll "${PREFIX}/share/brainsick"
