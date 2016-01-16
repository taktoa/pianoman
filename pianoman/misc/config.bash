#!/usr/bin/env bash
# -*- mode: sh; sh-shell: bash; coding: utf-8; -*-

# ------------------------------------------------------------------------------

# ### File: misc/config.bash
# #
# ### License:
# #
# # Copyright © 2016 Remy Goldschmidt <taktoa@gmail.com>
# #
# # The MIT License
# #
# # Permission is hereby granted, free of charge, to any person obtaining a copy
# # of this software and associated documentation files (the "Software"), to
# # deal in the Software without restriction, including without limitation the
# # rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
# # sell copies of the Software, and to permit persons to whom the Software is
# # furnished to do so, subject to the following conditions:
# #
# # The above copyright notice and this permission notice shall be included in
# # all copies or substantial portions of the Software.
# #
# # THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# # IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# # FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# # AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# # LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# # FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# # IN THE SOFTWARE.
# #
# ### Created:    2016/01/08
# #
# ### Author:     Remy Goldschmidt <taktoa@gmail.com>
# ### Maintainer: Remy Goldschmidt <taktoa@gmail.com>
# ### Maintainer: Michael Bishop <cleverca22@gmail.com>
# #
# ### Commentary:
# #
# # When building PianoMan via Nix, this file has '@'-enclosed names replaced
# # by their Nix values. For example, if default.nix contains,
# #
# #     # ...
# #     stdenv.mkDerivation rec {
# #       # ...
# #       foo = "bar";
# #       # ...
# #     }
# #
# # then any instances of '@foo@' will be replaced by 'bar'.
# #
# # Obviously, this is a fairly easy-to-use interface for anyone looking to
# # build PianoMan in other ways; just use your favorite regex tool or copy the
# # definition of substituteAllInPlace from the Nixpkgs source code.
# #
# ### Code:


# ------------------------------------------------------------------------------
# --- General ------------------------------------------------------------------
# ------------------------------------------------------------------------------


#
PROJECT="PianoMan"
PROJECT_HOME_PAGE="FIXME"
BUG_REPORT_EMAIL="FIXME"

OUTPUT_ROOT="@out@"

export PROJECT PROJECT_HOME_PAGE BUG_REPORT_EMAIL \
       OUTPUT_ROOT
