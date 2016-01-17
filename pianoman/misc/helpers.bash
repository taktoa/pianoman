#!/usr/bin/env bash
# -*- mode: sh; sh-shell: bash; coding: utf-8; -*-

# ------------------------------------------------------------------------------

# ### File: misc/helpers.bash
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
# # Helper functions for the PianoMan scripts.
# #
# ### Code:


# ------------------------------------------------------------------------------
# --- General ------------------------------------------------------------------
# ------------------------------------------------------------------------------


source @pianomanConfig@

set-pianoman-environment-variables () {
    [[ -z "${1}" ]] && {
        echo "set-pianoman-environment-variables: no TeamSpeak path given"
        return 255
    }

    unset DBUS_SESSION_BUS_ADDRESS
    export TSDIR="${1}"
    export HOME="${TSDIR}"
    export PULSE_SERVER="unix:${TSDIR}/native"
    export PULSE_RUNTIME_PATH="${TSDIR}"
    return 0
}

newline () { echo; }

usage-option () {
    echo -e "  ${1}, ${2}\t${3}";
}

usage-param () {
    echo -e "  ${1}\t${2}"
}

usage-header () {
    echo "Usage: ${1}"
    shift 1
    for x in "${@}"; do echo "${x}"; done
    newline
}

help-options () {
    newline
    usage-option "-h" "--help"    "display this help and exit"
    usage-option "-v" "--version" "display version information and exit"
    newline
}

usage-footer () {
    error () { echo "${2}"; exit "${1}"; }
    test -z "${BUG_REPORT_EMAIL}"  && error 1 "BUG_REPORT_EMAIL not set."
    test -z "${PROJECT_HOME_PAGE}" && error 2 "PROJECT_HOME_PAGE not set."
    test -z "${PROJECT}"           && error 3 "PROJECT not set."
    newline
    echo "Report bugs to: ${BUG_REPORT_EMAIL}"
    echo "${PROJECT} home page: <${PROJECT_HOME_PAGE}>"
}

align () {
    echo
}

handle-teamspeak-path () {
  set-pianoman-environment-variables "${1}"
}
