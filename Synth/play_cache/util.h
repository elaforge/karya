// Copyright 2023 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once

#include <string>


namespace util {

static bool
ends_with(const std::string &str, const std::string &suffix)
{
    return str.compare(
            str.length() - std::min(str.length(), suffix.length()),
            std::string::npos,
            suffix
        ) == 0;
}

}
