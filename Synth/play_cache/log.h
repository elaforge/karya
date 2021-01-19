// Copyright 2018 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#pragma once


#define LOG(MSG) do { log << __FILE__ << ':' << __LINE__ << ' ' \
    << MSG << std::endl << std::flush; } while (0)
