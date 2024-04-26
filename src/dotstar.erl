%%
%% Copyright (c) 2024 <winford@object.stream>
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(dotstar).

-include("ap102.hrl").

-export([init/3, write_dot/4, write_dot_hsv/3]).

-type color_bits() :: 0..255.
-type pin() :: non_neg_integer().
-type red() :: color_bits().
-type green() :: color_bits().
-type blue() :: color_bits().
% -type dot() :: non_neg_integer().
-type illumination() :: 0..100.

-spec init(Di_pin :: pin(), Ci_pin :: pin(), Dots :: non_neg_integer()) -> ok.
init(Di_pin, Ci_pin, Dots) ->
    SPIConfig = [
        {bus_config, [
            {mosi, Di_pin},
            {sclk, Ci_pin}
        ]},
        {device_config, [
            {?LED_DEVICE_NAME, [
                {cs, -1},
                {address_len_bits, 0},
                {command_len_bits, 0},
                {clock_speed_hz, ?SPI_CLOCK_HZ},
                {mode, 3}
            ]}
        ]}
    ],
    SPI = spi:open(SPIConfig),
    erlang:register(dotstar, SPI),
    case Dots of
        0 ->
            erlang:error(badarg);
        1 ->
            ok;
        _ ->
            %% TODO start gen_server to manage color state so individual stars can be set.
            erlang:error(unimplemented)
    end.

-spec write_dot(Red :: red(), Green :: green(), Blue :: blue(), Illumination :: illumination()) ->
    ok.
write_dot(Red, Green, Blue, Illumination) ->
    %% fast and dirty way to convert 0-100 Illumination into 5-bit Brightness.
    Bright = (Illumination * 31) div 97,
    {C1, C2, C3} = ?COLOR_ORDER,
    Dot_data =
        <<?SPI_START_FRAME:32, ?ILUM_START_BITS:3, Bright:5, C1:8, C2:8, C3:8, ?SPI_END_FRAME:32>>,
    Transaction = #{
        command => 0, address => 0, write_data => Dot_data, write_bits => 96, read_bits => 0
    },
    spi:write(erlang:whereis(dotstar), ?LED_DEVICE_NAME, Transaction).

% -spec set_dot(Dot :: dot(), Red :: red(), Green :: green(), Blue :: blue(),  Illumination :: illumination()) -> ok.
% set_dot(Dot, Red, Green, Blue, Illumination) ->
%     %% fast and dirty way to convert 0-100 Illumination into 5-bit Brightness.
%     Bright = (Illumination * 31) div 97,
%     Bright_8bits = ?LED_BRIGHTNESS_MSB bor Bright,
%     LED_32bits = Bright_8bits bsl 24 bor get_color_24bits(?COLOR_ORDER),
%     Data64 = LED_32bits bsl 32 bor ?SPI_END_FRAME,
%     Dot_96bits = ?SPI_START_FRAME bsl 64 bor Data64,
%     Dot_data = <<Dot_96bits:96>>,
%     Transaction = #{command => 0, address => 0, write_data => Dot_data, write_bits => 96, read_bits => 0},
%     spi:write(erlang:whereis(dotstar), ?LED_DEVICE_NAME, Transaction).

%% Based on atomvm_neopixel led_strip_hsv2rgb implementation, but less accurate and without floats.
write_dot_hsv(H, S, V) ->
    RGB_max = (V * 255) div 100,
    RGB_min = (RGB_max * (100 - S)) div 100,
    Diff = H rem 60,
    RGB_adj = ((RGB_max - RGB_min) * Diff) div 60,
    Sextant = H div 60,
    {R, G, B} =
        case Sextant of
            0 -> {RGB_max, RGB_min + RGB_adj, RGB_min};
            1 -> {RGB_max - RGB_adj, RGB_max, RGB_min};
            2 -> {RGB_min, RGB_max, RGB_min + RGB_adj};
            3 -> {RGB_min, RGB_max - RGB_adj, RGB_max};
            4 -> {RGB_min + RGB_adj, RGB_min, RGB_max};
            _ -> {RGB_max, RGB_min, RGB_max - RGB_adj}
        end,
    write_dot(R, G, B, V).
